(in-package :suturo-planning-planlib)

 ;;(define-policy dont-drop-object (arm))
#|
  "Monitors the griper of the given arm and checks if it closes
   completely"
  (:init ;;(perform (make-designator 'action 
         ;;                          '((to start-monitoring-gripper)))))
   (sp-gripper-monitor::call-action 'start-monitoring-gripper))
  (:check (sleep* 0.5)
          ;;(perform (make-designator 'action 
          ;;                          `((to gripper-is-closed)
          ;;                            (arm ,arm)))))
          (sp-gripper-monitor::call-action 'gripper-is-closed arm))
  (:recover ;;(perform (make-designator 'action 
            ;;                          '((to end-monitoring-gripper))))
            (sp-gripper-monitor::call-action 'end-monitoring-gripper)
            (cpl:fail 'dropped-object))
  (:clean-up ;;(perform (make-designator 'action 
             ;;                          '((to end-monitoring-gripper))))))
   (sp-gripper-monitor::call-action 'end-monitoring-gripper))) |# 
  
(def-goal (achieve (robot-at ?loc))
  "Moves the robot to a position described by ?loc"
  (with-failure-handling 
      ((move-base-failed (f)
         (declare (ignore f))
         (retry-with-next-solution ?loc)))
    (achieve '(drive-pose))
    (let ((pose-stamped (cond
                          ((typep ?loc 'location-designator) (reference ?loc))
                          ((typep ?loc 'cl-tf:pose-stamped) ?loc)
                          (t (cpl:error 'suturo-planning-common::unhandled-value)))))
      (publish-visualization-marker pose-stamped :min-z 1.2 :scale-x 0.24
                                    :scale-y 0.1 :scale-z 0.1)
      (perform (make-designator 'action `((to move-base) 
                                          (pose ,pose-stamped)))))))
                     
(def-goal (achieve (home-pose))
  "Moves the robot in the initial position"
  (info-out (suturo planlib) "Taking home pose")
  (achieve '(home-pose both-arms))
  (achieve '(home-pose head)))

(def-goal (achieve (drive-pose))
  "Brings the arms in a pose that brings the arms closer to the body"
  (achieve '(home-pose both-arms-move)))

(def-goal (achieve (home-pose ?body-part))
  "Brings the specified robotpart in the home pose"
  (with-retry-counters ((retry-counter 2))
     (with-designators ((take-home-pose (action 
                                          `((to take-pose)
                                            (pose initial)
                                            (body-part ,?body-part)))))
       (with-failure-handling 
           ((pose-not-reached (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to bring ~a into the initial pose"
                         ?body-part)
              (do-retry retry-counter
                (ros-info (suturo planlib) "Trying again")
                (retry))))
         (perform take-home-pose)))))

(def-goal (achieve (arm-at ?arm ?loc))
  "Brings the specified arm `?arm' to the location `?loc'"
  (with-retry-counters ((retry-counter 1))
     (with-designators ((move (action 
                               `((to move-arm)
                                 (loc ,?loc)
                                 (arm ,?arm)))))
       (with-failure-handling 
           ((move-arm-failed (f)
              (declare (ignore f))
              (ros-warn (suturo planlib) 
                         "Failed to move ~a"
                         ?arm)
              (do-retry retry-counter
                (ros-info (suturo planlib) "Trying again")
                (retry))))
         (publish-visualization-marker2 (cond
                                          ((typep ?loc 'location-designator) ?loc)
                                          ((typep ?loc 'cl-tf:pose-stamped) (pose-stamped->designator ?loc))))
         (perform move)))))

(def-goal (achieve (in-gripper ?obj))
  "Grasps the object with one gripper"
  (when (not (eql-or (desig-prop-value (desig-prop-value ?obj 'at) 'in)
                     'left-gripper
                     'right-gripper))
    (let ((arm 'left-arm)
          (loc-to-reach (make-designator 'location 
                                         `((to reach) (obj ,?obj))))
          (retry-counter 0))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (incf retry-counter)
             (if (< retry-counter 8)
                 (cond
                   ((eql (mod retry-counter 2) 1)
                    (error-out (suturo planlib) 
                               "Grasping failed retry with other arm")
                    (setf arm (switch-arms arm))
                    (retry))
                   ((eql (mod retry-counter 2) 0)
                    (ros-info (suturo planlib) "Trying different Position to reach the object.")
                    (retry-with-next-solution loc-to-reach))))
             (achieve `(home-pose ,arm))))
        (achieve `(robot-at ,loc-to-reach))
        (achieve `(object-in-hand ,?obj ,arm sp-manipulation::grasp-action-grasp nil))))))
  
(def-goal (achieve (object-in-hand ?obj ?arm ?grasp-action ?tolerance))
  "Takes the object in one hand"
  (achieve '(home-pose both-arms))
  (info-out (suturo planlib) "Grasping object ~a with ~a" ?obj ?arm)
  (with-designators ((grasp-obj (action `((to grasp)
                                          (obj ,?obj)
                                          (arm ,?arm)
                                          (grasp-action ,?grasp-action)
                                          (tolerance ,?tolerance))))
                     (monitor-gripper (action `((to monitor-gripper)
                                                (arm ,?arm)))))
    (perform grasp-obj)
    (if (perform monitor-gripper) 
        (cpl:fail 'grasping-failed))))  

(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (info-out (suturo planlib) "Moving ~a" ?arm)
  (let ((loc (desig-prop-value ?obj 'at)))
    (with-retry-counters ((move-arm-retry-counter 9))
      (with-failure-handling
          ((move-arm-failed (f)
             (declare (ignore f))
             (ros-warn (suturo planlib) "Failed to move arm")
             (do-retry move-arm-retry-counter
               (when (next-solution loc)
                 (ros-info (suturo planlib) "Trying again")
                 (retry)))))
        (let* ((loc-pose-stamp (reference loc))
               (orientation (cl-tf:orientation loc-pose-stamp))
               (loc-pose `(,(cl-tf:x orientation) ,(cl-tf:y orientation) 
                                                  ,(cl-tf:z orientation)
                                                  ,(cl-tf:w orientation)))
               (loc-origin (cl-tf:origin loc-pose-stamp))
               ;; Assumes z is the height
               (coords-over `(,(cl-tf:x loc-origin) ,(cl-tf:y loc-origin) 
                              ,(+ (cl-tf:z loc-origin) 0.3)))
               (loc-over (make-designator 'location 
                                          (update-designator-properties 
                                           `((coords ,coords-over)
                                             (frame ,(cl-tf:frame-id loc-pose-stamp))
                                             (pose ,loc-pose))
                                           (description loc)))))
          (achieve `(arm-at ,?arm ,loc-over)))))))

(def-goal (achieve (empty-hand ?obj ?target-on))
  "Opens the hand of the given arm and changes its location to ?target-on."
  (info-out (suturo planlib) "Opening, hand")
  (sleep 1.5)
  (with-retry-counters ((open-retry-counter 1))
    (with-failure-handling
        ((drop-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to open hand")
           (sleep 1.5)
           (do-retry open-retry-counter
             (ros-info (suturo planib) "Trying again.")
             (sleep 1.5)
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (obj ,?obj)
                                              (target-on ,?target-on)))))
        (perform open-hand)))))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  (let ((arm (get-best-arm ?box)))
    (achieve `(object-in-hand ,?obj ,arm))
    (with-failure-handling
        ((move-arm-failed (f)
           (declare (ignore f))
           (achieve `(empty-hand ,?obj))
           (achieve '(home-pose))))
      ;;TODO fix policy
      ;;(with-named-policy 'dont-drop-object (arm)
        (achieve `(hand-over ,?box ,arm)))
    (achieve `(empty-hand ,?obj))
    (with-designators ((placed-object-in-box 
                          (action `((to placed-object-in-box) 
                                    (obj ,?obj) 
                                    (container ,?box)))))
      (perform placed-object-in-box))))

(def-goal (achieve (face-loc ?loc))
  "Let the Head point to the given Location"
  (with-retry-counters ((head-retry-counter 3))
    (with-failure-handling
        ((simple-plan-failure (f)
           (declare (ignore f))
           (do-retry head-retry-counter
             (retry))))
      (with-designators ((move-head (action `((to move-head)
                                              (direction ,?loc)))))
        (perform move-head)))))

(defun seperate-known-from-unknown-objects (objs)
  "Gets a list of objects and returns two values. The first is the
   list of known objects and the second the list of unknown objects."
  (let ((known-objs nil)
        (unknown-objs nil))
    (loop for obj in objs
          do (if (desig-prop-value obj 'unknown)
                 (push obj unknown-objs)
                 (push obj known-objs)))
    (values known-objs unknown-objs)))