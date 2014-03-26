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
  (format t "move base~%")
  (with-failure-handling 
      ((move-base-failed (f)
         (declare (ignore f))
         (retry-with-next-solution ?loc)))
    (format t "Performing move")
    (sp-manipulation::call-action 'move-base (reference ?loc))))
    ;(perform (make-designator 'action `((to move-base) 
    ;                                    (pose ,(reference ?loc)))))))
 
                     
(def-goal (achieve (home-pose))
  "Moves the robot in the initial position"
  (info-out (suturo planlib) "Taking home pose")
  (achieve '(home-pose both-arms))
  (achieve '(home-pose head)))

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
                (info-out (suturo planlib) "Trying again")
                (retry))))
         (perform take-home-pose)))))

(def-goal (achieve (arm-at ?arm ?loc))
  "Brings the specified arm `?arm' to the location `?loc'"
  (format t "Achieving ~a at ~a~%" ?arm ?loc)
  (with-retry-counters ((retry-counter 1))
     (with-designators ((move (action 
                               `((to move-arm)
                                 (loc ,?loc)
                                 (arm ,?arm)))))
       (with-failure-handling 
           ((move-arm-failed (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to move ~a to location ~a~%"
                         ?arm
                         ?loc)
              (do-retry retry-counter
                (info-out (suturo planlib) "Trying again")
                (retry))))
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
                   ((< (mod retry-counter 3) 2)
                    (error-out (suturo planlib) 
                               "Grasping failed retry with other arm")
                    (achieve `(home-pose ,arm))
                    (setf arm (switch-arms arm))
                    (retry))
                   ((eql (mod retry-counter 3) 2)
                    (format t "Move around~%")
                    (retry-with-next-solution loc-to-reach))))
                 (achieve `(home-pose ,arm))))
        (format t "asd ~a ~a" (reference loc-to-reach) arm)
        ;(achieve `(robot-at ,loc-to-reach))
        (achieve `(object-in-hand ,?obj ,arm))))))
  

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
  (info-out (suturo planlib) "Grasping object with ~a" ?arm)
  (with-designators ((grasp-obj (action `((to grasp)
                                          (obj ,?obj)
                                          (arm ,?arm))))
                     (monitor-gripper (action `((to monitor-gripper)
                                                (arm ,?arm)))))
    (perform grasp-obj)
    (if (perform monitor-gripper) 
        (cpl:fail 'grasping-failed))))
  

(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (info-out (suturo planlib) "Moving ~a" ?arm)
  (with-retry-counters ((move-retry-counter 1))
    (with-failure-handling
        ((move-arm-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to move arm")
           (do-retry move-retry-counter
             (info-out (suturo planlib) "Trying again.")
             (retry))))
      (let* ((loc (desig-prop-value ?obj 'at))
             (loc-pose-stamp (reference loc))
             (test (format t "~a~%" loc-pose-stamp))
             (loc-origin (cl-tf:origin loc-pose-stamp))
             (coords-over `(,(cl-tf:x loc-origin) ,(cl-tf:y loc-origin) 
                                                  ,(+ (cl-tf:z loc-origin) 0.4)))
             (loc-over (make-designator 'location (update-designator-properties `((coords ,coords-over)
                                                                                  (frame "/map")
                                                                                  (pose (1 0 0 0)))
                                                                                (description loc)))))
        (achieve `(arm-at ,?arm ,loc-over))))))

(def-goal (achieve (empty-hand ?obj ?target-on))
  "Opens the hand of the given arm"
  (info-out (suturo planlib) "Opening, hand")
  (sleep 1.5)
  (with-retry-counters ((open-retry-counter 1))
    (with-failure-handling
        ((drop-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to open hand")
           (sleep 1.5)
           (do-retry open-retry-counter
             (info-out (suturo planib) "Trying again.")
             (sleep 1.5)
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (obj ,?obj)
                                              (target-on ,?target-on)))))
        ;(perform open-hand))))
        (sp-manipulation::call-open-hand-action ?obj ?target-on))))
  (info-out (suturo planlib) "Dropped, object"))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  (let ((arm (get-best-arm ?box)))
    (achieve `(object-in-hand ,?obj ,arm))
    (with-failure-handling
        ((move-arm-failed (f)
           (declare (ignore f))
           (achieve `(empty-hand ,?obj))
           (achieve '(home-pose))))
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
                                              (loc ,?loc)))))
        (perform move-head)))))
