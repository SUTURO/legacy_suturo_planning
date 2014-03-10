(in-package :suturo-planning-planlib)

(define-policy dont-drop-object (arm)
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
   (sp-gripper-monitor::call-action 'end-monitoring-gripper)))
  
(def-goal (achieve (robot-at ?loc))
  "Moves the robot to a position described by ?loc"
  (format t "move base~%")
  (with-failure-handling 
      ((move-base-failed (f)
         (declare (ignore f))
         (retry-with-next-solution ?loc)))
    ;;(perform (make-designator 'action `((to move) 
    ;;                                    (pose ,(reference ?loc)))))))
    (sp-manipulation::call-action 'move-base (reference ?loc))))
                     
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
         ;;(perform take-home-pose)))))
         (sp-manipulation::call-action 'take-pose 'initial ?body-part)))))

(def-goal (achieve (arm-at ?arm ?loc))
  "Brings the specified arm `?arm' to the location `?loc'"
  (format t "Achieving ~a at ~a~%" ?arm ?loc)
  (with-retry-counters ((retry-counter 2))
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
         ;;(perform move)))))
         (sp-manipulation::call-action 'move-arm ?loc ?arm)))))
  

(def-goal (achieve (in-gripper ?obj))
  "Grasps the object with one gripper"
  (when (not (eql-or (desig-prop-value (desig-prop-value ?obj 'at) 'in)
                     'left-gripper
                     'right-gripper))
    (let ((arm 'left-arm)
          (loc-to-reach (make-designator 'location 
                                         `((to reach) (obj ,?obj))))
          (try-other-arm t))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (error-out (suturo planlib) 
                        "Grasping failed retry with other arm")
             (achieve `(home-pose ,arm))
             (when try-other-arm
               (setf arm (switch-arms arm))
               (info-out (suturo planlib) "Trying again")
               (retry))
             (if try-other-arm
                 (setf try-other-arm nil)
                 (setf try-other-arm t))
             (retry-with-next-solution loc-to-reach)))
          (achieve `(robot-at ,loc-to-reach))
          (achieve `(object-in-hand ,?obj ,arm))))))
  

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
  (info-out (suturo planlib) "Grasping object with ~a" ?arm)
  (with-designators ((grasp-obj (action `((to grasp)
                                          (obj ,?obj)
                                          (arm ,?arm))))
                     (monitor-gripper (action `((to monitor-gripper)
                                                (arm ,?arm)))))
    ;;(perform grasp-obj)
    ;;(if (perform monitor-gripper) 
    ;;    (cpl:fail 'grasping-failed))))
    (sp-manipulation::call-action 'grasp ?obj ?arm)))

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
      (with-designators 
          ((get-loc-over-obj (action `((to get-location-over)
                                       (loc ,(desig-prop-value ?obj 'at))))))
        (achieve `(arm-at ,(sp-pm-utils::call-action 'get-location-over 
                                                     (desig-prop-value ?obj 'at))
                          ?arm))))))

(def-goal (achieve (empty-hand ?obj))
  "Opens the hand of the given arm"
  (info-out (suturo planlib) "Opening, hand")
  (sleep 1.5)
  (with-retry-counters ((open-retry-counter 2))
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
                                              (obj ,?obj)))))
        ;;(perform open-hand))))
        (sp-manipulation::call-action 'open-hand ?obj))))
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
      (with-named-policy 'dont-drop-object (arm)
        (achieve `(hand-over ,?box ,arm))))
    (achieve `(empty-hand ,?obj))
    (with-designators ((placed-object-in-box 
                          (action `((to placed-object-in-box) 
                                    (obj ,?obj) 
                                    (container ,?box)))))
      ;;(perform placed-object-in-box))))
      (sp-knowledge::call-action 'placed-object-in-box ?obj ?box))))

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
        ;;(perform move-head)))))
        (sp-manipulation::call-action 'move-head ?loc)))))



