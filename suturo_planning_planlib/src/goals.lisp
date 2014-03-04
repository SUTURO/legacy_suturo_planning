(in-package :suturo-planning-planlib)

(define-policy dont-drop-object (arm)
  "Monitors the griper of the given arm and checks if it closes
   completly"
  (:init (perform (make-designator 'action 
                                   '((to start-monitoring-gripper)))))
  (:check (sleep* 0.5)
          (perform (make-designator 'action 
                                    `((to gripper-is-closed)
                                      (arm ,arm)))))
  (:recover (perform (make-designator 'action 
                                      '((to end-monitoring-gripper))))
            (cpl:fail 'dropped-object))
  (:clean-up (perform (make-designator 'action 
                                       '((to end-monitoring-gripper))))))
  
(def-goal (achieve (robot-at ?loc))
  "Moves the robot to a position described by ?loc"
  (with-failure-handling 
      ((move-base-failed (f)
         (declare (ignore f))
         (retry-with-next-solution ?loc)))
    (perform (make-designator 'action `((to move) 
                                        (pose ,(reference ?loc)))))))
                     
(def-goal (achieve (home-pose))
  "Moves the robot in the initial position"
  (info-out (suturo planlib) "Taking home pose")
  (achieve '(home-pose both-arms))
  (achieve '(home-pose head)))

(def-goal (achieve (home-pose ?body-part))
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
  

(def-goal (achieve (in-gripper ?obj))
  "Grasps the object with one gripper"
  (when (not (eql-or (desig-prop-value (desig-prop-value ?obj 'at) 'in)
                     'left-gripper
                     'right-gripper))
    (let ((arm (get-best-arm ?obj))
          (loc-to-reach (make-designator 'location 
                                         `((to reach) (obj ,?obj)))))
      (with-retry-counters ((grasping-retry-counter 1))
        (with-failure-handling 
            ((grasping-failed (f)
               (declare (ignore f))
               (error-out (suturo planlib) 
                          "Grasping failed retry with other arm")
               (achieve `(home-pose ,arm))
               (do-retry grasping-retry-counter
                 (setf arm (switch-arms arm))
                 (info-out (suturo planlib) "Trying again")
                 (retry))
               ;; get next position
               ;; (setf grasping-retry-counter 1)
               ))
          (achieve `(robot-at ,loc-to-reach))
          (achieve `(object-in-hand ,?obj ,arm)))))))
  

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
  (info-out (suturo planlib) "Grasping object ~a with ~a" 
            (object-output ?obj) ?arm)
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
  (info-out (suturo planlib) "Moving, ~a, over, object, ~a"
            ?arm (object-output ?obj))
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
        (with-designators 
            ((move-hand (action `((to move-arm)
                                  (arm ,?arm)
                                  (loc ,(perform get-loc-over-obj))))))
          (perform move-hand))))))

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
        (perform open-hand))))
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



