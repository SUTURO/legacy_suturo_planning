(in-package :suturo-planning-planlib)

(define-policy dont-drop-object (arm)
  "Policy to monitor the gripper of the given arm that it wont completly close"
  (:init (perform (make-designator 'action '((to start-monitoring-gripper)))))
  (:check (sleep* 0.5)
          (perform (make-designator 'action `((to gripper-is-closed)
                                              (arm ,arm)))))
  (:recover (perform (make-designator 'action '((to end-monitoring-gripper))))
            (cpl:fail 'suturo-planning-common::dropped-object))
  (:clean-up (perform (make-designator 'action '((to end-monitoring-gripper))))))
  
(def-goal (achieve (robot-at ?loc))
  "Moves the robot to a position described by ?loc"
  (let ((loc-robot (locate ?loc)))
    (perform (make-designator 'action `((to move) (loc ,loc-robot))))))
                     
(def-goal (achieve (home-pose))
  "Move the robot in the initial position"
  (info-out (suturo planlib) "Taking home pose")
  (with-retry-counters ((arm-retry-counter 2)
                        (head-retry-counter 2))
     (with-designators ((take-home-pose-arms (action 
                                               '((to take-pose)
                                                 (pose initial)
                                                 (body-part both-arms))))
                         (take-home-pose-head (action 
                                               '((to take-pose)
                                                 (pose initial)
                                                 (body-part head)))))
       (with-failure-handling 
           ((suturo-planning-common::pose-not-reached (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to bring arms int the initial pose")
              (do-retry arm-retry-counter
                (info-out (suturo planlib) "Trying again")
                (retry))))
         (perform take-home-pose-arms))
       (with-failure-handling 
           ((suturo-planning-common::pose-not-reached (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to bring head in the initial pose")
              (do-retry head-retry-counter
                (info-out (suturo planlib) "Trying again")
                (retry))))        
         (perform take-home-pose-head)))))

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
    (with-retry-counters ((grasping-retry-counter 1))
      (with-failure-handling 
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (error-out (suturo planlib) "STOP! Hammer, time! Can't, touch, this. Failed, to ,grasp ,object")
             (sleep 4)
             (achieve '(home-pose))
             (do-retry grasping-retry-counter
               (info-out (suturo planlib) "Trying again")
               (retry))))
        (info-out (suturo planlib) "Grasping, object, ~a, with, ~a" 
                  (object-output ?obj) ?arm)
        (with-designators ((grasp-obj (action `((to grasp)
                                                (obj ,?obj)
                                                (arm ,?arm))))
                           (monitor-gripper (action `((to monitor-gripper)
                                                      (arm ,?arm)))))
          (perform grasp-obj)
          (if (perform monitor-gripper) 
            (cpl:fail 'suturo-planning-common::grasping-failed))))))
      
(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (info-out (suturo planlib) "Moving, ~a, over, object, ~a"
            ?arm (object-output ?obj))
  (with-retry-counters ((move-retry-counter 1))
    (with-failure-handling
        ((suturo-planning-common::move-arm-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to move arm")
           (do-retry move-retry-counter
             (info-out (suturo planlib) "Trying again.")
             (retry))))
      (with-designators ((get-loc-over-obj (action `((to get-location-over)
                                                     (loc ,(desig-prop-value ?obj 'at))))))
        (with-designators ((move-hand (action `((to move-arm)
                                                (arm ,?arm)
                                                (loc ,(perform get-loc-over-obj))))))
          (perform move-hand))))))

(def-goal (achieve (empty-hand ?obj))
  "Opens the hand of the given arm"
  (info-out (suturo planlib) "Opening, hand")
  (sleep 1.5)
  (with-retry-counters ((open-retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::drop-failed (f)
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
        ((suturo-planning-common::move-arm-failed (f)
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

(defun get-object-on-side (side objs)
  "Returns the object furthest on the given side"
  (let ((compare-fun nil)
        (obj-on-side (first objs)))
    (if (eql side 'left)
        (setf compare-fun #'<)
        (setf compare-fun #'>))
    (loop for obj in objs
          do (if obj-on-side
                 (if (funcall compare-fun
                              (second (get-coords obj))
                              (second (get-coords obj-on-side)))
                     (setf obj-on-side obj))
                 (setf obj-on-side obj)))
    obj-on-side))

(defun get-holding-arm (obj)
  "Returns the arm which holds the object"
  (when obj
    (let ((pos (desig-prop-value 
                (desig-prop-value (current-desig obj) 'at) 
                'in)))
      (if pos
          (if (eql pos 'left-gripper) 
              'left-arm
              (if (eql pos 'right-gripper)
                  'right-arm))))))

(defun get-best-arm (obj)
  "Returns the arm closest to the object"
  (with-designators ((get-arm (action `((to get-best-arm)
                                        (obj ,obj)))))
    (perform get-arm)))

