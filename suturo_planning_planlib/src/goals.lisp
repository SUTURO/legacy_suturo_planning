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
                (retry))))))))         
         ;(perform take-home-pose-head)))))

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
    (with-retry-counters ((grasping-retry-counter 2))
      (with-failure-handling 
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (error-out (suturo planlib) "Failed to grasp object")
             (do-retry grasping-retry-counter
               (info-out (suturo planlib) "Trying again")
               (achieve '(home-pose))
               (retry))))
        (info-out (suturo planlib) "Grasping object ~a with ~a" 
                  (desig-prop-value ?obj 'name) ?arm)
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
  (info-out (suturo planlib) "Moving ~a over object ~a"
            ?arm (desig-prop-value ?obj 'name))
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
  (info-out (suturo planlib) "Opening hand")
  (with-retry-counters ((open-retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::drop-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to open hand")
           (do-retry open-retry-counter
             (info-out (suturo planib) "Trying again.")
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (obj ,?obj)))))
        (perform open-hand)))))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  (let ((arm (get-best-arm ?box)))
    (achieve `(object-in-hand ,?obj ,arm))
    (with-failure-handling
        ((suturo-planning-common::move-arm-failed (f)
           (declare (ignore f))
           (achieve `(empty-hand ,?obj))
           (with-designators ((placed-object-in-box 
                               (action `((to placed-object-in-box) 
                                         (obj ,?obj) 
                                         (container ,?box)))))
        (perform placed-object-in-box))))
           (with-named-policy 'dont-drop-object (arm)
             (achieve `(hand-over ,?box ,arm))))
    (achieve `(empty-hand ,?obj))
    (with-designators ((placed-object-in-box 
                          (action `((to placed-object-in-box) 
                                    (obj ,?obj) 
                                    (container ,?box)))))
      (perform placed-object-in-box))))

(def-goal (achieve (objects-in-appropriate-boxes ?objs ?boxes))
  "Edible Objects in the left box and inedible ones in the right box"
  (let ((box-for-food (get-box ?boxes 'storage-for-food)) 
        (box-for-stuff (get-box ?boxes 'storage-for-stuff))
        (obj nil))
    (with-retry-counters ((plan-retry-counter 6))
      (with-failure-handling
          ((suturo-planning-common::dropped-object (f)
             (declare (ignore f))
             (error-out (planlib) 
                        "Dropped object ~a. Wont bother to retrieve it"
                        (desig-prop-value obj 'name))
             (sleep 2)
             (do-retry plan-retry-counter
               (info-out (planlib) "Trying next object")
               (retry)))
           (suturo-planning-common::simple-plan-failure (f)
             ;(declare (ignore f))
             (error-out (planlib) "Failed to put ~a in the box. ~a~%"
                        (desig-prop-value obj 'name) f)
             (sleep 2)
             (do-retry plan-retry-counter
               (info-out (planlib) "Trying next object")
               (append `(,obj) ?objs)
               (retry))))
        (loop while ?objs
              do (setf obj (pop ?objs))
                 (if (desig-prop-value obj 'edible)
                     (achieve `(object-in-box ,obj ,box-for-food))
                     (achieve `(object-in-box ,obj ,box-for-stuff)))
                 (achieve '(home-pose)))))))

(def-goal (achieve (objects-and-boxes-perceived ?nr-objs ?nr-boxes))
  "Tries to perceive the given number of objectes and boxes"
  (let ((objs nil)
        (boxes nil)
        (leftest-obj nil)
        (rightest-obj nil)
        (things nil)
        (counter 0))
    (loop while (or (< (length objs) ?nr-objs) (< (length boxes) ?nr-boxes))
          do (if (eql counter 8)
                 (cpl:fail 'suturo-planning-common::not-enough-objects-found))
             (incf counter)
             (setf things (concatenate 'list objs boxes))
             (when (> (length things) 0)
               (let ((new-leftest-obj (get-object-on-side 'left things))
                     (new-rightest-obj (get-object-on-side 'right things)))
                 ;; Check for objects on the left
                 (if (not (desig-equal leftest-obj new-leftest-obj))
                     (prog1
                         (achieve `(face-loc 
                                    ,(desig-prop-value new-leftest-obj 
                                                       'at)))
                         (setf leftest-obj new-leftest-obj))
                     ;; Check for objects on the right
                     (if (not (desig-equal rightest-obj new-rightest-obj))
                         (prog1
                             (achieve `(face-loc 
                                        ,(desig-prop-value new-rightest-obj 
                                                           'at)))
                             (setf rightest-obj new-rightest-obj))
                         (cpl:fail 'suturo-planning-common::not-enough-objects-found)))))              
             (with-designators ((update-map 
                                 (action 
                                  '((to update-semantic-map))))
                                (get-containers 
                                 (action 
                                  '((to get-container-objects))))
                                (get-objects
                                 (action 
                                  '((to get-graspable-objects)))))
               (perform update-map)
               (setf objs (perform get-objects))
               (setf boxes (perform get-containers))
               (ros-info (suturo planlib) "Perceived ~a ~a" objs boxes)))
    ;(info-out (suturo planlib) "Perceived ~a ~a" 
              ;(generate-output objs) (generate-output boxes))
    `(,objs ,boxes)))

(def-goal (achieve (face-loc ?loc))
  "Let the Head point to the given Location"
  (with-retry-counters ((head-retry-counter 3))
    (with-failure-handling
        ((simple-plan-failure (f)
           (declare (ignore f))
           (do-retry head-retry-counter
             (retry))))
      (with-designators ((move-head (action `((to move-head)
                                              (loc ,?loc)))))))))
        ;(perform move-head)))))

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
                              (first (get-coords obj))
                              (first (get-coords obj-on-side)))
                     (setf obj-on-side obj))
                 (setf obj-on-side obj)))
    obj-on-side))

(defun get-box (boxes use)
  "Returns a box with the given use"
  (let ((box nil))
    (loop while boxes
          do (setf box (pop boxes))
             (if (eql (desig-prop-value box 'use) use)
                 (return box)))))

(defun get-holding-hand (obj)
  "Returns the arm which holds the object"
  (if (not obj) (cpl:fail 'simple-plan-failure))
  (format t "holds obj ~a~%" obj)
  (let ((pos (desig-prop-value 
              (desig-prop-value (current-desig obj) 'at) 
              'in)))
    (if (not pos)
        (progn
          (format t "~a~%" obj)
          (cpl:fail 'simple-plan-failure))
        (if (eql pos 'left-gripper) 
            'left-arm
            (if (eql pos 'right-gripper)
                'right-arm)))))

(defun get-best-arm (obj)
  "Returns the arm closest to the object"
  (with-designators ((get-arm (action `((to get-best-arm)
                                        (obj ,obj)))))
    (perform get-arm)))

(defun get-coords (obj)
  "Returns the coordinates of the object"
  (if obj
      (desig-prop-value (desig-prop-value (current-desig obj) 'at) 'coords)
      nil))
