(in-package :suturo-planning-planlib)

(define-policy dont-drop-object (arm)
  (:check ;(with-designators ((keep-obj (action `((to keep-object-in-hand)
           ;                                     (arm ,arm)))))
            ;(perform keep-obj)))
           (format t "CHECK~%")
           (sleep 5))
  (:recover (format t "Recovering ~%"))
  (:clean-up (format t "Cleaning up ~%")))
                     
(def-goal (achieve (home-pose))
  (with-retry-counters ((pose-retry-counter 2))
    (with-failure-handling 
        ((suturo-planning-common::pose-not-reached (f)
           (declare (ignore f))
           (do-retry pose-retry-counter
             (retry))))
      (with-designators ((take-home-pose (action 
                                          '((to take-pose)
                                            (pose home)
                                            (body-part all)))))
        (perform take-home-pose))))
  (format t "Initial pose reached~%"))

(def-goal (achieve (object-in-hand ?obj))
  "Takes the object in one hand"
  (let ((arm (get-best-arm ?obj)))
    (with-retry-counters ((grasping-retry-counter 4))
      (with-failure-handling 
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (do-retry grasping-retry-counter
               (setf arm (switch-arms arm))
               (retry))))
        (with-designators ((grasp-obj (action `((to grasp)
                                                (obj ,?obj)
                                                (arm ,arm))))
                           (gripper-is-closed (action `((to gripper-is-closed)
                                                        (arm ,arm)))))
          (perform grasp-obj)
          (if (perform gripper-is-closed) 
            (cpl:fail 'suturo-planning-common::grasping-failed)))))))
      
(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (with-retry-counters ((move-retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::location-not-reached (f)
           (declare (ignore f))
           (error-out (planlib) "Failed to move arm")
           (do-retry move-retry-counter
             (retry))))
      (let ((coords (get-coords ?obj)))
        (setf coords `(,(nth 0 coords)
                       ,(nth 1 coords)
                       ,(+ (nth 2 coords) 10)))
        (with-designators ((loc-over-obj (location `((coords ,coords)))))
          (with-designators ((move-hand (action `((to move-arm)
                                                  (arm ,?arm)
                                                  (loc ,loc-over-obj)))))
            (perform move-hand))))
      (format t "Hand over ~a~%" ?obj))))

(def-goal (achieve (empty-hand ?arm))
  "Opens the hand of the given arm"
  (info-out (planlib) "Open hand")
  (with-retry-counters ((open-retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::drop-failed (f)
           (declare (ignore f))
           (error-out (planlib) "Failed to open hand")
           (do-retry open-retry-counter
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (arm ,?arm)))))
        (perform open-hand))
      (format t "Hand empty~%"))))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  (with-failure-handling 
      ((suturo-planning-common::grasping-failed (f)
         (declare (ignore f))
         (error-out (planlib) "Graping failed"))) ;add retry
    (achieve `(object-in-hand ,?obj)))
  (let ((arm (get-holding-hand (current-desig ?obj))))
    (with-named-policy 'dont-drop-object (arm)
      (achieve `(hand-over ,?box ,arm)))
    (format t "Hand over finished~%")
    (achieve `(empty-hand ,arm))))

(def-goal (achieve (objects-in-appropriate-boxes ?objs ?boxes))
  "Edible Objects in the left box and inedible ones in the right box"
  (let ((left-box (get-box ?boxes 'left)) 
        (right-box (get-box ?boxes 'right))
        (obj nil)
        (box nil))
    (with-retry-counters ((plan-retry-counter 6))
      (with-failure-handling
          ((simple-plan-failure (f)
             (declare (ignore f))
             (error-out (planlib) "Simple-failure")
             (do-retry plan-retry-counter
               (info-out (planlib) "Trying again")
               (append obj ?objs)
               (retry))))
        (loop while ?objs
              do (setf obj (pop ?objs))
                 (if (desig-prop-value obj 'edible)
                     (setf box left-box)
                     (setf box right-box))
                 (achieve `(object-in-box ,obj ,box)))))
    (format t "THE END")))

(def-goal (achieve (objects-and-boxes-perceived ?nr-objs ?nr-boxes))
  (let ((objs nil)
        (boxes nil)
        (leftest-obj nil)
        (rightest-obj nil)
        (things nil))
    (loop while (and (< (length objs) ?nr-objs) (< (length boxes) ?nr-boxes))
          do (setf things (concatenate 'list objs boxes))
             (when (> (length things) 0)
               (format t "ich war hier~%")
               (let ((new-leftest-obj (get-object-on-side 'left things))
                     (new-rightest-obj (get-object-on-side 'right things)))
                 (format t "ich auch~%")
                 (if (desig-equal leftest-obj new-leftest-obj)
                     (if (desig-equal rightest-obj new-rightest-obj)
                         (format t "Guck gerade aus")
                         (prog2
                             (achieve `(face-loc (get-unseen-location 'right
 new-rightest-obj)))
                             (setf rightest-obj new-rightest-obj)))
                     (prog2
                         (achieve `(face-loc (get-unseen-location 'left new-leftest-obj)))
                         (setf leftest-obj new-leftest-obj)))))              
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
                 (setf boxes (perform get-containers))))
    (format t "Objects perceived~%")
    `(,objs ,boxes)))

(def-goal (achieve (face-loc ?loc))
  (with-retry-counters ((head-retry-counter 3))
    (with-failure-handling
        ((simple-plan-failure (f)
           (declare (ignore f))
           (do-retry head-retry-counter
             (retry))))
      (with-designators ((move-head (action `((to move-head)
                                              (loc ,?loc)))))
        (perform move-head)))))

(defun get-unseen-location (side obj)
  (let ((loc (desig-prop-value obj 'at))
        (new-coords (get-coords obj))
        (offset 5))
    (if (eql side 'left)
        (setf offset (- offset)))
    (setf (first new-coords) (+ (first new-coords) offset))
    (make-designator 'location
                     (update-designator-properties `((coords ,new-coords))
                                                   (description loc)))))

(defun get-object-on-side (side objs)
  (let ((compare-fun nil)
        (obj-on-side nil))
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

(defun get-box (boxes side)
  "Returns the box that is on the given side of the other box"
  (format t "Get ~a box~%" side)
  (let ((x1 (first (get-coords (first boxes))))
        (x2 (first (get-coords (second boxes)))))
    (if (eql side 'left)
        (if (< x1 x2)
            (first boxes)
            (second boxes))
        (if (< x1 x2)
            (second boxes)
            (first boxes)))))

(defun get-holding-hand (obj)
  "Returns the arm which holds the object"
  (let ((pos (desig-prop-value 
              (desig-prop-value (current-desig obj) 'at) 
              'in)))
    (format t "Holding object ~a~%Pos: ~a~%" (current-desig obj) pos)
    (if (not pos)
        (cpl:fail 'simple-plan-failure)
        (progn
          (if (eql pos 'left-gripper) 
              'left-arm
              (if (eql pos 'right-gripper)
                  'right-arm))))))

(defun get-best-arm (obj)
  "Returns the arm closest to the object")
  (let ((coords (get-coords obj)))
    (if (< (first coords) 0)
        'left-arm
        'right-arm)))

(defun get-coords (obj)
  "Returns the coordinates of the object"
  (desig-prop-value (desig-prop-value (current-desig obj) 'at) 'coords))

(defun switch-arms (arm)
  (if (eql 'left-arm arm)
      'left-arm
      'right-arm))
