(in-package :suturo-planning-planlib)

(define-policy dont-drop-object (arm)
  (:check (monitor-grip arm)
          (whenever ((pulsed *gripper-closed*))
            (cpl:fail 'suturo-planning-common::grasp-fail))))
                     
(def-goal (achieve (home-pose))
  (with-designators ((take-home-pose (action 
                                      '((to take-pose)
                                        (pose home)))))
    (perform take-home-pose)
    (format t "Initial pose reached")))

(def-goal (achieve (object-in-hand ?obj))
  "Takes the object in one hand"
  (let ((arm (get-best-arm ?obj)))
    (with-retry-counters ((grasping-retry-counter 4))
      (with-failure-handling 
          ((suturo-planning-common::grasp-fail (f)
             (declare (ignore f))
             (incf fails)
             (do-retry grasping-retry-counter
               (setf arm (switch-arms arm))
               (retry))))
        (with-designators ((grasp-obj (action `((to grasp)
                                                (obj ,?obj)
                                                (arm ,arm)))))
          (perform grasp-obj))
        (if (gripper-is-closed) 
            (cpl:fail 'suturo-planning-common::grasp-fail))))))
      
(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (let ((coords (get-coords ?obj)))
    (setf coords `(,(nth 0 coords)
                   ,(nth 1 coords)
                   ,(+ (nth 2 coords) 10)))
    (with-designators ((loc-over-obj (location `((coords ,coords)))))
      (with-designators ((move-hand (action `((to move-arm)
                                              (arm ,?arm)
                                              (loc ,loc-over-obj)))))
        (perform move-hand))))
  (format t "Hand over ~a" ?obj))

(def-goal (achieve (empty-hand ?arm))
  "Opens the hand of the given arm"
  (with-designators ((open-hand (action `((to open-hand)
                                          (arm ,?arm)))))
    (perform open-hand))
  (format t "Hand empty"))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  ;add failure handling
  (achieve `(object-in-hand ,?obj))
  (let ((arm (get-holding-hand (current-desig ?obj))))
    (with-policy 'dont-drop-object (arm)
      (achieve `(hand-over ,?box ,arm)))
    (achieve `(empty-hand ,arm)))
  (format t "~a in box ~a" ?obj ?box))

(def-goal (achieve (objects-in-appropriate-boxes ?objs ?boxes))
  "Edible Objects in the left box and inedible ones in the right box"
  (let ((left-box (get-box ?boxes 'left)) 
        (right-box (get-box ?boxes 'right))
        (fails 0)
        (obj nil)
        (box nil))
    (with-failure-handling
        ((simple-plan-failure (f)
           (declare (ignore f))
           (if (< fails 6)
               (seq 
                 (incf fails)
                 (append obj ?objs)
                 (retry)))))
      (loop while ?objs
            do (setf obj (pop ?objs))
               (if (desig-prop-value obj 'edible)
                   (setf box left-box)
                   (setf box right-box))
               (achieve `(object-in-box ,obj ,box)))))
  (format t "~a in boxes ~a" ?objs ?boxes))

(def-goal (achieve (objects-and-boxes-perceived ?nr-objs ?nr-boxes))
  (let ((objs nil)
        (boxes nil))
    (loop while (and (< (length objs) ?nr-objs) (< (length boxes) ?nr-boxes))
          do (with-designators ((update-semantic-map 
                                (action 
                                 '((to update-sematic-map))))
                               (get-containers 
                                (action 
                                 '((to get-container-objects))))
                               (get-objects
                                (action 
                                 '((to get-graspable-objects)))))
               (perform update-semantic-map)
               (setf objs (perform get-objects))
               (setf boxes (perform get-containers))))
  (format t "Objects perceived")
  `(,objs ,boxes)))

(defun get-box (boxes side)
  "Returns the box that is on the given side of the other box"
  (let ((x1 (first (desig-prop-value (first boxes) 'coords)))
        (x2 (first (desig-prop-value (second boxes) 'coords))))
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
    (if (not pos)
        (format t "ERROR2")
        (progn
          (if (eql pos 'left-gripper) 
              'left-arm
              (if (eql pos 'right-gripper)
                  'right-arm))))))

(defun get-best-arm (obj)
  "Returns the arm closest to the object"
  (let ((coords (get-coords obj)))
    (if (< (first coords) 0)
        'left-arm
        'right-arm)))

(defun get-coords (obj)
  "Returns the coordinates of the object"
  (desig-prop-value (desig-prop-value obj 'at) 'coords))

(defun switch-arms (arm)
  (if (eql 'left-arm arm)
      'left-arm
      'right-arm))
