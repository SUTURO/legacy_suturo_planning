(in-package :suturo-planning-planlib)

(def-goal (achieve (home-pose))
  (with-designators ((take-home-pose (action 
                                      '((desig-props:to take-pose)
                                        (desig-props:pose home)))))
    (perform take-home-pose)
    (format t "Initial pose reached")))

(def-goal (achieve (object-in-hand ?obj))
  "Takes the object in one hand"
  (let ((arm (get-best-arm ?obj))
        (fails 0))
    (with-failure-handling 
        ((simple-plan-failure (f)
           (declare (ignore f))
           (incf fails)
           (if (< fails 4)
               (seq
                 (setf arm (switch-arms arm))
                 (retry)))))
      (with-designators ((grasp-obj (action `((desig-props:to grasp)
                                              (desig-props:obj ,?obj)
                                              (desig-props:arm ,arm)))))
        (perform grasp-obj))))
  (format t "~a in hand" ?obj))

(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (let ((coords (get-coords ?obj)))
    (setf coords `(,(nth 0 coords)
                   ,(nth 1 coords)
                   ,(+ (nth 2 coords) 10)))
    (with-designators ((loc-over-obj (location `((desig-props:coords ,coords)))))
      (with-designators ((move-hand (action `((desig-props:to move-arm)
                                              (desig-props:arm ,?arm)
                                              (desig-props:loc ,loc-over-obj)))))
        (perform move-hand))))
  (format t "Hand over ~a" ?obj))

(def-goal (achieve (empty-hand ?arm))
  "Opens the hand of the given arm"
  (with-designators ((open-hand (action `((desig-props:to open-hand)
                                          (desig-props:arm ,?arm)))))
    (perform open-hand))
  (format t "Hand empty"))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  ;add failure handling
  (achieve `(object-in-hand ,?obj))
  (let ((arm (get-holding-hand (current-desig ?obj))))
    (achieve `(hand-over ,?box ,arm))
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
               (if (desig-prop-value obj 'desig-props:edible)
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
                                 '((desig-props:to update-sematic-map))))
                               (get-containers 
                                (action 
                                 '((desig-props:to get-container-objects))))
                               (get-objects
                                (action 
                                 '((desig-props:to get-graspable-objects)))))
               (perform update-semantic-map)
               (setf objs (perform get-objects))
               (setf boxes (perform get-containers))))
  (format t "Objects perceived")))

(defun get-box (boxes side)
  "Returns the box that is on the given side of the other box"
  (let ((x1 (first (desig-prop-value (first boxes) 'desig-props:coords)))
        (x2 (first (desig-prop-value (second boxes) 'desig-props:coords))))
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
              (desig-prop-value (current-desig obj) 'desig-props:at) 
              'desig-props:in)))
    (if (not pos)
        (format t "ERROR2")
        (seq 
          (if (eql pos 'desig-props:left-gripper) 
              'left-arm
              (if (eql pos 'desig-props:right-gripper)
                  'right-arm))))))

(defun get-best-arm (obj)
  "Returns the arm closest to the object"
  (let ((coords (get-coords obj)))
    (if (< (first coords) 0)
        'desig-props:left-arm
        'desig-props:right-arm)))

(defun get-coords (obj)
  "Returns the coordinates of the object"
  (desig-prop-value (desig-prop-value obj 'desig-props:at) 'desig-props:coords))

(defun switch-arms (arm)
  (if (eql 'desig-props:left-arm arm)
      'desig-props:left-arm
      'desig-props:right-arm))