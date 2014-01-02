(in-package :suturo-planning-planlib)

(def-goal (achieve (initial-pose))
  (with-designators 
      ((reach-initial-pose (action
                            '((to move)
                              (pose initial)))))
    (perform reach-initial-pose)
    (format t "Initial pose reached")))

(def-goal (achieve (object-in-hand ?obj))
  (format t "~a in hand" ?obj))

(def-goal (achieve (object-in-box ?obj ?box))
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

(def-goal (achieve (objects-perceived))
  (format t "Objects perceived"))

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

