(in-package :suturo-planning-planlib)

(def-goal (achieve (initial-pose))
  (with-designators 
      ((reach-initial-pose (action
                            '((desig-props:to desig-props:move)
                              (desig-props:pose desig-props:initial)))))
    (perform reach-initial-pose)
    (format t "Initial pose reached")))

(def-goal (achieve (object-in-hand ?obj))
  (format t "~a in hand" ?obj))

(def-goal (achieve (object-in-box ?obj ?box))
  (format t "~a in box ~a" ?obj ?box))

(def-goal (achieve (objects-in-appropriate-boxes ?objs ?boxes))
  (format t "~a in boxes ~a" ?objs ?boxes))

(def-goal (achieve (objects-perceived))
  (format t "Objects perceived"))

