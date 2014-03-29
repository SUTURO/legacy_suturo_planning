(in-package :suturo-planning-planlib)

(def-goal (perceive (?obj))
  (let ((objs (perform (make-designator 'action 
                                        `((to get-objects-with-properties) 
                                          (obj ,?obj)
                                          (props (on)))))))
    (if (not objs)
      (let* ((loc (desig-prop-value ?obj 'at))
             (name (desig-prop-value loc 'name))
             (on-obj (get-furniture name))
             (coords (get-coords on-obj))
             (frame (desig-prop-value (desig-prop-value on-obj 'at) 'frame)))
        (achieve '(home-pose))
        (achieve `(robot-at ,loc))
        (perform (make-designator 'action 
                                  `((to update-objects-on) 
                                    (name ,name))))
        (perform (make-designator 'action 
                                  `((to get-objects-with-properties) 
                                    (obj ,?obj)
                                    (props (on))))))
      objs)))
