(in-package :suturo-planning-planlib)

(defun get-locations-to-face ()
  `(,(make-designator 'location '((coords (1 0.4 0.4))
                                  (frame "/base_link")))
    ,(make-designator 'location '((coords (1 -0.4 0.4))
                                  (frame "/base_link")))
    ,(make-designator 'location '((coords (1 0 0.1))
                                  (frame "/base_link")))))

(def-goal (perceive (?objs))
  "Moves the Robot to the a position from where he can see the location of
   object. Then updates the Perception and returns the objects that match
   the description of `?obj'."
  (let* ((loc (desig-prop-value (first ?objs) 'at))
         (loc-to-see (make-designator 'location `((to reach) (loc ,loc))))
         (name (desig-prop-value loc 'name)))
    (achieve `(robot-at ,loc-to-see))
    (achieve '(home-pose))
    (loop for loc-to-face in (get-locations-to-face)
          do (achieve `(face-loc ,loc-to-face))
             (perform (make-designator 'action 
                                       `((to update-objects-on) 
                                         (name ,name)))))
    (look-for-unknown-objs (get-objects-on name) name)
    (get-objects-on name)))

(defun get-objects-on (name)
  "Returns all perceived objects over the surface of `name'"
  (perform (make-designator 'action
                            `((to get-graspable-objects)
                              (name ,name)))))

(defun look-for-unknown-objs (objs table)
  "Takes a list of objects and faces all unknown objects and
   updates the perception."
  (loop for obj in objs
        do (when (desig-prop-value obj 'unknown)
             (achieve `(face-loc ,(desig-prop-value obj 'at)))
             (perform (make-designator 'action 
                              `((to update-objects-on) 
                                (name ,table)))))))