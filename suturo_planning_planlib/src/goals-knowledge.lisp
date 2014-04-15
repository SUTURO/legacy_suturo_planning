(in-package :suturo-planning-planlib)

(defun get-locations-to-face ()
  `(,(make-designator 'location '((coords (1 0.4 0.4))
                                  (frame "/base_link")))
    ,(make-designator 'location '((coords (1 -0.4 0.4))
                                  (frame "/base_link")))
    ,(make-designator 'location '((coords (1 0 0.1))
                                  (frame "/base_link")))))

(def-goal (perceive (?obj))
  (let* ((objs nil)
         (loc (desig-prop-value ?obj 'at))
         (loc-to-see (make-designator 'location `((to reach) (loc ,loc))))
         (name (desig-prop-value loc 'name))
         (edible (desig-prop-value ?obj 'edible)))
    (achieve '(home-pose))
    (achieve `(robot-at ,loc-to-see))
    (loop for loc-to-face in (get-locations-to-face)
          do (achieve `(face-loc ,loc-to-face))
             (perform (make-designator 'action 
                              `((to update-objects-on) 
                                (name ,name)))))
    (look-for-unknown-objs (get-objects-on name))
    (setf objs (get-objects-on))
    (setf objs (remove-if (lambda (desig) 
                            (not (eql (desig-prop-value desig 'edible)
                                      edible)))
                          objs))
    objs))

(defun get-objects-on (name)
  (json-prolog->designators (json-prolog:prolog-simple-1 (format nil "onObject('~a',Out)" name))))

(defun look-for-unknown-objs (objs table)
  (loop for obj in objs
        do (when (is-unknown obj)
             (achieve `(face-loc ,(desig-prop-value obj 'at)))
             (perform (make-designator 'action 
                              `((to update-objects-on) 
                                (name ,table)))))))

(defun is-unknown (obj)
  nil)