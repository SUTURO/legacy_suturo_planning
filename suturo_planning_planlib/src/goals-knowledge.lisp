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
    (sleep 1)
    (loop for loc-to-face in (get-locations-to-face)
          do (achieve `(face-loc ,loc-to-face))
             (perform (make-designator 'action 
                              `((to update-objects-on) 
                                (name ,name)))))
    (setf objs 
          (json-prolog->designators (json-prolog:prolog-simple-1 (format nil "onObject('~a',Out)" name))))
    (setf objs (remove-if (lambda (desig) 
                            (not (eql (desig-prop-value desig 'edible)
                                      edible)))
                          objs))
    objs))
