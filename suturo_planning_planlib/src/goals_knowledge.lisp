(in-package :suturo-planning-planlib)

(def-goal (perceive (?obj))
  (let ((objs (perform (make-designator 'action `((to get-objects-with-properties) ,?obj)))))
    (if (not objs)
      (let* ((loc (desig-prop-value ?obj 'at))
             ; TODO: Also consider 'in', ...
             (on (desig-prop-value loc 'on)))
        (if on
          (let* ((on-obj (perceive (make-designator 'object `((name ,on)))))
                 (over-obj (perform (make-designator 'action `((to get-location-over) (loc ,loc))))))
            ; TODO: Bewegen
            (perform (make-designator 'action `((to move-head) (loc ,over-obj))))
            (perform (make-designator 'action '((to update-semantic-map))))
            (perceive ?obj))))
      objs)))

