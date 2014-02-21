(in-package :suturo-planning-planlib)

(defvar *map-subscriber* nil)

(def-goal (locate (location))
  (let* ((act (desig-prop-value location 'to-execute))
         (obj (desig-prop-value location 'obj))
         (in (desig-prop-value location 'in)))
    (cond
      ((eq act 'put-down)
       (get-put-down in))
    )))

(defun get-put-down-location (in)
  (format t "get put down location"))










 #|(format t "locate")
  (if (not *map-subscriber*)
      (setf *map-subscriber*
          (roslisp:subscribe "/map"
                             "nav_msgs/OccupancyGrid"
                             #'(lambda (map) (refresh-map map)))))
  
t)

(defun refresh-map (map)
  (format t "refresh"))|#