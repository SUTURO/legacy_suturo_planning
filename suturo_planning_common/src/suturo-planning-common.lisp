(in-package :suturo-planning-common)

(defun get-holding-arm (obj)
  "Retruns the arm which contians `obj' if it is being contained by any.
Otherwise returns `nil'"
  (let* ((obj-at (desig-prop-value obj 'at))
         (loc (desig-prop-value obj-at 'in)))
    (cond
      ((eq loc 'left-arm) 'left-arm)
      ((eq loc 'right-arm) 'right-arm)
      (t nil))))
