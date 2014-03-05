(in-package :suturo-planning-common)

(defun get-holding-gripper (obj)
  "Retruns the gripper which contians `obj' if it is being contained by any.
Otherwise returns `nil'"
  (let* ((obj-at (desig-prop-value obj 'at))
         (loc (desig-prop-value obj-at 'in)))
    (cond
      ((eq loc 'left-gripper) 'left-gripper)
      ((eq loc 'right-gripper) 'right-gripper)
      (t nil))))

(defun get-last-gripper-pose (obj)
  "Returns the gripper's grasping pose of the gripper which is containing `obj'.
If there isn't any `nil' is returned."
  (format t "Getting last gripper pose.~%")
  (let* ((current-designator (current-desig obj))
         (parent-designator (if current-designator (parent current-designator)))
         (current-gripper nil)
         (parent-gripper nil)
         (result nil))
    (if (position
         (desig-prop-value (desig-prop-value current-designator 'at) 'in)
         '(left-gripper right-gripper))
        (loop while (and (not result) current-designator parent-designator)
              do (let* ((current-at (desig-prop-value current-designator 'at))
                        (current-in (desig-prop-value current-at 'in))
                        (parent-at (desig-prop-value parent-designator 'at))
                        (parent-in (desig-prop-value parent-at 'in)))
                   (format t "current: ~a~%parent:~a~%" current-designator parent-designator)
                   (if (and (setf current-gripper current-in) (not (setf parent-gripper parent-in)))
                       (setf result (desig-prop-value current-at 'pose))
                       (progn
                         (setf current-designator parent-designator)
                         (setf parent-designator (parent parent-designator))))))
        (format t "Object is not being hold by any gripper.~%at: ~a~%in: ~a"
                (desig-prop-value current-designator 'at)
                (desig-prop-value (desig-prop-value current-designator 'at) 'in)))
    result))