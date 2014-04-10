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
        (format t "Object is not being hold by any gripper.~%at: ~a~%in: ~a~%"
                (desig-prop-value current-designator 'at)
                (desig-prop-value (desig-prop-value current-designator 'at) 'in)))
    result))

(defun calc-gripper-offset (gripper offset-frame &key (target-frame "/base_link"))
  (format t
          "Calculating offset. gripper: ~a, offset-frame: ~a, target-frame: ~a~%"
          gripper offset-frame target-frame)
  (defparameter *tf* (make-instance 'cl-tf:transform-listener))
  (let ((gripper-vector nil)
        (offset-vector nil)
        (result nil)
        (pov-frame (cond
                     ((position gripper '(left-gripper left-arm)) "l_wrist_roll_link")
                     ((position gripper '(right-gripper right-arm)) "r_wrist_roll_link")
                     (t (cpl:error 'suturo-planning-common::unhandled-body-part)))))
    (if (and (setf gripper-vector (transform pov-frame target-frame))
             (setf offset-vector (transform offset-frame target-frame)))          
        (progn
          (setf result (cl-transforms:v- offset-vector gripper-vector))
          (if (position gripper '(right-gripper right-arm))
              (setf result
                    (cl-transforms:make-3d-vector
                     (cl-transforms:x offset-vector)
                     (* 1 (cl-transforms:y result))
                     (* 1 (cl-transforms:z result)))))))
    result))

(defun transform (source-frame target-frame &key (timeout 2))
  (let ((time (roslisp:ros-time))
        (result nil)
        (intents 5))
    (loop while (and (not result) (> intents 0))
          do (setf timeout (* timeout 1.5))
             (setf result
                   (tf:wait-for-transform *tf*
                                          :timeout timeout
                                          :time time
                                          :source-frame source-frame
                                          :target-frame target-frame))
             (format t "result: ~a~%" result)
             (if (not result)
                 (progn
                   (format t "No result. Result: ~a, *tf*: ~a. Retrying.~%" result *tf*)
                   (decf intents)
                   (setf time (roslisp:ros-time)))))
    (if result
        (progn
          (cl-transforms:origin
           (cl-transforms:transform->pose
            (tf:lookup-transform *tf*
                                 :time time
                                 :source-frame source-frame
                                 :target-frame target-frame))))
        nil)))

(defun cl-transforms-euler-degree->quaternion (&key (ax 0.0) (ay 0.0) (az 0.0))
  "Converts roll, pitch and yaw angles (in degree) to a (x y z w) quaternion"
  (let ((fac (/ pi 180.0)))
    (cl-transforms:euler->quaternion :ax (* ax fac) :ay (* ay fac) :az (* az fac))))

(defun cl-transforms-euler-degree->quaternion-as-list (&key (ax 0.0) (ay 0.0) (az 0.0))
  (let ((quat (cl-transforms-euler-degree->quaternion :ax ax :ay ay :az az)))
    (list (cl-transforms:x quat) (cl-transforms:y quat) (cl-transforms:z quat) (cl-transforms:w quat))))