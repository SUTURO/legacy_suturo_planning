(in-package :suturo-planning-common)

(defvar *tf*)

(defun get-tf (&key force)
  (if (or force (not *tf*))
      (setf *tf* (make-instance 'cl-tf:transform-listener)))
  *tf*)

(roslisp-utilities:register-ros-init-function get-tf)

(defun get-holding-gripper (obj)
  "Retruns the gripper which contians `obj' if it is being contained by any.
Otherwise returns `nil'"
  (let* ((obj-at (desig-prop-value obj 'at))
         (loc (desig-prop-value obj-at 'in)))
    (cond
      ((eq loc 'left-gripper) 'left-gripper)
      ((eq loc 'right-gripper) 'right-gripper)
      (t nil))))

(defun get-gripper-frame (x)
  (cond
    ((eq x 'left-gripper) "/l_wrist_roll_link")
    ((eq x 'left-arm) "/l_wrist_roll_link")
    ((eq x 'right-gripper) "/r_wrist_roll_link")
    ((eq x 'right-arm) "/r_wrist_roll_link")
    (t (let ((gripper (get-holding-gripper x)))
         (if gripper
             (get-gripper-frame gripper)
             nil)))))

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
                       (setf result (desig-prop-value current-at 'in-pose))
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
  (let ((gripper-vector nil)
        (offset-vector nil)
        (result nil)
        (pov-frame (cond
                     ((position gripper '(left-gripper left-arm)) "l_wrist_roll_link")
                     ((position gripper '(right-gripper right-arm)) "r_wrist_roll_link")
                     (t (cpl:error 'suturo-planning-common::unhandled-body-part)))))
    (if (and (setf gripper-vector (transform-get-origin pov-frame target-frame))
             (setf offset-vector (transform-get-origin offset-frame target-frame)))          
        (progn
          (setf result (cl-transforms:v- offset-vector gripper-vector))
          (if (position gripper '(right-gripper right-arm))
              (setf result
                    (cl-transforms:make-3d-vector
                     (cl-transforms:x offset-vector)
                     (* 1 (cl-transforms:y result))
                     (* 1 (cl-transforms:z result)))))))
    result))

(defun pose-stamped->designator (pose-stamped)
  (make-designator
   'location `((frame ,(cl-tf:frame-id pose-stamped))
               (coords (,(cl-transforms:x (cl-tf:origin pose-stamped))
                         ,(cl-transforms:y (cl-tf:origin pose-stamped))
                         ,(cl-transforms:z (cl-tf:origin pose-stamped))))
               (pose (,(cl-transforms:x (cl-tf:orientation pose-stamped))
                       ,(cl-transforms:y (cl-tf:orientation pose-stamped))
                       ,(cl-transforms:z (cl-tf:orientation pose-stamped))
                       ,(cl-transforms:w (cl-tf:orientation pose-stamped)))))))

(defun stamped-transform->transform (stamped-transform)
  (cl-transforms:make-transform (cl-tf:translation stamped-transform)
                                (cl-tf:rotation stamped-transform)))

(defun pose-stamped->transform (pose-stamped)
  (cl-transforms:pose->transform (pose-stamped->pose pose-stamped)))

(defun pose-stamped->stamped-transform (pose-stamped)
  (cl-tf:make-stamped-transform nil
                                (cl-tf:frame-id pose-stamped)
                                (cl-tf:stamp pose-stamped)
                                (cl-transforms:origin pose-stamped)
                                (cl-transforms:orientation pose-stamped)))

(defun pose-stamped->pose (pose-stamped)
  (cl-transforms:make-pose (cl-transforms:origin pose-stamped)
                           (cl-transforms:orientation pose-stamped)))

(defun pose->pose-stamped (pose frame-id)
  (cl-tf:make-pose-stamped frame-id 0.0
                           (cl-transforms:origin pose)
                           (cl-transforms:orientation pose)))


(defun transform-get-pose (source-frame target-frame &key timeout intents)
  (cl-transforms:transform->pose (transform source-frame target-frame :timeout timeout :intents intents)))

(defun transform-get-pose-stamped (source-frame target-frame &key timeout intents)
  (let ((trans (transform source-frame target-frame :timeout timeout :intents intents)))
    (cl-tf:make-pose-stamped target-frame
                             0
                             (cl-transforms:translation trans)
                             (cl-transforms:rotation trans))))

(defun transform->pose (transform)
  (cl-transforms:transform->pose transform ))

(defun transform-get-origin (source-frame target-frame &key timeout intents)
  (cl-transforms:origin (transform-get-pose source-frame target-frame :timeout timeout :intents intents)))

(defun transform->origin (transform)
  (cl-transforms:origin (transform->pose transform)))

(defun transform->orientation (transform)
  (cl-transforms:orientation (transform->pose transform)))

(defun transform->origin-as-list (source-frame target-frame &key timeout intents)
  (let ((origin (transform-get-origin source-frame target-frame :timeout timeout :intents intents)))
    (list (cl-transforms:x origin) (cl-transforms:y origin) (cl-transforms:z origin))))

(defun transform->matrix (source-frame target-frame &key timeout intents)
  (cl-transforms:transform->matrix (transform source-frame target-frame :timeout timeout :intents intents)))

(defun transform->quaternion (source-frame target-frame &key timeout intents)
  (cl-transforms:matrix->quaternion (transform->matrix source-frame target-frame :timeout timeout :intents intents)))

(defun transform->quaternion-as-list (source-frame target-frame &key timeout intents)
  (let ((quat (transform->quaternion source-frame target-frame :timeout timeout :intents intents)))
    (list (cl-transforms:x quat) (cl-transforms:y quat) (cl-transforms:z quat) (cl-transforms:w quat))))

(defun transform-coords-to-frame (source-frame target-frame coords &key timeout intents)
  (let* ((offset (transform-get-origin source-frame target-frame :timeout timeout :intents intents))
         (new-coords (list
                      (+ (first coords) (cl-transforms:x offset))
                      (+ (second coords) (cl-transforms:y offset))
                      (+ (third coords) (cl-transforms:z offset)))))
    (format t "Transformed ~a in ~a to ~a in ~a.~%" coords source-frame new-coords target-frame)
    new-coords))

(defun transform (source-frame target-frame &key timeout intents)
  (format t "Transforming  from ~a to ~a with timeout ~a and ~a intents.~%"
          source-frame target-frame timeout intents)
  (let ((result nil)
        (time (roslisp:ros-time))
        (additional-time 0))
    (loop while (or (and (not intents) (not result)) (and (not result) (> intents 0)))
          do (setf result
                   (if timeout
                       (tf:wait-for-transform (get-tf)
                                              :timeout (+ timeout additional-time)
                                              :time time
                                              :source-frame source-frame
                                              :target-frame target-frame)
                       (tf:wait-for-transform (get-tf)
                                              :time time
                                              :source-frame source-frame
                                              :target-frame target-frame)))
             (format t "result: ~a~%" result)
             (if (not result)
                 (progn
                   (format t "No result. Time: ~a, result: ~a, *tf*: ~a. Retrying.~%" time result *tf*)
                   (if intents
                       (decf intents)
                       (incf additional-time))
                   (setf time (roslisp:ros-time)))))
    (if result
        (progn
          (tf:lookup-transform *tf*
                               :time time
                               :source-frame source-frame
                               :target-frame target-frame))
        nil)))

(defun cl-transforms-euler-degree->quaternion (&key (ax 0.0) (ay 0.0) (az 0.0))
 "Converts roll, pitch and yaw angles (in degree) to a (x y z w) quaternion"
 (let ((fac (/ pi 180.0)))
   (cl-transforms:euler->quaternion :ax (* ax fac) :ay (* ay fac) :az (* az fac))))
 
(defun cl-transforms-euler-degree->quaternion-as-list (&key (ax 0.0) (ay 0.0) (az 0.0))
 (let ((quat (cl-transforms-euler-degree->quaternion :ax ax :ay ay :az az)))
   (list (cl-transforms:x quat) (cl-transforms:y quat) (cl-transforms:z quat) (cl-transforms:w quat))))

(defun split-string (str pattern)
  (ppcre:split pattern str))

(defun desig-prop-value-concat (desig prop-names)
  (reduce #'(lambda (x y) (desig-prop-value x y)) prop-names :initial-value desig))