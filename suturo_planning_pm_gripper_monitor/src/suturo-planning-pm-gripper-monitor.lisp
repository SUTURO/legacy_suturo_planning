(in-package :suturo-planning-pm-gripper-monitor)

(defgeneric call-action (action &rest params))

(defmethod call-action ((action-sym t) &rest params)
  (roslisp:ros-info
    (suturo process-module)
    "Unimplemented operation `~a' with parameters ~a. Doing nothing."
    action-sym params)
    (sleep 0.5))

(defmethod call-action :around (action-sym &rest params)
  (roslisp:ros-info (suturo process-module)
                    "Executing action in gripper-monitor: ~a ~a."
                    action-sym params)
  (prog1 (call-next-method)
    (roslisp:ros-info (suturo process-module)
                      "Action done.")))

(def-process-module suturo-planning-pm-gripper-monitor (desig)
  (apply #'call-action (reference desig)))

(defun get-gripper-pose (gripper &key (target-frame "/base_footprint"))
  (format t "Getting gripper pose from ~a~%" gripper)
  (if (not *tf*)
      (defparameter *tf* (make-instance 'cl-tf:transform-listener)))
  (let* ((source-frame (cond
                         ((eq gripper 'left-gripper) "l_wrist_roll_link")
                         ((eq gripper 'left-arm) "l_wrist_roll_link")
                         ((eq gripper 'right-gripper) "r_wrist_roll_link")
                         ((eq gripper 'right-arm) "r_wrist_roll_link")
                         (t nil)))                        
         (time (roslisp:ros-time))
         (result nil)
         (intents 5)
         (timeout 1))
    (loop while (and source-frame (not result) (> intents 0))
          do (setf timeout (* timeout 1.5))
             (setf result (tf:wait-for-transform *tf*
                                                 :timeout timeout
                                                 :time time
                                                 :source-frame source-frame
                                                 :target-frame target-frame))
             (if (not result)                 
                 (progn
                   (format t "No result. Result: ~a, *tf*: ~a Retrying.~%" result *tf*)
                   (decf intents)
                   (setf time (roslisp:ros-time)))))
    (if result
        (cl-transforms:transform->pose
         (tf:lookup-transform *tf*
                              :time time
                              :source-frame source-frame
                              :target-frame target-frame)))))