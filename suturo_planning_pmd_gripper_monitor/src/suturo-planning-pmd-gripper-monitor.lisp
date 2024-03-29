(in-package :suturo-planning-pmd-gripper-monitor)

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

(def-process-module suturo-planning-pmd-gripper-monitor (desig)
  (apply #'call-action (reference desig)))
