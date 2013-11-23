(in-package :suturo-planning-process-module-dummy)

(defgeneric call-action (action &rest params))

(defmethod call-action ((action-sym t) &rest params)
  (roslisp:ros-info
    (suturo process-module-dummy)
    "Unimplemented operation `~a' with parameters ~a. Doing nothing."
    action-sym parmas)
    (sleep 0.5))

(defmethod call-action :around (action-sym &rest params)
  (roslisp:ros-info (suturo process-module-dummy)
                    "Executing action ~a ~a."
                    action-sym params)
  (prog1 (call-next-method)
    (roslisp:ros-info (suturo process-module-dummy)
                      "Action done.")))

(def-process-module suturo-planning-process-module-dummy (desig)
  (apply #'call-action (reference desig)))
