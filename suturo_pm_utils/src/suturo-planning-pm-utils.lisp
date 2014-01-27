(in-package :suturo-planning-pm-utils)

(defgeneric call-action (action &rest params))

(defmethod call-action ((action-sym t) &rest params)
  (roslisp:ros-info
    (suturo process-module-utils)
    "Unimplemented operation `~a' with parameters ~a. Doing nothing."
    action-sym params)
    (sleep 0.5))

(defmethod call-action :around (action-sym &rest params)
  (roslisp:ros-info (suturo process-module-utils)
                    "Executing action ~a ~a."
                    action-sym params)
  (prog1 (call-next-method)
    (roslisp:ros-info (suturo process-module-utils)
                      "Action done.")))

(def-process-module suturo-planning-pm-utils (desig)
  (apply #'call-action (reference desig)))
