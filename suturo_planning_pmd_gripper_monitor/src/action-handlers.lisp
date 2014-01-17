(in-package :suturo-planning-pmd-gripper-monitor)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(defvar *gripper-closed* nil) ;(make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(def-action-handler start-monitoring-gripper (arm)
  (nil))

(def-action-handler gripper-is-closed ()
  (nil))

(def-action-handler monitor-gripper (arm)
  (nil))

(def-action-handler end-monitoring-gripper ()
  (nil))

  

