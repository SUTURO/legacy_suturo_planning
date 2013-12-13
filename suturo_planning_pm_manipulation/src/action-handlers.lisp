(in-package :suturo-planning-pm-manipulation)

(defvar *move-fails* 0)
(defvar *move-head-fails* 0)
(defvar *take-fails* 0)
(defvar *box-move-fails* 0)
(defvar *move-closer-fails* 0)
(defvar *touch-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))