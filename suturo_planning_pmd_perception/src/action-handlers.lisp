(in-package :suturo-planning-pmd-perception)

(defvar *perceive-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler perceive ()
  (if (= *perceive-fails* 1)
      (progn
        (setq *perceive-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *perceive-fails* 1)
        NIL)))
