(in-package :suturo-planning-pmd-knowledge)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler ground(objs)
  "Looks up matching objects from the knowledge base"
 objs)


