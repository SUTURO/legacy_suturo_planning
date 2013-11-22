(in-package :suturo-process-module)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler perceive (obj-desig)
  "Returns a list of objects perceived on the table"
  (roslisp:ros-info (suturo-pm perceive)
                    "Perceiving object."))

(def-action-handler ground (obj-desig)
  "Looks up matching objects from the knowledge base"
  (roslisp:ros-info (suturo-pm ground)
                    "Grounding object."))

(def-action-handler move (pose)
  "Foobar"
  (roslisp:ros-info (suturo-pm move)
                    "Reached pose ~a."
                    pose))

(def-action-handler touch (arm obj)
  "Moves the arm to the object"
  (roslisp:ros-info (suturo-pm touch)
                    "Touched object"))
