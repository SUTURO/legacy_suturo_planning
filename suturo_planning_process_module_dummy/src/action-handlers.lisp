(in-package :suturo-planning-process-module-dummy)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler perceive ()
  "Returns a list of objects perceived on the table"
  (vector 1 2 3))

(def-action-handler ground (objs)
  "Gets a "
  (roslisp:ros-info (suturo-pm ground)
                    "Grounding object.")
  (list 1))

(def-action-handler move (pose)
  "Foobar"
  (roslisp:ros-info (suturo-pm move)
                    "Reached pose ~a."
                    pose))

(def-action-handler touch (arm obj)
  "Moves the arm to the object"
  (roslisp:ros-info (suturo-pm touch)
                    "Touched object"))
