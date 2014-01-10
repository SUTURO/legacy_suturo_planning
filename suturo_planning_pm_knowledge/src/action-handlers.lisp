(in-package :suturo-planning-pm-knowledge)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  "Invokes Knowledge Representation's process to update the semantic map."
  (let ((query (concatenate 'string "update_semantic_map(Answer)"))
        (service "/json_prolog/simple_query")
        (ret-service "/json_prolog/next_solution")
        (fin-service "/json_prolog/finish"))
    (format t "~a~%" query)
    (if (not (roslisp:wait-for-service service 10))
        (roslisp:ros-warn nil "Timed out waiting for simple_query")
        (let ((result (roslisp:call-service
                       service
                       "json_prolog/PrologQuery"
                       :mode 0
                       :id ""
                       :query query)))
          (roslisp:with-fields (ok message) result
            (format t "~a~%" ok)
            (format t "~a~%" message)
            (if ok
                (roslisp:ros-info nil "Successfully updated semantic map.")
                (roslisp:ros-warn nil "Could not update semantic map.")))))))
                

(def-action-handler get-container-objects ()
  "Receives all containers from Knowledge Representation as a list."
  (nil))

(def-action-handler get-graspable-objects ()
  "Receives all graspable objects from Knowledge Representation as a list"
  (nil))
