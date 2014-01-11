(in-package :suturo-planning-pm-knowledge)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  "Invokes Knowledge Representation's process to update the semantic map."
  (let ((gen (json-prolog:prolog-simple-1 "edibleOnTable(table, OUT)")))
        (if gen
            (with-vars-bound
                (?OUT) gen
              (format t "~a~%" ?OUT)
              ?OUT)
            (roslisp:ros-warn nil "Could not update semantic map."))))

(def-action-handler get-container-objects ()
  "Receives all containers from Knowledge Representation as a list."
    (let ((gen (json-prolog:prolog-simple-1 "edibleOnTable(table, OUT)")))
        (if gen
            (with-vars-bound
                (?OUT) gen
              (format t "~a~%" ?OUT)
              ?OUT)
            (roslisp:ros-warn nil "Could not receive container objects."))))

(def-action-handler get-graspable-objects ()
  "Receives all graspable objects from Knowledge Representation as a list"
   (let ((gen (json-prolog:prolog-simple-1 "containerOnTable(table, OUT)")))
        (if gen
            (with-vars-bound
                (?OUT) gen
              (format t "~a~%" ?OUT)
              ?OUT)
            (roslisp:ros-warn nil "Could not receive graspable objects."))))
