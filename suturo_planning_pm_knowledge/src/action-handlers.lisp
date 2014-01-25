(in-package :suturo-planning-pm-knowledge)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  "Invokes Knowledge Representation's process to update the semantic map."
  (let ((gen (json-prolog:prolog-simple-1 "updatePerception(Out)")))
        (if gen
            (with-vars-bound
                (?OUT) gen
              (format t "~a~%" ?OUT)
              ?OUT)
            (roslisp:ros-warn nil "Could not update semantic map."))))

; TODO: Error-handling
(def-action-handler placed-object-in-box (object box)
  "Invokes Knowledge Representation's process to update a object that has been placed in a box"
  (let ((gen (json-prolog:prolog-simple-1 (format nil "placedObjectInBox('~a', '~a', Out)"
                                                      (desig-prop-value object 'name)
                                                      (desig-prop-value box 'name)))))))

(def-action-handler get-container-objects ()
  "Receives all containers from Knowledge Representation as a list."
    (let* ((loc (make-designator 'location `((desig-props:on ,(make-designator 'object `((desig-props:name "kitchen_island_counter_top")))))))
           (gen (json-prolog:prolog-simple-1 (suturo-planning-common:designator->string (make-designator 'object `((desig-props:type container) 
                                                                                           (desig-props:at ,loc)))))))
      (if gen
        (suturo-planning-common::json-prolog->designators gen)
        (roslisp:ros-warn nil "Could not receive container objects."))))


(def-action-handler get-graspable-objects ()
  "Receives all graspable objects from Knowledge Representation as a list"
    (let* ((loc (make-designator 'location `((desig-props:on ,(make-designator 'object `((desig-props:name "kitchen_island_counter_top")))))))
           (gen (json-prolog:prolog-simple-1 (suturo-planning-common:designator->string (make-designator 'object `((desig-props:type graspable) 
                                                                                           (desig-props:at ,loc)))))))
      (if gen
        (suturo-planning-common::json-prolog->designators gen)
        (roslisp:ros-warn nil "Could not receive graspable objects."))))
