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
          (format t "Result: ~a~%" ?OUT)
          ?OUT)
        (roslisp:ros-warn nil "Could not update semantic map."))))

(def-action-handler clear-maps ()
  "Clears the semantic map and planning scene"
  (json-prolog:prolog-simple-1 "clearPerceived"))

;; TODO: Error-handling
(def-action-handler placed-object-in-box (object box)
  "Invokes Knowledge Representation's process to update a object that has been placed in a box"
  (let ((gen (json-prolog:prolog-simple-1 (format nil "placedObjectInBox('~a', '~a', Out)"
                                                  (desig-prop-value object 'name)
                                                  (desig-prop-value box 'name)))))))

(def-action-handler get-objects-with-properties (object props)
  "Retrieves objects that match the given designator"
  (let* ((gen (suturo-planning-common:designator->string object props))
         (res (json-prolog:prolog-simple-1 gen)))
    (roslisp:ros-info (get-objects-with-properties) "Generated prolog function call: ~a~%" gen)
    (roslisp:ros-info (get-objects-with-properties) "Result of prolog function call: ~a~%" res)
    (if res
      (suturo-planning-common:json-prolog->designators res)
      (roslisp:ros-warn nil "Could not find any matching objects."))))

(def-action-handler update-objects-on (object-name)
  "Retrieves objects that match the given designator"
  (let ((gen (json-prolog:prolog-simple-1 (format nil "updatePerception('~a', Out)" object-name))))
    (roslisp:ros-info (update-objects-on) "Result of prolog function call: ~a~%" gen)))

(def-action-handler get-container-objects ()
  "Receives all containers from Knowledge Representation as a list."
  (let* ((loc
           (make-designator
            'location
            `((on ,(make-designator
                    'object
                    `((name "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top")))))))
         (objs (call-action 'get-objects-with-properties (make-designator 'object `((type container) 
                                                                                    (at ,loc))))))
    (if objs
        objs
        (roslisp:ros-warn nil "Could not receive container objects."))))


(def-action-handler get-graspable-objects (on-name)
  "Receives all graspable objects from Knowledge Representation as a list"
  (let* ((on-desig (make-designator 'object `((name ,on-name))))
         (loc (make-designator 'location `((on ,on-desig))))
         (objs (call-action 'get-objects-with-properties (make-designator 'object `((at ,loc))) `(at))))
    (if objs
        (progn
          (mapcar #'(lambda (x)
                      (let* ((loc-desig (make-designator 'location
                                                         (update-designator-properties
                                                          `((on ,on-desig))
                                                          (description (desig-prop-value x 'at))))))
                        (make-designator 'object
                                         (update-designator-properties
                                          `((at ,loc-desig))
                                          (description x)))))
                  objs))
        (progn
          (roslisp:ros-warn nil "Could not receive graspable objects.")
          nil))))

(def-action-handler get-static-object (object-name)
  "Returns a object designator of the object in the semantic map with the
   given name."
  (let ((gen (json-prolog:prolog-simple-1 (format nil
                                                  "getKnowrobDimension('~a',Out)"
                                                  object-name))))
     (suturo-planning-common::json-prolog->short-designator gen)))

(defparameter *learn-object-success* 0)
(defparameter *learn-object-to-close-to-other-object* 1)
(defparameter *learn-object-object-lost* 2)
(defparameter *learn-object-failed* 3)

(def-action-handler learn-object (action name)
  "Learn object stuff."
  (let ((gen (json-prolog:prolog-simple-1
              (cond
                ((eq action 'learn-object-start) (format nil "startObjectLearning"))
                ((eq action 'learn-object-learn) (format nil "learnObject('~a', Out)" name))
                ((eq action 'learn-object-abort) (format nil "abortLearning"))
                ((eq action 'learn-object-finish) (format nil "finishLearning"))
                (t (cpl:error 'suturo-planning-common::unhandled-value))))))
  (cond
    ((eq action 'learn-object-learn) (cdr (first (first gen)))))))

(def-action-handler scan-barcode (obj)
  "Scan barcode and retrieve found object"
  (json-prolog:prolog-simple-1 (format nil "scanObject('~a', Out)" (desig-prop-value obj 'name))))
