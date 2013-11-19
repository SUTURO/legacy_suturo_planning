(in-package :suturo-planlib)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (suturo-process-module:suturo-process-module)
     ,@body))

(define-condition food-overflow (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "food-overflow"))

(define-condition no-food-found (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-food-found"))

(declare-goal test-plan (indicator)
  (roslisp:ros-info (suturo planlib)
                    "TEST-PLAN ~a" indicator))

(declare-goal reach-position (indicator)
  (roslisp:ros-info (suturo planlib)
                    "INITIAL-POSITION ~a" indicator))

(declare-goal find-objects ()
  (roslisp:ros-info (suturo planlib)
                    "FIND-OBJECTS"))

(declare-goal get-edible-objects (indicator designators)
  (roslisp:ros-info (suturo planlib)
                    "GET-EDIBLE-OBJECTS ~a ~a" indicator designators))

(declare-goal touch-object (designator)
  (roslisp:ros-info (suturo planlib)
                    "TOUCH-OBJECT ~a" designator))              

(def-goal (test-plan test1)
  (roslisp:ros-info (suturo planlib)
                    "TEST1 COMPLETED"))

(def-goal (reach-position initial)
  (roslisp:ros-info (suturo planlib)
                    "REACHED INITIAL-POSITION"))

(def-goal (find-objects)
  (roslisp:ros-info (suturo planlib)
                    "OBJECTS FOUND")
  (list (make-designator
         'cram-designators:object
         `((desig-props:type desig-props:box)
           (desig-props:color desig-props:blue)))
        (make-designator
         'cram-designators:object
         `((desig-props:type desig-props:box)
           (desig-props:color desig-props:red)))))

(def-goal (get-edible-objects the ?objs)
  (let ((edible-objects (get-edible-objects 'all ?objs)))
    (cond ((= (length edible-objects) 1)
           edible-objects)
          ((= (length edible-objects) 0)
           (cpl:error 'no-food-found
                      :result edible-objects))
          (t (cpl:error 'food-overflow
                        :result edible-objects)))
    (roslisp:ros-info (suturo planlib)
                      "FOUND THE EDIBLE OBJECT ~a"
                      edible-objects)))

(def-goal (get-edible-objects all ?objs)
  (with-process-modules
    (with-designators
        ((act-ground (action
                      `((desig-props:to
                         desig-props:ground)
                        (desig-props:obj, ?objs)))))      
      (perform act-ground)
      (roslisp:ros-info (suturo planlib)
                        "OBJECTS FILTERED")
      ?objs)))     ;?objs muss spaeter geloescht werden

(def-goal (touch-object ?obj)
  (roslisp:ros-info (suturo planlib)
                      "TOUCHED OBJECT"))

