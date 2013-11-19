(in-package :suturo-planlib)

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
  (let ((filtered-objects (get-edible-objects 'all ?objs)))
    (cond ((= (length filtered-objects) 1)
           filtered-objects)
          ((= (length filtered-objects) 0)
           (cpl:error 'no-food-found
                      :result filtered-objects))
          (t (cpl:error 'food-overflow
                        :result filtered-objects)))
    (roslisp:ros-info (suturo planlib)
                      "FOUND THE EDIBLE OBJECT ~a"
                      filtered-objects)))

(def-goal (get-edible-objects all ?objs)
  (let ((filtered-objects ?objs)) ;todo pm einbauen
    (roslisp:ros-info (suturo planlib)
                      "OBJECTS FILTERED ~a"
                      filtered-objects)
    filtered-objects))

(def-goal (touch-object ?obj)
  (roslisp:ros-info (suturo planlib)
                      "TOUCHED OBJECT"))

