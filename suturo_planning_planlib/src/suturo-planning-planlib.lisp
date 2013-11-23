(in-package :suturo-planning-planlib)

(define-condition food-overflow (simple-plan-failure)
  "Condition is thrown if to many edible objects found."
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "food-overflow"))

(define-condition no-food-found (simple-plan-failure)
  "Condition is thrown if no edible objects found."
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-food-found"))

(define-condition touch-failed (simple-plan-failure)
  "Condition is thrown if the robot failed to touch the edible object."
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "touch-failed"))

(define-condition pose-not-reached (simple-plan-failure)
  "Condition is thrown if the selected pose could not be reached by the robot."
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "pose-not-reached"))

(declare-goal test-plan (indicator)
  "Testrun for top-level plan."
  (roslisp:ros-info (suturo planlib)
                    "TEST-PLAN ~a" indicator))

(declare-goal reach-position (indicator)
  "Manipulation moves arms into position"
  (roslisp:ros-info (suturo planlib)
                    "INITIAL-POSITION ~a" indicator))

(declare-goal find-objects ()
  "Perception perceives objects"
  (roslisp:ros-info (suturo planlib)
                    "FIND-OBJECTS"))

(declare-goal get-edible-objects (indicator designators)
  "Knowledge Representation returns edible object(s)"
  (roslisp:ros-info (suturo planlib)
                    "GET-EDIBLE-OBJECTS ~a ~a" indicator designators))

(declare-goal touch-object (indicator designator)
  "Manipulation moves arm to object"
  (roslisp:ros-info (suturo planlib)
                    "TOUCH-OBJECT ~a" designator))              

(def-goal (test-plan test1)
    (roslisp:ros-info (suturo planlib)
                      "TEST1 COMPLETED"))

(def-goal (reach-position initial)
    (with-designators
        ((act-pose (action
                    `((desig-props:to
                       desig-props:move)
                      (desig-props:pose, 'initial)))))
      (perform act-pose)
      (roslisp:ros-info (suturo planlib)
                        "REACHED INITIAL-POSITION")))

(def-goal (find-objects)
    (with-designators
        ((obj-1 (object ()))
         (act-perceive (action
                        `((desig-props:to
                           desig-props:perceive)))))      
      (let ((results (perform act-perceive)))
        (format t "Foo ~a~%" results)
        (roslisp:ros-info (suturo planlib)
                          "OBJECTS FOUND")
        results)))
;      (list (make-designator
;             'cram-designators:object
;             `((desig-props:type desig-props:box)
;               (desig-props:color desig-props:blue)))
;            (make-designator
;             'cram-designators:object
;             `((desig-props:type desig-props:box)
;               (desig-props:color desig-props:red))))))

(def-goal (get-edible-objects the ?objs)
    (let ((edible-objects (get-edible-objects 'all ?objs)))
      (cond ((= (length edible-objects) 1)
             edible-objects)
            ((= (length edible-objects) 0)
             ;Condtion is thrown if no edible objects were found.
             (cpl:error 'no-food-found
                        :result edible-objects))
             ;Condition is thrown if to many (>1) edible objects ware found. 
            (t (cpl:error 'food-overflow
                          :result edible-objects)))
      (roslisp:ros-info (suturo planlib)
                        "FOUND THE EDIBLE OBJECT ~a"
                        edible-objects)))

(def-goal (get-edible-objects all ?objs)
    (with-designators
        ((act-ground (action
                      `((desig-props:to
                         desig-props:ground)
                        (desig-props:obj ,?objs)))))      
      (perform act-ground)
      (roslisp:ros-info (suturo planlib)
                        "OBJECTS FILTERED")
      ?objs))    ;?objs muss spaeter geloescht werden

(def-goal (touch-object ?arm ?obj)
    (with-designators
        ((act-touch (action
                     `((desig-props:to
                        desig-props:touch)
                        (desig-props:arm ,?arm)
                        (desig-props:obj ,?obj)))))
      (perform act-touch)
      (roslisp:ros-info (suturo planlib)
                        "TOUCHED OBJECT")))

