(in-package :suturo-planning-planlib)

(declare-goal reach-position (indicator)
  ;Manipulation moves arms into position
  (roslisp:ros-info (suturo planlib)
                    "INITIAL-POSITION ~a" indicator))

(declare-goal find-objects ()
  ;Perception perceives objects
  (roslisp:ros-info (suturo planlib)
                    "FIND-OBJECTS"))

(declare-goal get-edible-objects (indicator designators)
  ;Knowledge Representation returns edible object(s)
  (roslisp:ros-info (suturo planlib)
                    "GET-EDIBLE-OBJECTS ~a" indicator))

(declare-goal touch-object (indicator designator)
  ;Manipulation moves arm to object
  (roslisp:ros-info (suturo planlib)
                    "TOUCH-OBJECT"))              

(def-goal (reach-position suturo-planning-common:initial)
    (with-designators
        ((act-pose (action
                    `((desig-props:to
                       desig-props:move)
                      (desig-props:pose, 'suturo-planning-common:initial)))))
      (perform act-pose)
      (roslisp:ros-info (suturo planlib)
                        "REACHED INITIAL-POSITION")))

(def-goal (find-objects)
    (with-designators
        ((act-perceive (action
                        `((desig-props:to
                           desig-props:perceive)))))      
      (let ((results (perform act-perceive)))
        (if results
            results
            (cpl:error 'suturo-planning-common::no-object-perceived))
        (roslisp:ros-info (suturo planlib) "OBJECTS FOUND"))))

(def-goal (get-edible-objects the ?objs)
    (let ((edible-objects (get-edible-objects 'all ?objs)))
      (cond ((= (length edible-objects) 1)
             edible-objects)
            ((= (length edible-objects) 0)
             ;Condtion is thrown if no edible objects were found.
             (cpl:error 'suturo-planning-common::no-food-found
                        :result edible-objects))
             ;Condition is thrown if to many (>1) edible objects ware found.
            (t (cpl:error 'suturo-planning-common::food-overflow
                          :result edible-objects)))
      (roslisp:ros-info (suturo planlib)
                        "FOUND THE EDIBLE OBJECT ~a"
                        edible-objects)
      edible-objects))

(def-goal (get-edible-objects all ?objs)
    (with-designators
        ((act-ground (action
                      `((desig-props:to
                         desig-props:ground)
                        (desig-props:obj ,?objs)))))      
        (let ((result (perform act-ground)))
          (roslisp:ros-info (suturo planlib)
                            "OBJECTS FILTERED")
          result)))

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

