(in-package :suturo-planning-planlib)

(declare-goal initial-pose ()
  "Moves arms into position"
  (roslisp:ros-info (suturo planlib)
                    "INITIAL-POSE"))

(declare-goal reach-pose (indicator)
  "Takes the given pose"
  (roslisp:ros-info (suturo planlib)
                    "REACH-POSITION ~a" indicator))

(declare-goal perceive-objects ()
  "Perceives objects"
  (roslisp:ros-info (suturo planlib)
                    "PERCEIVE-OBJECTS"))

(declare-goal get-edible-objects (indicator designators)
  "Returns edible object(s)"
  (roslisp:ros-info (suturo planlib)
                    "GET-EDIBLE-OBJECTS ~a" indicator))

(declare-goal touch-object (arm designator)
  "Moves arm to object"
  (roslisp:ros-info (suturo planlib)
                    "TOUCH-OBJECT"))  

(def-goal (initial-pose)
  (format t "sdfsdf2")
  (reach-pose 'initial))

(def-goal (reach-pose initial)
    (with-designators
        ((act-pose (action
                    `((desig-props:to
                       desig-props:move)
                      (desig-props:pose, 'initial)))))
      (perform act-pose)
      (roslisp:ros-info (suturo planlib)
                        "REACHED INITIAL-POSITION")))

(def-goal (perceive-objects)
    (with-designators
        ((act-perceive (action
                        `((desig-props:to
                           desig-props:perceive)))))      
      (let ((results (perform act-perceive)))
        (if results
            (progn
              (roslisp:ros-info (suturo planlib) "OBJECTS FOUND ~a" results)
              results)
            (cpl:error 'suturo-planning-common::no-object-perceived)))))

(def-goal (get-edible-objects the ?objs)
    (let ((edible-objects (get-edible-objects 'all ?objs)))
      (cond ((= (length edible-objects) 1)
             edible-objects)
            ((= (length edible-objects) 0)
             ;;Condtion is thrown if no edible objects were found.
             (cpl:error 'suturo-planning-common::no-food-found
                        :result edible-objects))
            ;;Condition is thrown if to many (>1) edible objects were found.
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

