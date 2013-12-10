(in-package :suturo-planning-pmd-manipulation)

(def-fact-group suturo-manipulation-designators (action-desig)

  (<- (action-desig ?desig (move ?pose))
      ;Action designator for action to move the robot to a specified pose.
    (desig-prop ?desig (to move))
    (desig-prop ?desig (pose ?pose)))

  (<- (action-desig ?desig (touch ?arm ?obj))
      ;Action designator for action to touch an object with the specified arm.
    (desig-prop ?desig (to touch))
    (desig-prop ?desig (arm ?arm))
    (desig-prop ?desig (obj ?obj)))

  (<- (action-desig ?desig (move-head ?direction))
      ;Action designator for action to move the head of the robot in a specific direction.
    (desig-prop ?desig (to move-head))
    (desig-prop ?desig (direction ?direction)))

  (<- (action-desig ?desig (take-object ?arm ?obj))
      ;Action designator for action to take an object with the hand of the specified arm.
    (desig-prop ?desig (to take-object))
    (desig-prop ?desig (arm ?arm))
    (desig-prop ?desig (obj ?obj)))

  (<- (action-desig ?desig (move-arm-over-box ?arm ?box))
      ;Action designator for action to move the specified arm with the hand over the specified box.
    (desig-prop ?desig (to mover-arm-over-box))
    (desig-prop ?desig (arm ?arm))
    (desig-prop ?desig (box ?box)))

  (<- (action-desig ?desig (move-closer-to-ohter-arm ?arm ?obj))
      ;Action designator for action to move the specified object with one specified arm closer to the other arm. 
    (desig-prop ?desig (to move-closer-to-other-arm))
    (desig-prop ?desig (arm ?arm))
    (desig-prop ?desig (obj ?obj))))


(def-fact-group suturo-planning-pmd-manipulation (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pmd-manipulation)
    (or (desig-prop ?designator (to move))
        ;desig-prop ?designator (to touch))
        (desig-prop ?designator (to move-head))
        (desig-prop ?designator (to take-object))
        (desig-prop ?designator (to move-arm-over-box))
        (desig-prop ?designator (to move-closer-to-other-arm))))
  
  (<- (available-process-module suturo-planning-pmd-manipulation)
    (symbol-value cram-projection:*projection-environment* nil)))
