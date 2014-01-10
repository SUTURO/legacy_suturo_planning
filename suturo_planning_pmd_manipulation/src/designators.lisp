(in-package :suturo-planning-pmd-manipulation)

(def-fact-group suturo-planning-pmd-manipulation (action-desig)

  (<- (action-desig ?desig (take-pose ?pose)) ; bleibt (identifyer home/home-position) 
      ;;Action designator for action to move the robot to a specified pose.
      (desig-prop ?desig (desig-props:to take-pose))
      (desig-prop ?desig (desig-props:pose ?pose)))

  (<- (action-desig ?desig (move-head ?direction)) ; object mit coordinaten (perceive obj)/fake obj für obj suchen  / move head server
      ;;Action designator for action to move the head of the robot to a specific direction.
      (desig-prop ?desig (desig-props:to move-head))
      (desig-prop ?desig (desig-props:direction ?direction)))

  (<- (action-desig ?desig (grasp ?obj ?arm)) ; name-obj (identifyer) & arm 
      ;;Action designator for action to take an object with the hand of the specified arm.
      (desig-prop ?desig (desig-props:to grasp))
      (desig-prop ?desig (desig-props:arm ?arm))
      (desig-prop ?desig (desig-props:obj ?obj)))

  (<- (action-desig ?desig (open-hand ?arm)) ; 
      ;;Action designator for action to open the hand of the specified arm.
      (desig-prop ?desig (desig-props:to open-hand))
      (desig-prop ?desig (desig-props:arm ?arm)))

  (<- (action-desig ?desig (move-arm ?location ?arm)) ; pose arm coorsystem
      ;;Action designator for action to move the specified arm with the hand over the specified box.
      (desig-prop ?desig (desig-props:to move-arm))
      (desig-prop ?desig (desig-props:arm ?arm))
      (desig-prop ?desig (desig-props:loc ?location))))


(def-fact-group suturo-planning-pmd-manipulation (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pmd-manipulation)
    (or (desig-prop ?designator (to move))
        (desig-prop ?designator (to move-head))
        (desig-prop ?designator (to grasp))
        (desig-prop ?designator (to open-hand))
        (desig-prop ?designator (to move-arm))))
  
  (<- (available-process-module suturo-planning-pmd-manipulation)
    (symbol-value cram-projection:*projection-environment* nil)))