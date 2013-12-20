(in-package :suturo-planning-pmd-manipulation)

(def-fact-group suturo-manipulation-designators (action-desig)

  (<- (action-desig ?desig (take-pose ?pose)) ; bleibt (identifyer home/home-position) 
      ;;Action designator for action to move the robot to a specified pose.
      (desig-prop ?desig (to take-pose))
      (desig-prop ?desig (pose ?pose)))

  (<- (action-desig ?desig (move-head ?direction)) ; object mit coordinaten (perceive obj)/fake obj für obj suchen  / move head server
      ;;Action designator for action to move the head of the robot to a specific direction.
      (desig-prop ?desig (to move-head))
      (desig-prop ?desig (direction ?direction)))

  (<- (action-desig ?desig (grasp ?obj ?arm)) ; name-obj (identifyer) & arm 
      ;;Action designator for action to take an object with the hand of the specified arm.
      (desig-prop ?desig (to grasp))
      (desig-prop ?desig (arm ?arm))
      (desig-prop ?desig (obj ?obj)))

  (<- (action-desig ?desig (open-hand ?arm)) ; 
      ;;Action designator for action to open the hand of the specified arm.
      (desig-prop ?desig (to open-hand))
      (desig-prop ?desig (arm ?arm)))

  (<- (action-desig ?desig (move-arm ?location ?arm)) ; pose arm coorsystem
      ;;Action designator for action to move the specified arm with the hand over the specified box.
      (desig-prop ?desig (to move-arm))
      (desig-prop ?desig (arm ?arm))
      (desig-prop ?desig (loc ?location))))


(def-fact-group suturo-planning-pm-manipulation (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pm-manipulation)
    (or (desig-prop ?designator (to move))
        (desig-prop ?designator (to move-head))
        (desig-prop ?designator (to grasp))
        (desig-prop ?designator (to open-hand))
        (desig-prop ?designator (to move-arm))))
  
  (<- (available-process-module suturo-planning-pm-manipulation)
    (symbol-value cram-projection:*projection-environment* nil)))