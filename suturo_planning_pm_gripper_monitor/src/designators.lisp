(in-package :suturo-planning-pm-gripper-monitor)

(def-fact-group suturo-gripper-monitor-designators (action-desig)
  
  (<- (action-desig ?desig (start-monitoring-gripper ?arm)) 
    ;;Action designator for action to start monitoring the gripper-joint
    (desig-prop ?desig (to start-monitoring-gripper))
    (desig-prop ?desig (arm ?arm)))
	
  (<- (action-desig ?desig (end-monitoring-gripper))
    ;;Action designator for action to end monitoring the gripper-joint
    (desig-prop ?desig (to end-monitoring-gripper)))

  (<- (action-desig ?desig (gripper-is-closed))
    ;;Action designator for action to end monitoring the gripper-joint
    (desig-prop ?desig (to gripper-is-closed)))
  
  (<- (action-desig ?desig (get-gripper-state ?arm)) 
    ;;Action designator for action to start monitoring the gripper-joint
    (desig-prop ?desig (to get-gripper-state))
    (desig-prop ?desig (arm ?arm)))

  (<- (action-desig ?desig (monitor-gripper ?arm))
    ;;Action designator for action to end monitoring the gripper-joint
    (desig-prop ?desig (to monitor-gripper))
    (desig-prop ?desig (arm ?arm))))
  
(def-fact-group suturo-planning-pm-gripper-monitor (matching-process-module
                                                 available-process-module)
  
  (<- (matching-process-module ?designator suturo-planning-pm-gripper-monitor)
    (or (desig-prop ?designator (to start-monitoring-gripper))
        (desig-prop ?designator (to end-monitoring-gripper))
        (desig-prop ?designator (to monitor-gripper))
        (desig-prop ?designator (to gripper-is-closed))
        (desig-prop ?designaotr (to get-gripper-state))))
  
  (<- (available-process-module suturo-planning-pm-gripper-monitor)
    (symbol-value cram-projection:*projection-environment* nil)))
