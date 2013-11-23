(in-package :suturo-planning-process-module)

(def-fact-group suturo-manipulation-designators (action-desig)
  
  (<- (action-desig ?desig (perceive))
      ;Action designator for action to perceive objects.
    (desig-prop ?desig (to perceive)))
  
  (<- (action-desig ?desig (ground ?obj))
      ;Action designator for action to get information about objects.
    (desig-prop ?desig (to ground))
    (desig-prop ?desig (obj ?obj)))

  (<- (action-desig ?desig (move ?pose))
      ;Action designator for action to move the robot to a specified pose.
    (desig-prop ?desig (to move))
    (desig-prop ?desig (pose ?pose)))

  (<- (action-desig ?desig (touch ?arm ?obj))
      ;Action designator for action to touch an object with the specified arm.
    (desig-prop ?desig (to touch))
    (desig-prop ?desig (arm ?arm))
    (desig-prop ?desig (obj ?obj))))

(def-fact-group suturo-planning-process-module (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-process-module)
    (or (desig-prop ?designator (to perceive))
        (desig-prop ?designator (to move))
        (desig-prop ?designator (to ground))
        (desig-prop ?designator (to touch))))
  
  (<- (available-process-module suturo-planning-process-module)
    (symbol-value cram-projection:*projection-environment* nil)))
