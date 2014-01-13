(in-package :suturo-planning-pm-knowledge)

(def-fact-group suturo-knowledge-designators (action-desig)
  
  (<- (action-desig ?desig (update-semantic-map))
      ;;Action designator for action to get information about objects.
      (desig-prop ?desig (to update-semantic-map)))

  (<- (action-desig ?desig (get-container-objects))
      ;;Action designator for action to get information about objects.
      (desig-prop ?desig (to get-container-objects)))

  (<- (action-desig ?desig (get-graspable-objects))
      ;;Action designator for action to get information about objects.
      (desig-prop ?desig (to get-graspable-objects))))

(def-fact-group suturo-planning-pm-knowledge (matching-process-module
                                              available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pm-knowledge)
    (or (desig-prop ?designator (to update-semantic-map))
        (desig-prop ?designator (to get-container-objects))
        (desig-prop ?designator (to get-graspable-objects))))
  
  (<- (available-process-module suturo-planning-pm-knowledge)
    (symbol-value cram-projection:*projection-environment* nil)))
