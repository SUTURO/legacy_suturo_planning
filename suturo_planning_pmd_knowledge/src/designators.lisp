(in-package :suturo-planning-pmd-knowledge)

(def-fact-group suturo-knowledge-dummy-designators (action-desig)

  (<- (action-desig ?desig (update-semantic-map))
    ;;Action designator for action to get information about objects.
    (desig-prop ?desig (to update-semantic-map)))

  (<- (action-desig ?desig (placed-object-in-box ?obj ?container))
    (desig-prop ?desig (to placed-object-in-box))
    (desig-prop ?desig (obj ?obj))
    (desig-prop ?desig (container ?container)))
  
  (<- (action-desig ?desig (get-objects-with-properties ?obj))
    (desig-prop ?desig (to get-objects-with-properties))
    (desig-prop ?desig (obj ?obj))))

(def-fact-group suturo-planning-pmd-knowledge (matching-process-module
                                               available-process-module)
  
  (<- (matching-process-module ?designator suturo-planning-pmd-knowledge)
    (or (desig-prop ?designator (to update-semantic-map))
        (desig-prop ?designator (to placed-object-in-box))
        (desig-prop ?designator (to get-objects-with-properties))))
  
  (<- (available-process-module suturo-planning-pmd-knowledge)
    (symbol-value cram-projection:*projection-environment* nil)))
