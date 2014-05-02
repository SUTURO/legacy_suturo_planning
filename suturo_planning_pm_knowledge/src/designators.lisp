(in-package :suturo-planning-pm-knowledge)

(def-fact-group suturo-knowledge-designators (action-desig)
  
  (<- (action-desig ?desig (update-semantic-map))
    ;;Action designator for action to get information about objects.
    (desig-prop ?desig (to update-semantic-map)))

  (<- (action-desig ?desig (clear-maps))
    (desig-prop ?desig (to clear-maps)))

  (<- (action-desig ?desig (placed-object-in-box ?obj ?container))
    (desig-prop ?desig (to placed-object-in-box))
    (desig-prop ?desig (obj ?obj))
    (desig-prop ?desig (container ?container)))
  
  (<- (action-desig ?desig (get-objects-with-properties ?obj ?props))
    (desig-prop ?desig (to get-objects-with-properties))
    (desig-prop ?desig (obj ?obj))
    (desig-prop ?desig (props ?props)))

  (<- (action-desig ?desig (update-objects-on ?name))
    (desig-prop ?desig (to update-objects-on))
    (desig-prop ?desig (name ?name)))

  (<- (action-desig ?desig (get-static-object ?name))
    (desig-prop ?desig (to get-static-object))
    (desig-prop ?desig (name ?name)))

  (<- (action-desig ?desig (get-container-objects))
    ;;Action designator for action to get information about objects.
    (desig-prop ?desig (to get-container-objects)))
  
  (<- (action-desig ?desig (get-graspable-objects ?name))
    ;;Action designator for action to get information about objects.
    (desig-prop ?desig (to get-graspable-objects))
    (desig-prop ?desig (name ?name)))

  (<- (action-desig ?desig (learn-object ?action ?name))
    ;;Action designator for action to get information about objects.
    (desig-prop ?desig (to learn-object))
    (desig-prop ?desig (action ?action))
    (desig-prop ?desig (name ?name)))

  (<- (action-desig ?desig (scan-barcode ?obj))
    (desig-prop ?desig (to scan-barcode))
    (desig-prop ?desig (obj ?obj))))

(def-fact-group suturo-planning-pm-knowledge (matching-process-module
                                              available-process-module)
  
  (<- (matching-process-module ?designator suturo-planning-pm-knowledge)
    (or (desig-prop ?designator (to update-semantic-map))
        (desig-prop ?designator (to clear-maps))
        (desig-prop ?designator (to placed-object-in-box))
        (desig-prop ?designator (to get-objects-with-properties))
        (desig-prop ?designator (to update-objects-on))
        (desig-prop ?designator (to get-container-objects))
        (desig-prop ?designator (to get-static-object))
        (desig-prop ?designator (to get-graspable-objects))
        (desig-prop ?designator (to learn-object))
        (desig-prop ?designator (to scan-barcode))))
  
  (<- (available-process-module suturo-planning-pm-knowledge)
    (symbol-value cram-projection:*projection-environment* nil)))
