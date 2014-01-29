(in-package :suturo-planning-pm-utils)

(def-fact-group suturo-util-designators (action-desig)

  (<- (action-desig ?desig (get-best-arm ?obj))
      ;;Action designator for action to get information about objects.
      (desig-prop ?desig (to get-best-arm))
      (desig-prop ?desig (obj ?obj))))

(def-fact-group suturo-planning-pm-utils (matching-process-module
                                          available-process-module)
  
  (<- (matching-process-module ?designator suturo-planning-pm-utils)
      (or (desig-prop ?designator (to get-best-arm))))
  
  (<- (available-process-module suturo-planning-pm-utils)
      (symbol-value cram-projection:*projection-environment* nil)))