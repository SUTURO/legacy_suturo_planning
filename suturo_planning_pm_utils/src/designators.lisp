(in-package :suturo-planning-pm-utils)

(def-fact-group suturo-util-designators (action-desig)

  (<- (action-desig ?desig (get-best-arm ?obj))
      (desig-prop ?desig (to get-best-arm))
      (desig-prop ?desig (obj ?obj)))

  (<- (action-desig ?desig (get-location-over ?loc))
      (desig-prop ?desig (to get-location-over))
      (desig-prop ?desig (loc ?loc))))

(def-fact-group suturo-planning-pm-utils (matching-process-module
                                          available-process-module)
  
  (<- (matching-process-module ?designator suturo-planning-pm-utils)
      (or (desig-prop ?designator (to get-best-arm))
          (desig-prop ?designator (to get-location-over))))
  
  (<- (available-process-module suturo-planning-pm-utils)
      (symbol-value cram-projection:*projection-environment* nil)))