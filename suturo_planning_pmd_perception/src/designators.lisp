(in-package :suturo-planning-pmd-perception)

(def-fact-group suturo-manipulation-designators (action-desig)
  
  (<- (action-desig ?desig (perceive))
    (desig-prop ?desig (to perceive))))
  
(def-fact-group suturo-planning-pmd-perception (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pmd-perception)
    (or (desig-prop ?designator (to perceive))))
  
  (<- (available-process-module suturo-planning-pmd-perception)
    (symbol-value cram-projection:*projection-environment* nil)))
