(in-package :suturo-planning-pmd-knowledge)

(def-fact-group suturo-knowledge-designators (action-desig)
  
  (<- (action-desig ?desig (ground ?obj))
      ;Action designator for action to get information about objects.
    (desig-prop ?desig (to ground))
    (desig-prop ?desig (obj ?obj))))

(def-fact-group suturo-planning-pmd-knowledge (matching-process-module
                                       available-process-module)

  (<- (matching-process-module ?designator suturo-planning-pmd-knowledge)
    (or (desig-prop ?designator (to ground))))
  
  (<- (available-process-module suturo-planning-pmd-knowledge)
    (symbol-value cram-projection:*projection-environment* nil)))
