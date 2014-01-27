(in-package :suturo-planning-executive)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pm-manipulation:suturo-planning-pm-manipulation
     suturo-planning-pm-knowledge:suturo-planning-pm-knowledge
     suturo-planning-pm-gripper-monitor:suturo-planning-pm-gripper-monitor)
     ,@body))

(defmacro with-dummy-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pmd-manipulation:suturo-planning-pmd-manipulation
     suturo-planning-pmd-knowledge:suturo-planning-pmd-knowledge
     suturo-planning-pmd-gripper-monitor:suturo-planning-pmd-gripper-monitor)
     ,@body))

(def-top-level-cram-function clean-table ()
  (with-process-modules
    (clean-table-plan)))

(def-top-level-cram-function clean-table-dummy ()
  (with-dummy-process-modules
    (clean-table-plan)))

(def-cram-function clean-table-plan ()
  (with-failure-handling
    ((suturo-planning-common::pose-not-reached (f)
      (declare (ignore f))
      (error-out (planning exec) "Could not reach initial pose.")))
    (suturo-planning-planlib:achieve 
      '(suturo-planning-planlib:home-pose)))
  (with-failure-handling
    ((suturo-planning-common::no-object-perceived (f)
      (declare (ignore f))
      (error-out (planning exec) "No more objects found.")))
    (let ((result (suturo-planning-planlib:achieve
                    '(suturo-planning-planlib:objects-and-boxes-perceived 3 2))))
      (with-failure-handling
        ((suturo-planning-common::grasping-failed (f)
          (declare (ignore f))
          (error-out (planning exec) "Grasping failed.")))
        (suturo-planning-planlib:achieve 
          `(suturo-planning-planlib:objects-in-appropriate-boxes ,(first result)
                                                                  ,(second result)))))))