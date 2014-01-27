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
      (error-out (suturo exec) "Could not reach initial pose.")))
    (suturo-planning-planlib:achieve 
      '(suturo-planning-planlib:home-pose)))
  (let ((objects-to-perceive 3)
        (retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::not-enough-objects-found (f)
           (declare (ignore f))
           (error-out (suturo exec) "Not enough objects found.")
           (sleep 2)
           (when (> objects-to-perceive 1)
             (error-out (suturo exec) "Trying again with fewer objects.")
             (decf objects-to-perceive)
             (retry)))
         (suturo-planning-common::simple-plan-failure (f)
           (declare (ignore f))
           (error-out (suturo exec) "Couldn't put all objects away")
           (sleep 2)
           (if (> retry-counter 0)
               (prog1 (decf retry-counter)
                 (retry))
               (error-out (suturo exec) "Sorry I failed."))))
    (let ((result (suturo-planning-planlib:achieve
                   `(suturo-planning-planlib:objects-and-boxes-perceived 
                     ,objects-to-perceive 2))))
      (suturo-planning-planlib:achieve 
       `(suturo-planning-planlib:objects-in-appropriate-boxes 
           ,(first result)
           ,(second result)))
      (setf retry-counter 2)))))