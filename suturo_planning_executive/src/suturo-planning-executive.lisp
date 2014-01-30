(in-package :suturo-planning-executive)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pm-manipulation:suturo-planning-pm-manipulation
     suturo-planning-pm-knowledge:suturo-planning-pm-knowledge
     suturo-planning-pm-gripper-monitor:suturo-planning-pm-gripper-monitor
     suturo-planning-pm-utils:suturo-planning-pm-utils)
     ,@body))

(defmacro with-dummy-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pmd-manipulation:suturo-planning-pmd-manipulation
     suturo-planning-pmd-knowledge:suturo-planning-pmd-knowledge
     suturo-planning-pmd-gripper-monitor:suturo-planning-pmd-gripper-monitor
     suturo-planning-pm-utils:suturo-planning-pm-utils)
     ,@body))

(def-top-level-cram-function clean-table (obj-nr)
  "Starts the plan on the PR2"
  (with-process-modules
    (clean-table-plan obj-nr)))

(def-top-level-cram-function clean-table-dummy (obj-nr)
  "Starts the plan with stubbed process modules"
  (with-dummy-process-modules
    (clean-table-plan obj-nr)))

(def-cram-function clean-table-plan (obj-nr)
  "Plan to clean a table in front of the robot with 3 objects and 2 boxes on it"
  (perform (make-designator 'action '((to clear-maps))))
  (with-failure-handling
      ((suturo-planning-common::pose-not-reached (f)
         (declare (ignore f))
         (error-out (suturo exec) "Could not reach initial pose.")))
    (suturo-planning-planlib:achieve 
     '(suturo-planning-planlib:home-pose)))
  (let ((objects-to-perceive obj-nr)
        (retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::not-enough-objects-found (f)
           (declare (ignore f))
           (error-out (suturo exec) "Did not find the required number of objects.")
           (sleep 3)       
           (when (not (eq objects-to-perceive 1))
             (error-out (suturo exec) "Trying again objects.")
             (perform (make-designator 'action '((to clear-maps))))
             ;(decf objects-to-perceive)
             (retry)))
         (suturo-planning-common::simple-plan-failure (f)
           (declare (ignore f))
           (error-out (suturo exec) "Couldn't put all objects away")
           (sleep 2)
           (if (> retry-counter 0)
               (prog1 (decf retry-counter)
                 (retry))
               (error-out (suturo exec) "Oh noo! Sorry I failed."))))
      (let ((result (suturo-planning-planlib:achieve
                     `(suturo-planning-planlib:objects-and-boxes-perceived 
                       ,objects-to-perceive 2))))
        (suturo-planning-planlib:achieve 
         `(suturo-planning-planlib:objects-in-appropriate-boxes 
           ,(first result)
           ,(second result)))
        (setf retry-counter 2))))
  (info-out (suturo-executive) "Oops, I, did, it, again! Passed, with, one, point, zero!")
  (sleep 5.0)
  (info-out (suturo-executive) "Come, on, Barby, let's, go, Party!"))
