(in-package :suturo-planning-executive)

(defvar *kitchen-table* "kitchen_island")
(defvar *kitchen-counter* "kitchen_island_counter")

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

(def-top-level-cram-function clean-table ()
  "Starts the plan on the PR2"
  (with-process-modules
    (clean-table-plan)))

(def-top-level-cram-function place-gently-go (obj loc)
  "Starts the plan"
  (with-process-modules
    (place-gently-plan obj loc)))

(def-top-level-cram-function clean-table-dummy ()
  "Deprecated!!! Starts the plan with stubbed process modules"
  (with-dummy-process-modules
    (clean-table-plan)))

(def-cram-function clean-table-plan ()
  ""
  (with-designators ((loc-table (location '((on *kitchen-table*)
                                            (name *kitchen-table*))))
                     (loc-counter (location '((on *kitchen-counter*)
                                              (name *kitchen-counter*))))
                     (objs-edible (object `((edible t) 
                                            (at ,loc-table)))))
    (achieve `(all ,objs-edible on ,loc-counter))))

(def-cram-function place-gently-plan (object location)
  ""
  (format t "Iniciating plan.~%")
  (format t "Achieving goal.~%")
  (achieve `(,object placed-gently ,location)))
