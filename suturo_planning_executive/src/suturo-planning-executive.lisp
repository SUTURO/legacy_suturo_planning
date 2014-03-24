(in-package :suturo-planning-executive)

(defvar *table-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island")
(defvar *counter-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_sink_block")
(defvar *trash-name* "http://www.suturo.de/ontology/semantic#r_dumpster")

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

(def-top-level-cram-function clean-table-dummy ()
  "Deprecated!!! Starts the plan with stubbed process modules"
  (with-dummy-process-modules
    (clean-table-plan)))

(def-cram-function clean-table-plan ()
  ""
  (with-designators ((loc-table (location `((on ,*table-name*)
                                            (name ,*table-name*))))
                     (loc-counter (location `((on ,*counter-name*)
                                              (name ,*counter-name*))))
                     (objs-edible (object `((edible t) 
                                            (at ,loc-table))))
                     (loc-trash (location `((in ,*trash-name*)
                                            (name ,*trash-name*))))
                     (objs-inedible (object `((edible nil)
                                              (at ,loc-trash)))))
    (achieve `(all ,objs-edible on ,loc-counter))
    (achieve `(all ,objs-inedible in ,loc-trash))))

(def-top-level-cram-function place-gently-plan (object location)
  ""
  (format t "Iniciating plan.~%")
  (with-process-modules
    (achieve `(,object placed-gently ,location))))

(def-top-level-cram-function test-plan ()
  (with-process-modules
      (with-designators ((table (object `((name ,*table-name*))))
                         (loc-table (location `((on ,table)
                                                (name ,*table-name*))))
                         (objs-edible (object `((at ,loc-table)))))
        (achieve '(home-pose))
        (sp-planlib::init-localize)
        (achieve `(all ,objs-edible in ,loc-table)))))

(defun how-do-i-reach-these-thiiiiiiiiiiings ()
  (test-plan))
