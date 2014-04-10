(in-package :suturo-planning-executive)

(defvar *table-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top")
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
                     (loc-trash (location `((on ,*trash-name*)
                                            (name ,*trash-name*))))
                     (objs-inedible (object `((edible nil)
                                              (at ,loc-table)))))
    (achieve `(all ,objs-inedible in ,loc-trash))
    (achieve `(all ,objs-edible in ,loc-counter))
    (achieve '(home-pose))
    (info-out (suturo executive) "Yeah!!! I've done it.")))




;;;;;;;;;;;;;;; things to make testing easier

(def-top-level-cram-function know-unknown-object-plan (object location)
  ""
  (format t "Iniciating plan.~%")
  (with-process-modules
    (achieve `(,object know-unknown-object ,location))))

(def-top-level-cram-function place-gently-plan (object location)
  ""
  (format t "Iniciating plan.~%")
  (with-process-modules
    (achieve `(,object placed-gently ,location))))

(def-top-level-cram-function from-table-to-box ()
  (with-process-modules
      (with-designators ((table (object `((name ,*table-name*))))
                         (loc-table (location `((on ,table)
                                                (name ,*table-name*))))
                         (loc-box (location `((on ,*trash-name*)
                                              (name ,*trash-name*))))
                         (objs-edible (object `((at ,loc-table)))))
        (achieve `(all ,objs-edible in ,loc-box)))))

(def-top-level-cram-function from-island-to-sink ()
  (with-process-modules
    (with-designators ((table (object `((name ,*table-name*))))
                       (loc-table (location `((on ,table)
                                              (name ,*table-name*))))
                       (loc-counter (location `((on ,*counter-name*)
                                                (name ,*counter-name*))))
                       (objs (object `((at ,loc-table)))))
      (achieve `(all ,objs in ,loc-counter)))))

(def-top-level-cram-function test-plan ()
  (with-process-modules
    (with-designators ((table (object `((name ,*table-name*))))
                       (loc-table (location `((on ,table)
                                              (name ,*table-name*))))
                       (objs (object `((at ,loc-table)))))
      (achieve `(all ,objs in ,loc-table)))))

(defun how-do-i-reach-these-thiiiiiiiiiiings ()
  (test-plan))

(defun drop-obj (side)
  (top-level
    (with-process-modules
      (achieve `(desig-props:empty-hand ,(make-designator 'object `((desig-props:at ,(make-designator 'location `((desig-props:in ,(if (eql side 'left) 'desig-props:left-gripper 'desig-props:right-gripper))))))) boden)))))

(def-top-level-cram-function home-pose ()
  (with-process-modules
        (achieve '(home-pose))))

(defun clear-perceived ()
  (json-prolog:prolog-simple-1 "clearPerceived"))

(defun update-perception ()
  (json-prolog:prolog-simple-1 "updatePerception(O)"))

(defun on-table ()
  (json-prolog:prolog-simple-1 "onObject('http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top', Out)"))

(defun publish-sematic-map ()
  (json-prolog:prolog-simple-1 "publishSemanticMap"))

(defun update-on-table ()
  (json-prolog:prolog-simple-1 "updatePerception('http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top', Out)"))
