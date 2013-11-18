(in-package :suturo-executive)

;(defmacro with-process-modules (&body body)
;  `(cpm:with-process-modules-running
;       (suturo-process-module:suturo-process-module)
;     ,@body))

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table"
  (suturo-planlib::reach-position 'suturo-planlib:initial)
  (let (perceived-objects `(suturo-planlib::find-objects))
    (let (filtered-objects `(suturo-planlib::filter-objects perceived-objects))
      (suturo-planlib::touch-object (first filtered-objects))))
  (ros-info (suturo-executive) "PLAN SUCCESS"))

(def-top-level-cram-function test-planlib ()
  (suturo-planlib::test-plan 'suturo-planlib:test1))
