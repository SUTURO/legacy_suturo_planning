(in-package :suturo-executive)

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of three objects located on the table"
  (ros-info (suturo-executive) "TODO"))

(def-top-level-cram-function test-planlib ()
  (suturo-planlib::test-plan 'suturo-planlib:test1))
