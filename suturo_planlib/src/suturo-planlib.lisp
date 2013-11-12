(in-package :suturo-planlib)

(declare-goal test-plan (indicator)
             (roslisp:ros-info (suturo planlib)
                               "TEST-PLAN ~a" indicator))

(def-goal (test-plan test1)
    (roslisp:ros-info (suturo planlib)
                      "TEST1 COMPLETED"))
