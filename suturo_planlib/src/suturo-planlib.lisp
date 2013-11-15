(in-package :suturo-planlib)

(declare-goal test-plan (indicator)
              (roslisp:ros-info (suturo planlib)
                               "TEST-PLAN ~a" indicator))

(declare-goal reach-position (indicator)
              (roslisp:ros-info (suturo planlib)
                                "INITIAL-POSITION ~a" indicator))

(declare-goal find-objects ()
              (roslisp:ros-info (suturo planlib)
                                "FIND-OBJECTS"))

(declare-goal filter-objects (designators)
              (roslisp:ros-info (suturo planlib)
                                "FILTER-OBJECTS ~a" designators))

(declare-goal touch-object (designator)
              (roslisp:ros-info (suturo planlib)
                                "TOUCH-OBJECT ~a" designator))              

(def-goal (test-plan test1)
    (roslisp:ros-info (suturo planlib)
                      "TEST1 COMPLETED"))

(def-goal (reach-position initial)
    (roslisp:ros-info (suturo planlib)
                      "REACHED INITIAL-POSITION"))

(def-goal (find-objects)
    (roslisp:ros-info (suturo planlib)
                      "OBJECTS FOUND"))

(def-goal (filter-objects ?objs)
    (roslisp:ros-info (suturo planlib)
                      "OBJECTS FILTERED"))

(def-goal (touch-object ?obj)
    (roslisp:ros-info (suturo planlib)
                      "TOUCHED OBJECT"))

