(in-package :suturo-planning-executive)

(defvar *attempts-to-find-food* 0)
(defvar *attempts-to-reach-initial-pose* 0)
(defvar *attempts-to-perceive-objects* 0)
(defvar *attempts-to-recognize-and-touch-object* 0)
(defvar *attempts-to-touch* 0)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pm-manipulation:suturo-planning-pm-manipulation
     ;suturo-planning-pm-knowledge:suturo-planning-pm-knowledge
     )
     ,@body))

(defmacro with-dummy-process-modules (&body body)
  `(cpm:with-process-modules-running
     (suturo-planning-pmd-manipulation:suturo-planning-pmd-manipulation
     suturo-planning-pmd-knowledge:suturo-planning-pmd-knowledge)
     ,@body))

(def-top-level-cram-function clean-table ()
  (with-process-modules
    (clean-table-plan)))

(def-top-level-cram-function clean-table-dummy ()
  (with-dummy-process-modules
    (clean-table-plan)))

(def-cram-function clean-table-plan ()
  (format t "This is a plan"))






;;; "test"

(def-top-level-cram-function test-desig ()
  (with-designators ((loc-1 (location 
                             `((on table)))) 
                     (loc-2 (location 
                             `((on cupboard))))) 
    (with-designators ((obj-1 (object 
                               `((type cup) (at ,loc-1))))
                       (obj-2 (object
                               `((type box) (at ,loc-2)))))
      (equate obj-1 obj-2) 
      (format t "first-desig: ~a ~% current-desig: ~a" (first-desig obj-1) (current-desig obj-1))
      (desig-prop-value loc-1 'on))))

(defvar *test-loc1* (make-designator 'location '((desig-props:frame 1) 
                                                 (desig-props:coords (1 1 1)))))

(defvar *test-obj1* (make-designator 'object `((desig-props:name test-obj) 
                                               (desig-props:type graspable-object) 
                                               (desig-props:at ,*test-loc1*))))

(def-top-level-cram-function test-goals ()
  (with-dummy-process-modules
    (suturo-planning-planlib::achieve 
     `(suturo-planning-planlib:object-in-hand ,*test-obj1*))))

(def-top-level-cram-function test-man-pmd ()
  (with-dummy-process-modules
    (with-designators ((grasp-obj (action 
                                   `((desig-props:to grasp) 
                                     (desig-props:arm desig-props:left-arm) 
                                     (desig-props:obj ,*test-obj1*)))))
      (perform grasp-obj))))

(def-top-level-cram-function test-man-pmd2 ()
  (with-dummy-process-modules
    (with-designators ((grasp-obj (action `((to grasp) 
                                            (arm left-arm) 
                                            (obj ,*test-obj1*)))))
      (perform grasp-obj))))

(def-top-level-cram-function test-kno-pmd ()
  (with-dummy-process-modules
    (with-designators ((get-objects (action 
                                     `((desig-props:to desig-props:get-graspable-objects)))))
      (perform get-objects))))

(def-top-level-cram-function test-kno-pmd2 ()
  (with-dummy-process-modules
    (with-designators ((get-objects (action 
                                     `((to get-graspable-objects)))))
      (perform get-objects))))