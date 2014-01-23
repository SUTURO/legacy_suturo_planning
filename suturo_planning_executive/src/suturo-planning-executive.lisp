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
      (error-out (planning exec) "Could not reach initial pose.")))
    (suturo-planning-planlib::achieve 
      '(suturo-planning-planlib::home-pose)))
  (with-failure-handling
    ((suturo-planning-common::no-object-perceived (f)
      (declare (ignore f))
      (error-out (planning exec) "No more objects found.")))
    (let ((result (suturo-planning-planlib::achieve
                    '(suturo-planning-planlib::objects-and-boxes-perceived 3 2))))
      (with-failure-handling
        ((suturo-planning-common::grasping-failed (f)
          (declare (ignore f))
          (error-out (planning exec) "Grasping failed.")))
        (suturo-planning-planlib::achieve 
          `(suturo-planning-planlib::objects-in-appropriate-boxes ,(first result)
                                                                  ,(second result)))))))


;;; "test"

(def-top-level-cram-function test-desig ()
  (with-designators ((loc-1 (location 
                             `((in table)))) 
                     (loc-2 (location 
                             `((in cupboard))))) 
    (with-designators ((obj-1 (object 
                               `((name "obj1") (type cup) (at ,loc-1))))
                       (obj-2 (object
                               `((type box) (at ,loc-2)))))
      (equate obj-1 obj-2) 
      (format t "first-desig: ~a ~% current-desig: ~a" (first-desig obj-1) (current-desig obj-1))
      (desig-prop-value loc-1 'in))))

(defvar *test-loc1* (make-designator 'location '((frame 1) 
                                                 (coords (1 1 1)))))

(defvar *test-obj1* (make-designator 'object `((name test-obj) 
                                               (type graspable-object) 
                                               (at ,*test-loc1*))))

(def-top-level-cram-function test-goals ()
  (with-dummy-process-modules
    (suturo-planning-planlib::achieve 
     `(suturo-planning-planlib:object-in-hand ,*test-obj1*))))

(def-top-level-cram-function test-man-pmd ()
  (with-dummy-process-modules
    (with-designators ((grasp-obj (action 
                                   `((to grasp) 
                                     (arm left-arm) 
                                     (obj ,*test-obj1*)))))
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
                                     '((to get-graspable-objects)))))
      (perform get-objects))))

(def-top-level-cram-function test-kno-pmd2 ()
  (with-dummy-process-modules
    (with-designators ((update (action 
                                     '((to update-semantic-map)))))
      (perform update))))

(def-top-level-cram-function test-kno-pmd3 ()
  (with-dummy-process-modules
    (with-designators ((get-objects (action 
                                     '((to get-container-objects)))))
      (perform get-objects))))
