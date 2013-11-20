(in-package :suturo-executive)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (suturo-process-module:suturo-process-module)
     ,@body))

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table"
  (with-process-modules
      (suturo-planlib::reach-position 'suturo-planlib:initial)
    (with-failure-handling
        ((suturo-planlib::no-food-found 
          (f)
          (declare (ignore f))
          (roslisp:ros-error
           (peceive-failure plan)
           "No edible object found.")
          (retry)))
      (let ((perceived-objects (suturo-planlib::find-objects))
            (edible-obj-indicator 'suturo-planlib:the))
        (with-failure-handling
            ((suturo-planlib::food-overflow 
              (f)
              (declare (ignore f))
              (roslisp:ros-error
               (perceive-failure plan)
               "More than one edible object.")
              (setq edible-obj-indicator 'suturo-planlib:all)
              (retry)))
          (let ((edible-object (first (suturo-planlib::get-edible-objects
                                       edible-obj-indicator
                                       perceived-objects))))
            (suturo-planlib::touch-object edible-object)))))
    (ros-info (suturo-executive) "PLAN SUCCESS")))

(def-top-level-cram-function test-planlib ()
  (suturo-planlib::test-plan 'suturo-planlib:test1))

(def-top-level-cram-function test-process-module ()
  (with-process-modules
      (with-designators
          ((obj-1 (object `((color red))))
           (act-perceive (action
                     `((desig-props:to
                        desig-props:perceive)
                       (desig-props:obj, obj-1)))))
        (perform act-perceive))))

