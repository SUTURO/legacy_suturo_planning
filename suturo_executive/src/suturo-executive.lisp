(in-package :suturo-executive)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (suturo-process-module:suturo-process-module)
     ,@body))

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table"
  (with-process-modules
    (with-failure-handling
        ((suturo-planlib::pose-not-reached
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (manipulation-failure plan)
            "Failed bring the robot in the initial pose.")
           (retry)))
      (suturo-planlib::reach-position 'suturo-planlib:initial))
    (with-failure-handling
        ((suturo-planlib::no-food-found 
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (perceive-failure plan)
            "No edible object found.")
           (retry))
         (suturo-planlib::touch-failed
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (manipulation-failure plan)
            "Still failed to touch object after serveral attempts.")
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
                                       perceived-objects)))
                (arm 'suturo-planlib:right)
                (attempts-left 4))
            (with-failure-handling
                ((suturo-planlib::touch-failed
                   (f)
                   (declare (ignore f))
                   (roslisp:ros-error
                    (manipulation-failure plan)
                    "Failed to touch the object")
                   (setq arm (switch-arms arm))
                   (setq attempts-left (- attempts-left 1))
                   (if (> attempts-left 0) (retry))))
                 (suturo-planlib::touch-object arm edible-object))))))
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

(defun switch-arms (arm)
  (if (eql arm 'suturo-planlib:left)
      'suturo-planlib:right
      'suturo-planlib:left))


