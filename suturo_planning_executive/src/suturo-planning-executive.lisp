(in-package :suturo-planning-executive)

(defvar *attempts-to-find-food-left* 3)
(defvar *attempts-to-reach-initial-pose* 4)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (suturo-planning-process-module:suturo-planning-process-module)
     ,@body))

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table."
  (with-process-modules
    (with-failure-handling
        ; Initial position not reached.
        ((suturo-planning-planlib::pose-not-reached
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (manipulation-failure plan)
            "Failed bring the robot in the initial pose.")
           (setq *attempts-to-reach-initial-pose*
                 (- *attempts-to-reach-initial-pose* 1))
           (if (not (= *attempts-to-reach-initial-pose* 0))
               (retry))))
      ; Take initial position.
      (suturo-planning-planlib::reach-position 'suturo-planning-planlib:initial))
    (with-failure-handling
        ; No edible object found
        ((suturo-planning-planlib::no-food-found 
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (perceive-failure plan)
            "No edible object found.")
           (retry))
         ; Still could not touch object.
         (suturo-planning-planlib::touch-failed
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (manipulation-failure plan)
            "Still failed to touch object after serveral attempts.")
           (setq *attempts-to-find-food-left* 
                 (- *attempts-to-find-food-left* 1))
           (if (not (= *attempts-to-find-food-left* 0))
               (retry))))
      ; Find all objects.
      (let ((perceived-objects (suturo-planning-planlib::find-objects))
            (edible-obj-indicator 'suturo-planning-planlib:the))
        (with-failure-handling
            ; Found too many (>1) edible objects.
            ((suturo-planning-planlib::food-overflow 
               (f)
               (declare (ignore f))
               (roslisp:ros-error
                (perceive-failure plan)
                "More than one edible object.")
               (setq edible-obj-indicator 'suturo-planning-planlib:all)
               (retry)))
          ; Find all edible objects.
          (let ((edible-object (first (suturo-planning-planlib::get-edible-objects
                                       edible-obj-indicator
                                       perceived-objects)))
                (arm 'suturo-planning-planlib:right)
                (attempts-left 4))
            (with-failure-handling
                ; Could not touch object for first time.
                ((suturo-planning-planlib::touch-failed
                   (f)
                   (declare (ignore f))
                   (roslisp:ros-error
                    (manipulation-failure plan)
                    "Failed to touch the object")
                   (setq arm (switch-arms arm))
                   (setq attempts-to-touch-left (- attempts-to-touch-left 1))
                   (if (> attempts--to-touch-left 0) (retry))))
                 ; Touch edible object with desired arm.
                 (suturo-planning-planlib::touch-object arm edible-object))))))
      (ros-info (suturo-planning-executive) "PLAN SUCCESS")))

(defun switch-arms (arm)
  "Returns 'suturo-planning-planlib:right' in case 'suturo-planning-planlib:left' has been passed, 'suturo-planning-planlib:left' else"
  (if (eql arm 'suturo-planning-planlib:left)
      'suturo-planning-planlib:right
      'suturo-planning-planlib:left))
