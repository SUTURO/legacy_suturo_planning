(in-package :suturo-planning-executive)

(defvar *attempts-to-find-food* 0)
(defvar *attempts-to-reach-initial-pose* 0)
(defvar *attempts-to-perceive-objects* 0)
(defvar *attempts-to-recognize-and-touch-object* 0)
(defvar *attempts-to-touch* 0)

(defmacro with-process-modules (&body body)
  `(cpm:with-process-modules-running
       (suturo-planning-process-module:suturo-planning-process-module)
     ,@body))

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table."
  ; Initialise all counters/variables.
  (init-plan)
  (with-process-modules
    (with-failure-handling
        ; Initial position not reached.
        ((suturo-planning-common::pose-not-reached
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
      (suturo-planning-planlib::reach-position 'suturo-planning-common:initial))
    (with-failure-handling
        ; No edible object found
        ((suturo-planning-common::no-food-found 
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (perceive-failure plan)
            "No edible object found.")
           (setq *attempts-to-find-food* 
                 (- *attempts-to-find-food* 1))
           (if (not (= *attempts-to-find-food* 0))
               (retry)))
         ; Perception didn't find any objects
         (suturo-planning-common::no-object-perceived
             (f)
           (declare (ignore f))
           (roslisp:ros-error
            (perceive-failure plan)
            "No objects perceived.")
           (setq *attempts-to-perceive-objects* 
                 (- *attempts-to-perceive-objects* 1))
           (if (not (= *attempts-to-perceive-objects* 0))
               (retry)))
         ; Still could not touch object.
         (suturo-planning-common::touch-failed
           (f)
           (declare (ignore f))
           (roslisp:ros-error
            (manipulation-failure plan)
            "Still failed to touch object after serveral attempts.")
           (setq *attempts-to-recognize-and-touch-object* 
                 (- *attempts-to-recognize-and-touch-object* 1))
           (if (not (= *attempts-to-recognize-and-touch-object* 0))
               (retry))))
      ; Find all objects.
      (let ((perceived-objects (suturo-planning-planlib::find-objects))
            (edible-obj-indicator 'suturo-planning-common:the))
        (with-failure-handling
            ; Found too many (>1) edible objects.
            ((suturo-planning-common::food-overflow 
               (f)
               (declare (ignore f))
               (roslisp:ros-error
                (perceive-failure plan)
                "More than one edible object.")
               (setq edible-obj-indicator 'suturo-planning-common:all)
               (retry)))
          ; Find all edible objects.
          (let ((edible-object (first (suturo-planning-planlib::get-edible-objects
                                       edible-obj-indicator
                                       perceived-objects)))
                (arm 'suturo-planning-common:right)
                (attempts-to-touch-left *attempts-to-touch*))
            (with-failure-handling
                ; Could not touch object for first time.
                ((suturo-planning-common::touch-failed
                   (f)
                   (declare (ignore f))
                   (roslisp:ros-error
                    (manipulation-failure plan)
                    "Failed to touch the object")
                   (setq arm (switch-arms arm))
                   (setq attempts-to-touch-left (- attempts-to-touch-left 1))
                   (if (> attempts-to-touch-left 0) (retry))))
                 ; Touch edible object with desired arm.
                 (suturo-planning-planlib::touch-object arm edible-object))))))
      (ros-info (suturo-planning-executive) "PLAN SUCCESS")))

(defun switch-arms (arm)
  "Returns 'suturo-planning-common:right' in case 'suturo-planning-common:left' has been passed, 'suturo-planning-common:left' else"
  (if (eql arm 'suturo-planning-common:left)
      'suturo-planning-common:right
      'suturo-planning-common:left))

(defun init-plan ()
  "Sets the amount of attempts for th diffe"
  (setq *attempts-to-find-food* 4)
  (setq *attempts-to-reach-initial-pose* 4)
  (setq *attempts-to-perceive-objects* 4)
  (setq *attempts-to-recognize-and-touch-object* 3)
  (setq *attempts-to-touch* 4))
