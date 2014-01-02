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






;;; old stuff

(def-top-level-cram-function touch-edible ()
  "Finds and touches the edible object out of the objects located on the table."
  ;; Initialise all counters/variables.
  (init-plan)
  (with-process-modules
    (with-failure-handling
        ;; Initial position not reached.
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
      ;; Take initial position.
      (suturo-planning-planlib::reach-pose 'initial))
    (with-failure-handling
        ;; No edible object found
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
         ;; Perception didn't find any objects
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
         ;; Still could not touch object.
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
      ;; Find all objects.
      (let ((perceived-objects (suturo-planning-planlib::perceive-objects))
            (edible-obj-indicator 'the))
        (with-failure-handling
                                        ; Found too many (>1) edible objects.
            ((suturo-planning-common::food-overflow 
                 (f)
               (declare (ignore f))
               (roslisp:ros-error
                (perceive-failure plan)
                "More than one edible object.")
               (setq edible-obj-indicator 'all)
               (retry)))
          ;; Find all edible objects.
          (let ((edible-object (first (suturo-planning-planlib::get-edible-objects
                                       edible-obj-indicator
                                       perceived-objects)))
                (arm 'right)
                (attempts-to-touch-left *attempts-to-touch*))
            (with-failure-handling
                ;; Could not touch object for first time.
                ((suturo-planning-common::touch-failed
                     (f)
                   (declare (ignore f))
                   (roslisp:ros-error
                    (manipulation-failure plan)
                    "Failed to touch the object")
                   (setq arm (switch-arms arm))
                   (setq attempts-to-touch-left (- attempts-to-touch-left 1))
                   (if (> attempts-to-touch-left 0) (retry))))
              ;; Touch edible object with desired arm.
              (suturo-planning-planlib::touch-object arm edible-object))))))
    (ros-info (suturo-planning-executive) "PLAN SUCCESS")))

(defun switch-arms (arm)
  "Returns 'right' in case 'left' has been passed, 'left' else"
  (if (eql arm 'left)
      'right
      'left))

(defun init-plan ()
  "Sets the amount of attempts for th diffe"
  (setq *attempts-to-find-food* 4)
  (setq *attempts-to-reach-initial-pose* 4)
  (setq *attempts-to-perceive-objects* 4)
  (setq *attempts-to-recognize-and-touch-object* 3)
  (setq *attempts-to-touch* 4))

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

(defun asd ()
  (top-level
    (par 
      (loop do
        (format t "a"))
      (loop do
        (format t "b")))))

(defun qwe ()
  (top-level
    (achieve '(right 1))))

(def-goal (achieve (left ?int))
  (format t "hi~a" ?int))

(def-goal (achieve (right ?int))
  (format t "moin~a" ?int))
