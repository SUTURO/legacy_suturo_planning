(in-package :suturo-planning-planlib)

(defvar *perceived-objects* nil)

(declare-goal achieve (occasion)
  "Achieves `occasion' if it is not yet achieved."
  (info-out (planlib) "Trying to achieve ~a" occasion)
  (when (holds occasion)
    (info-out (planlib) "Occasion '~a' already achieved." occasion)
    (return nil)))

(defun holds (?occ) nil)


