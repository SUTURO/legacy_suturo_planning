(in-package :suturo-planning-planlib)

(defvar *perceived-objects* nil)

(declare-goal achieve (occasion)
  "Achieves `occasion' if it is not yet achieved."
  (info-out (planlib) "Trying to achieve ~a" (generate-output occasion)))
  ;(when (holds occasion)
   ; (info-out (planlib) "Occasion '~a' already achieved." occasion)
    ;(return nil)))

;(defun holds (?occ) nil)

(defun generate-output (occ)
  (let ((output nil))
    (loop for item in occ
          do (if (symbolp item)
                 (push item output)
                 (push (desig-prop-value item 'name) output)))
    (reverse output)))


