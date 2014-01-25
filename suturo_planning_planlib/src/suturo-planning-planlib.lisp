(in-package :suturo-planning-planlib)

(defvar *perceived-objects* nil)

(declare-goal achieve (occasion)
  "Achieves `occasion' if it is not yet achieved."
  (info-out (suturo planlib) "Trying to achieve ~a" (generate-output occasion)))
  ;(when (holds occasion)
   ; (info-out (planlib) "Occasion '~a' already achieved." occasion)
    ;(return nil)))

;(defun holds (?occ) nil)

(defun generate-output (occ)
  (let ((output nil))
    (map nil (lambda (item)
               (if (typep item 'object-designator)
                   (push (desig-prop-value item 'name) output)
                   (push item output)))
         occ)
    (reverse output)))

