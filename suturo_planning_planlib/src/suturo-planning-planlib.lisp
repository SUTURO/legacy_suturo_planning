(in-package :suturo-planning-planlib)

(declare-goal achieve (occasion)
  "Achieves `occasion' if it is not yet achieved."
  (ros-info (suturo planlib) "Trying to achieve ~a" (generate-output occasion)))
  ;(when (holds occasion)
   ; (info-out (planlib) "Occasion '~a' already achieved." occasion)
    ;(return nil)))

;(defun holds (?occ) nil)

(defun generate-output (occ)
  (let ((output nil))
    (map nil (lambda (item)
               (if (typep item 'object-designator)
                   (push (object-output item) output)
                   (if (not (typep item 'location-designator))
                       (push item output))))
         occ)
    (reverse output)))

(defun object-output (obj)
  (subseq (desig-prop-value obj 'name) 40))

