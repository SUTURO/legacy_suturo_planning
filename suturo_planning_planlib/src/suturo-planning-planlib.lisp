(in-package :suturo-planning-planlib)

(declare-goal achieve (occasion)
  "Achieves `occasion' if it is not yet achieved."
  (ros-info (suturo planlib) "Trying to achieve ~a" (generate-output occasion)))
  ;(when (holds occasion)
   ; (info-out (planlib) "Occasion '~a' already achieved." occasion)
    ;(return nil)))

;(defun holds (?occ) nil)

(declare-goal perceive (target)
  "Perceives the target and updates its description
 with the updated properties"
  (ros-info (suturo planlib) "Perceive ~a" target))

(declare-goal locate (location)
  "Gets a general description of the location and 
equates it with a exact location"
  (ros-info (suturo planlib) "Locate ~a" location))


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

