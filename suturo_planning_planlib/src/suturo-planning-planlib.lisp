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

(defmacro with-perceived-objects (vars-and-objs &rest body)
  "Perceives the given objects and binds all equal designators to 
   the variable"
  (let ((let-list nil)
        (perceive-list nil)
        result)
    (loop for x in vars-and-objs
          do (push `(perceive '(,(second x))) perceive-list)
             (push `(,(first x) (get-equal-designators ,(second x))) 
                   let-list))
    (push `(let ,let-list ,@body) result)
    (setf result (append perceive-list result))
    (push 'progn result)
    result))

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

(defun get-coords (desig)
  "Returns the coordinates of the object"
  (if (typep desig 'object-designator)
      (desig-prop-value (desig-prop-value (current-desig desig) 'at) 'coords)
      (if (typep desig 'location-designator)
          (desig-prop-value desig 'coords)
          nil)))

