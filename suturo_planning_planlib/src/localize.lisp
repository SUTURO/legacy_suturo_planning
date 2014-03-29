(in-package :suturo-planning-planlib)

(defvar *transform-listener* nil)

(defvar *table-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top")
(defvar *counter-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_sink_block")
(defvar *red-box-name* "http://www.suturo.de/ontology/semantic#r_dumpster")

(defvar *location-on-table-nr* 0)
(defvar *location-on-counter-nr* 0)
(defvar *location-on-red-box-nr* 0)
(defvar *locations-on-table* nil)
(defvar *locations-on-counter* nil)
(defvar *locations-on-red-box* nil)

(defvar *location-to-see-table* nil) 
(defvar *location-to-see-counter* nil)

(defvar *locations-to-reach* nil)
(defvar *location-to-reach-nr* 0)
(defvar *location-to-reach-name* nil)

(defvar *quaternion-table* '(0 0 1 0))
(defvar *quaternion-counter* '(0 0 0 1))

(defparameter *gap-object-robot* 0.4)
(defparameter *gap-between-objects* 0.15)
(defparameter *gap-table-center-robot* 0.5)

(defun init-localize ()
  (setf *location-to-see-table* (make-pose '(0 0 0)
                                           *quaternion-table*))
  (setf *location-to-see-counter* (make-pose '(0 0 0)
                                             *quaternion-counter*)))

(defmethod reference ((loc location-designator) &optional (role *default-role*))
  123)

(defmethod reference :around ((loc location-designator) &optional (role *default-role*))
  (format t "reference ~a~%" loc)
  (cond
    ;; Location to reach something
    ((eql (desig-prop-value loc 'to) 'reach)
     (cond 
       ((desig-prop-value loc 'loc)
        (let* ((loc2 (desig-prop-value loc 'loc))
               (pose (reference loc2))
               (origin (cl-tf:origin pose))
               (x (if (equal (desig-prop-value loc2 'name) *table-name*)
                      (+ (cl-tf:x origin) *gap-table-center-robot*)
                      (- (cl-tf:x origin) *gap-table-center-robot*)))
               (quaternion (if (equal (desig-prop-value loc2 'name) *table-name*)
                               *quaternion-table*
                            *quaternion-counter*)))
          (make-pose `(,x ,(cl-tf:y origin) 0)
                     quaternion)))
       ((desig-prop-value loc 'obj)
        (let* ((obj (desig-prop-value loc 'obj))
               (name (desig-prop-value obj 'name)))
          (when (not (equal name *location-to-reach-name*))
            (generate-locations-to-reach obj)
            (setf *location-to-reach-name* name)
            (setf *location-to-reach-nr* 0))
          (nth *location-to-reach-nr* *locations-to-reach*)))))            
    ;; Location to see something
    ((eql (desig-prop-value loc 'to) 'see)
     (let ((name (desig-prop-value loc 'name)))
       (cond 
         ((equal name *table-name*) *location-to-see-table*)
         ((equal name *counter-name*) *location-to-see-counter*)))) 
    ;; Location on something
    ((desig-prop-value loc 'on)
     (let ((name (desig-prop-value loc 'name)))
       (cond 
         ((equal name *table-name*) 
          (if (not *locations-on-table*)
              (generate-locations-on name))
          (nth *location-on-table-nr*
               *locations-on-table*))
          ((equal name *counter-name*) 
          (if (not *locations-on-counter*)
              (generate-locations-on name))
          (nth *location-on-counter-nr*
               *locations-on-counter*))
         ((equal name *red-box-name*) 
          (if (not *locations-on-red-box*)
              (generate-locations-on name))
          (nth *location-on-red-box-nr*
               *locations-on-red-box*)))))
    ;; Someone fucked up
    (t (make-pose (desig-prop-value loc 'coords) (desig-prop-value loc 'pose)))))

(defun next-solution (loc)
  (cond 
    ((desig-prop-value loc 'on)
     (let ((name (desig-prop-value loc 'name)))
       (cond
         ((equal name *table-name*) 
          (incf *location-on-table-nr*)
          (nth *location-on-table-nr* 
               *locations-on-table*))
         ((equal name *counter-name*)
          (incf *location-on-counter-nr*)
          (nth *location-on-counter-nr*
               *locations-on-counter*)))))
    ((eql (desig-prop-value loc 'to) 'reach)
     (if (eql *location-to-reach-nr* (- (length *locations-to-reach*) 1))
         (setf *location-to-reach-nr* 0)
         (incf *location-to-reach-nr*))
     (nth *location-to-reach-nr* *locations-to-reach*))))

(defun make-pose (vector quaternion)
  (tf:make-pose-stamped "/map" 0.0
                        (tf:make-3d-vector (nth 0 vector)
                                           (nth 1 vector)
                                           (nth 2 vector))
                        (tf:make-quaternion (nth 0 quaternion)
                                            (nth 1 quaternion)
                                            (nth 2 quaternion)
                                            (nth 3 quaternion))))

(defun generate-locations-to-reach (obj)
  (let* ((coords (get-coords obj))
         (x (+ (nth 0 coords) *gap-object-robot*))
         (locations nil))
    (format t "asd ~a" coords)
    (setf *location-to-reach-name* (desig-prop-value obj 'name))
    (push (make-pose `(,x ,(+ (nth 1 coords) 0.12) 0) *quaternion-table*)  
          locations)
    (push (make-pose `(,x ,(- (nth 1 coords) 0.12) 0) *quaternion-table*) 
          locations)
    (push (make-pose `(,x ,(+ (nth 1 coords) 0.06) 0) *quaternion-table*)
          locations)
    (push (make-pose `(,x ,(- (nth 1 coords) 0.06) 0) *quaternion-table*)  
          locations)
    (push (make-pose `(,x ,(nth 1 coords) 0) *quaternion-table*) 
          locations)
    (setf *locations-to-reach* locations)))

(defun generate-locations-on (name)
  (let* ((obj (get-furniture name))
         (dims (desig-prop-value obj 'dimensions))
         (coords (get-coords obj))
         (x (first coords))
         (y (second coords))
         (z (+ (third coords) (/ (first dims) 2))))
    (cond 
      ((equal name *table-name*)
       (push (make-pose `(,(+ x 0.2) ,(+ y *gap-between-objects* -0.2) ,z) *quaternion-table*) *locations-on-table*)
       (push (make-pose `(,(+ x 0.2) ,(- y *gap-between-objects* 0.2) ,z) *quaternion-table*) *locations-on-table*)
       (push (make-pose `(,(+ x 0.2) ,(- y 0.2) ,z) *quaternion-table*) *locations-on-table*))
      ((equal name *counter-name*)
       (push (make-pose `(,(- x 0.15) ,(+ y *gap-between-objects* -0.2) ,z) *quaternion-counter*) *locations-on-counter*)
       (push (make-pose `(,(- x 0.15) ,(- y *gap-between-objects* 0.2) ,z) *quaternion-counter*) *locations-on-counter*)
       (push (make-pose `(,(- x 0.15) ,(- y 0.2) ,z) *quaternion-counter*) *locations-on-counter*))
      ((equal name *red-box-name*)
       (push (make-pose `(,x ,(+ y *gap-between-objects*) ,z) *quaternion-table*) *locations-on-red-box*)
       (push (make-pose `(,x ,(- y *gap-between-objects*) ,z) *quaternion-table*) *locations-on-red-box*)
       (push (make-pose `(,x ,y ,z) *quaternion-table*) *locations-on-red-box*)))))
    
(defun get-furniture (name)
  (let ((gen (json-prolog:prolog-simple-1 (format nil
                                                  "getKnowrobDimension('~a',Out)"
                                                  name))))
     (suturo-planning-common::json-prolog->short-designator gen)))

; (perform (make-designator 'action `((to get-static-object)
;                                      (name ,name)))))
    
    