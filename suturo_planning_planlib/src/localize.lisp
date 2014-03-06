(in-package :suturo-planning-planlib)

(defvar *transform-listener* nil)

(defvar *table-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_island_counter_top")
(defvar *counter-name* "http://ias.cs.tum.edu/kb/knowrob.owl#kitchen_sink_block_counter_top")

(defvar *location-on-table-nr* 0)
(defvar *location-on-counter-nr* 0)
(defvar *locations-on-table* nil)
(defvar *locations-on-counter* nil)

(defvar *location-to-see-table* nil) 
(defvar *location-to-see-counter* nil)

(defvar *gap-object-robot* 0.5)
(defvar *gap-between-objects* 0.15)

(defun init-localize ()
  (setf *location-to-see-table* (make-pose '(0 0 0)
                                           '(0 0 0 1)))
  (setf *location-to-see-counter* (make-pose '(0 0 0)
                                             '(0 0 0 1)))) 

(defun reference (loc)
  (cond
    ;; Location to reach something
    ((eql (desig-prop-value loc 'to) 'reach)
     (let* ((loc2 (desig-prop-value loc 'loc))
            (pose (reference loc2)))
       (make-pose `(,(- (cl-tf:x pose) *gap-object-robot*) ,(cl-tf:y pose) 0)
                  '(0 0 0 1))))
    ;; Location to place something
    ((eql (desig-prop-value loc 'to) 'place)
     (let* ((pose (desig-prop-value loc 'pose)))
       (make-pose `(,(+ (cl-tf:x pose) *gap-object-robot*) ,(cl-tf:y pose) 0)
                  '(0 0 0 1))))
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
               *locations-on-counter*)))))
    ;; Someone fucked up
    (t loc)))

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
               *locations-on-counter*)))))))

(defun make-pose (vector quaternion)
  (tf:make-pose-stamped "/map" 0.0
                        (tf:make-3d-vector (nth 0 vector)
                                           (nth 1 vector)
                                           (nth 2 vector))
                        (tf:make-quaternion (nth 0 quaternion)
                                            (nth 1 quaternion)
                                            (nth 2 quaternion)
                                            (nth 3 quaternion))))

(defun generate-locations-on (name)
  (let* ((obj (get-furniture name))
         (dims (desig-prop-value obj 'dimensions))
         (coords (get-coords obj))
         (x (first coords))
         (y (second coords))
         (z (+ (third coords) (/ (third dims) 2)))
         (locs (if (equal name *table-name*)
                   *locations-on-table*
                   *locations-on-counter*)))
    (push (make-pose `(,x ,(+ y *gap-between-objects*) ,z) '(0 0 0 1)) locs)
    (push (make-pose `(,x ,(- y *gap-between-objects*) ,z) '(0 0 0 1)) locs)
    (push (make-pose `(,x ,y ,z) '(0 0 0 1)) locs)))
    

(defun get-furniture (name)
  (let ((gen (json-prolog:prolog-simple-1 (format nil
                                                  "getKnowrobDimension('~a',Out)"
                                                  name))))
     (suturo-planning-common::json-prolog->short-designator gen)))
    
    