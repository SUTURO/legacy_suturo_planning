(in-package :suturo-planning-common)

(define-condition food-overflow (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "food-overflow"))

(define-condition no-food-found (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-food-found"))

(define-condition grasping-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "touch-failed"))

(define-condition pose-not-reached (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "pose-not-reached"))

(define-condition no-object-perceived (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-object-perceived"))

(defun designator->string (desig)
  "Returns a string containing the properties of the given designator"
  (let ((des (description desig)))
    (list->string des)))

(defun element->string (e)
  "Returns a string representation of the given parameter. If a list is given, the list is converted to a string."
  (if (listp e)
    (list->string e)
    (write-to-string e)))

(defun list->string (l)
  "Converts a given list to a string with a format of [a,b,c,...]"
  (let ((result ""))
    (dolist (e l)
      (setq result (concatenate 'string result 
                                (element->string e) ",")))
    (concatenate 'string "[" 
                 (subseq result 0 (- (length result) 1)) 
                 "]")))

;; TODO: Currently includes only edible and centroid
 (defun string->designators (str)
   "Converts a return string with format
    '[[edible,[centroid_x, centroid_y, centroid_z],volume,frame_id,[origin_x,origin_y,origin_z],width,height],[...]]'
   to an object designator"
   (let ((result '())
         (regex "\\\[(true|false),\\\[([\\d.]+),([\\d.]+),([\\d.]+)\\\],([\\d.]+),'(\\w+)',\\\[([\\d.]+),([\\d.]+),([\\d.]+)\\\],([\\d.]+),([\\d.]+)\\\],?"))
     (ppcre:do-register-groups (edible centroid_x centroid_y centroid_z
                                volume frame_id origin_x origin_y origin_z
                                width height)
                               (regex str nil :start 0 :end (length str) :sharedp t)
                               (push (make-designator 'object `((desig-props:edible ,(string-equal edible "true")) 
                                                                (at (make-designator 'location ((desig-props:loc (,(read-from-string centroid_x) 
                                                                                                                  ,(read-from-string centroid_y) 
                                                                                                                  ,(read-from-string centroid_z)))))))) 
                                     result))
     result))
