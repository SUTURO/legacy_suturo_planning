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

(define-condition move-head-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "move-head-failed"))

(define-condition grasp-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "grasp-failed"))

(define-condition drop-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "drop-failed"))

(define-condition location-not-reached (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "location-not-reached"))

(defun list->camel (strings)
  "Concatenates a list of strings as camelCase; Thanks Jan!"
  (nstring-downcase (apply #'concatenate 'string (mapcar #'string-capitalize strings)) :end 1))

(defun list->params (strings)
  "Concatenates a list of strings to a Prolog parameter list string"
  (let ((result ""))
    (dolist (e strings)
      (setq result (concatenate 'string result
                                (element->string e) ",")))
    (concatenate 'string "("
                 (subseq result 0 (- (length result) 1))
                 ")")))

(defun designator->string (desig)
  (if (eq (type-of desig) 'LOCATION-DESIGNATOR)
    (let ((STRS (location-designator->string desig)))
      (format nil "~a~a" (list->camel (first STRS)) (list->params (second STRS))))
    (if (eq (type-of desig) 'OBJECT-DESIGNATOR)
      (let ((STRS (object-designator->string desig)))
        (format nil "~a~a" (list->camel (first STRS)) (list->params (second STRS)))))))

(defun object-designator->string (desig)
  (let* ((TYP (desig-prop-value desig 'TYPE))
        (LOC (desig-prop-value desig 'LOC))
        (L (location-designator->string LOC))
        (FUNCNAME (first L))
        (PARAMS (second L)))
    (if (not (null TYP))
      (push (format nil "~a" TYP) FUNCNAME))
    `(,FUNCNAME ,PARAMS)))

(defun location-designator->string (desig)
  (let ((FUNCNAME '())
        (PARAMS '())
        (ON (desig-prop-value desig 'ON))
        (IN (desig-prop-value desig 'IN))
        (LOC (desig-prop-value desig 'LOC))
        (BETWEEN (desig-prop-value desig 'BETWEEN)))
    (if (not (null ON))
        (push "on" FUNCNAME))
    (if (eq (type-of ON) 'SYMBOL)
      (push (format nil "~a" ON) FUNCNAME)
      (if (eq (type-of ON) 'OBJECT-DESIGNATOR)
        (let ((TYP (desig-prop-value ON 'TYPE))
              (NAME (desig-prop-value ON 'NAME)))
          (push (format nil "~a" NAME) PARAMS))))
    (if (not (null IN))
        (push "in" FUNCNAME))
    (if (eq (type-of IN) 'SYMBOL)
      (push (format nil "~a" IN) FUNCNAME)
      (if (eq (type-of IN) 'OBJECT-DESIGNATOR)
        (let ((TYP (desig-prop-value IN 'TYPE))
              (NAME (desig-prop-value IN 'NAME)))
          (push (format nil "~a" TYP) FUNCNAME)
          (push (format nil "~a" NAME) PARAMS))))
    (if (not (null LOC))
      (push "at" FUNCNAME))
    (if (not (null BETWEEN))
      (push (format nil "between") FUNCNAME))
    `(,(reverse FUNCNAME) ,(reverse (push "Obj" PARAMS)))))

 (defun string->designators (str)
   "Converts a return string with format
    '[[edible,[centroid_x, centroid_y, centroid_z],frame_id],[...]]'
   to an object designator"
   (let ((result '())
         (regex "\\\[(true|false),'(\\w+)',\\\[([\\d.]+),([\\d.]+),([\\d.]+)\\\],([\\d.]+),'(\\w+)',\\\[([\\d.]+),([\\d.]+),([\\d.]+)\\\],([\\d.]+),([\\d.]+)\\\],?"))
     (ppcre:do-register-groups (edible name centroid_x centroid_y centroid_z
                                volume frame_id origin_x origin_y origin_z
                                width height)
                               (regex str nil :start 0 :end (length str) :sharedp t)
                               (let ((loc (make-designator 'location `((desig-props:loc (,(read-from-string centroid_x) 
                                                                                         ,(read-from-string centroid_y) 
                                                                                         ,(read-from-string centroid_z)))
                                                                       (desig-props:frame ,frame_id)))))
                                 (push (make-designator 'object `((desig-props:edible ,(string-equal edible "true"))
                                                            (desig-props:name ,name)
                                                            (desig-props:at ,loc))) result)))
     result))
