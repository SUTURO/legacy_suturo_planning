(in-package :suturo-planning-common)

(define-condition food-overflow (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "food-overflow"))

(define-condition no-food-found (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-food-found"))

(define-condition touch-failed (simple-plan-failure)
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
