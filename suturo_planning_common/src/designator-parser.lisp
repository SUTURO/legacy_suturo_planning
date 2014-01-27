(in-package :suturo-planning-common)

(defun list->camel (strings)
  "Concatenates a list of strings as camelCase; Thanks Jan!"
  (nstring-downcase (apply #'concatenate 'string (mapcar #'string-capitalize strings)) :end 1))

(defun list->params (strings)
  "Concatenates a list of strings to a Prolog parameter list string"
  (let ((result ""))
    (dolist (e strings)
      (setq result (concatenate 'string result
                                (format nil "~a" e) ",")))
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
        (LOC (desig-prop-value desig 'AT))
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
          (push (format nil "'~a'" NAME) PARAMS))))
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
      (let ((x (first LOC))
            (y (second LOC))
            (z (third LOC)))
        (push "at" FUNCNAME)
        (push (format nil "[~a,~a,~a]" x y z) PARAMS)))
    (if (not (null BETWEEN))
      (push (format nil "between") FUNCNAME))
    `(,(reverse FUNCNAME) ,(reverse (push "Out" PARAMS)))))

(defun symbol->string (s)
  (let ((str (symbol-name s)))
    (subseq str 1 (- (length str) 1))))

(defun json-prolog->designators (jj)
  (let ((objs (subseq (first (first jj)) 1)))
    (mapcar (lambda (obj)
              (let* ((edible (first obj))
                     (name (second obj))
                     (centroid (third obj))
                     (frame-id (fourth obj))
                     (grip-force (fifth obj))
                     (use (sixth obj))
                     (loc (make-designator
                           'location
                           `((coords ,centroid)
                             (frame ,(symbol->string frame-id))))))
                (make-designator
                 'object
                 `((edible ,(eq 'TRUE (intern (nstring-upcase (symbol->string edible)))))
                   (name ,(symbol->string name))
                   (use ,(if (eq (symbol->string use) "edible") 'storage-for-food 'storage-for-stuff))
                   (grip-force ,grip-force)
                   (at ,loc))))) objs)))
