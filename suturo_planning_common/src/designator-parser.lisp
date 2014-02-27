(in-package :suturo-planning-common)

; get location designator if present
;   sort desig-props
;   if desig-prop-name is coords
;     replace desig-prop-name -> at
;   if desig-prop-value is not symbol
;     add to string -> desig-prop-name Object
;     add to param list -> desig-prop-value
;   else
;     add to string -> desig-prop-name desig-prop-value
; if designator has other properties than location
;   add to string -> With
;   sort desig-props
;   add to string -> desig-prop-name
;   add to param list -> desig-prop-value


(defun desig->string (desig)
  (let ((function-name '())
        (parameter-list '()))
    (if (not (null (desig-prop-value desig 'at)))
      (let ((location-desig (desig-prop-value desig 'at)))
        (mapcar (lambda (k)
                  (setq function-name (append function-name `(,(symbol-name k))))
                  (if (eq (type-of (desig-prop-value location-desig k)) 'SYMBOL)
                    (setq function-name (append function-name `(,(symbol-name (desig-prop-value location-desig k)))))
                    (prog2
                      (if (eq (type-of (desig-prop-value location-desig k)) 'OBJECT-DESIGNATOR)
                        (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value (desig-prop-value location-desig k) 'NAME)))))
                        (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value location-desig k))))))
                      (setq function-name (append function-name '("OBJECT"))))))
                (remove-if (lambda (e) (eq e 'frame))
                           (sorted-description location-desig)))
        (list (list->camel function-name) parameter-list)))
    (if (not (null (remove-if (lambda (j) (eq j 'at)) (sorted-description desig))))
      (let ((other-props (remove-if (lambda (i) (eq i 'at)) (sorted-description desig))))
        (setq function-name (append function-name '("WITH")))
        (mapcar (lambda (l)
                  (setq function-name (append function-name `(,(symbol-name l))))
                  (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value desig l))))))
                other-props)))
    (setq parameter-list (append parameter-list '("Out")))
    (concatenate 'string (list->camel function-name) (list->params parameter-list))))

(defun param->string (param)
  (if (not param)
    "false"
    (if (eq (type-of param) 'BOOLEAN)
      "true"
      (write-to-string param))))

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
    (if ON
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

(defun sorted-description (desig)
  (mapcar #'first 
          (sort (copy-list (description desig)) 
                (lambda (e1 e2)
                  (string< (symbol-name (first e1))
                           (symbol-name (first e2)))))))

(defun symbol->string (s)
  (let ((str (symbol-name s)))
    (subseq str 1 (- (length str) 1))))

(defun match-pattern (pattern value)
  "Creates a list of name value pairs, to be used as first argument for a let block"
  (let ((res nil))
    (if (eq (type-of pattern) 'SYMBOL)
      (setq res (append res `((,pattern ,value))))
      (if (and (eq (type-of pattern) (type-of value)) (eq (type-of pattern) 'CONS))
        (mapcar (lambda (p v)
                  (setq res (append res (match-pattern p v))))
                pattern value)))
    res))

(defmacro bind-pattern (pattern value &body body)
  "Executes body with the bindings defined by pattern and value"
  (let ((bindings (mapcar (lambda (e)
                            (if (or (eq (type-of (second e)) 'CONS) (eq (type-of (second e)) 'SYMBOL))
                              `(,(first e) (quote ,(second e)))
                              e)) (match-pattern pattern value))))
    `(let ,bindings
      ,@body)))

(defun json-prolog->designators (jj)
  "Converts a JSON-Prolog return value to object designators"
  (mapcar (lambda (e)
    (eval `(bind-pattern (edible name centroid frame-id grip-force use dimensions) ,e
      (make-designator 'object `((edible ,(equal "true" (symbol->string `,edible)))
                                 (name ,(symbol->string `,name))
                                 (use ,(if (equal (symbol->string `,use) "storage-for-food") 'storage-for-food 'storage-for-stuff))
                                 (grip-force ,grip-force)
                                 (at ,(make-designator 'location `((coords ,centroid)
                                                                   (frame ,(symbol->string `,frame-id)))))
                                 (dimensions ,dimensions))))))
    (subseq (first (first jj)) 1)))
