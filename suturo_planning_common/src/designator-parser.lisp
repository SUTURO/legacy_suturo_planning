(in-package :suturo-planning-common)

(defun designator->string (desig)
  (let ((function-name '())
        (parameter-list '()))
    (if (not (null (desig-prop-value desig 'at)))
      (let ((location-desig (desig-prop-value desig 'at)))
        (mapcar (lambda (k)
                  (setq function-name (append function-name `(,(cl-ppcre:regex-replace-all "-" (symbol-name k) ""))))
                  (cond
                    ((eq (type-of (desig-prop-value location-desig k)) 'SYMBOL)
                      (setq function-name (append function-name `(,(symbol-name (desig-prop-value location-desig k))))))
                    ((eq (type-of (desig-prop-value location-desig k)) 'OBJECT-DESIGNATOR)
                      (prog2
                        (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value (desig-prop-value location-desig k) 'NAME)))))
                        (setq function-name (append function-name '("OBJECT")))))
                    (t (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value location-desig k))))))))
                (remove-if (lambda (e) (eq e 'frame))
                           (sorted-description location-desig)))))
    (if (not (null (remove-if (lambda (j) (eq j 'at)) (sorted-description desig))))
      (let ((other-props (remove-if (lambda (i) (eq i 'at)) (sorted-description desig))))
        (setq function-name (append function-name '("WITH")))
        (mapcar (lambda (l)
                  (setq function-name (append function-name `(,(cl-ppcre:regex-replace-all "-" (symbol-name l) ""))))
                  (setq parameter-list (append parameter-list `(,(param->string (desig-prop-value desig l))))))
                other-props)))
    (setq parameter-list (append parameter-list '("Out")))
    (concatenate 'string (list->camel function-name) (list->params parameter-list))))

(defun param->string (param)
  (cond 
    ((not param) "false")
    ((eq (type-of param) 'BOOLEAN) "true")
    ((eq (type-of param) 'CONS) (format nil "[~{~A~^, ~}]" (mapcar #'write-to-string param)))
    ((eq (type-of param) 'SYMBOL) (write-to-string (symbol-name param)))
    (t (write-to-string param))))

(defun list->camel (strings)
  "Concatenates a list of strings as camelCase; Thanks Jan!"
  (nstring-downcase (apply #'concatenate 'string (mapcar #'string-capitalize strings)) :end 1))

(defun list->params (strings)
  "Concatenates a list of strings to a Prolog parameter list string"
  (format nil "(~{~A~^, ~})" strings))

(defun sorted-description (desig)
  (mapcar #'first 
          (sort (copy-list (description desig)) 
                (lambda (e1 e2)
                  (string< (symbol-name (first e1))
                           (symbol-name (first e2)))))))

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
    (eval `(bind-pattern (edible name centroid frame-id grip-force use dimensions pose) ,e
      (make-designator 'object `((edible ,(equal "true" (symbol-name `,edible)))
                                 (name ,(symbol-name `,name))
                                 (use ,(if (equal (symbol-name `,use) "storage-for-food") 'storage-for-food 'storage-for-stuff))
                                 (grip-force ,grip-force)
                                 (at ,(make-designator 'location `((coords ,centroid)
                                                                   (frame ,(symbol-name `,frame-id))
                                                                   (pose ,pose))))
                                 (dimensions ,dimensions))))))
    (subseq (first (first jj)) 1)))
