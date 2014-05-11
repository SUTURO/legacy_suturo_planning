(in-package :suturo-planning-common)

(defmacro append-to-list (l elem)
  `(setq ,l (append ,l ,elem)))

(defmacro bind-pattern (pattern value &body body)
  "Executes body with the bindings defined by pattern and value"
  (let ((bindings (mapcar (lambda (e)
                            (if (or (eq (type-of (second e)) 'CONS) (eq (type-of (second e)) 'SYMBOL))
                              `(,(first e) (quote ,(second e)))
                              e)) (match-pattern pattern value))))
    `(let ,bindings
      ,@body)))

(defun list->camel (strings)
  "Concatenates a list of strings as camelCase; Thanks Jan!"
  (nstring-downcase (apply #'concatenate 'string (mapcar #'string-capitalize strings)) :end 1))

(defun list->params (strings)
  "Concatenates a list of strings to a Prolog parameter list string"
  (format nil "(~{~A~^, ~})" strings))

(defun param->string (param)
  (cond 
    ((not param) "false")
    ((eq (type-of param) 'BOOLEAN) "true")
    ((eq (type-of param) 'CONS) (format nil "[~{~A~^, ~}]" (mapcar #'param->string param)))
    ((eq (type-of param) 'SYMBOL) (write-to-string (symbol-name param)))
    (t (write-to-string param))))

(defun sorted-description (desig)
  (mapcar #'first 
          (sort (copy-list (description desig)) 
                (lambda (e1 e2)
                  (string< (symbol-name (first e1))
                           (symbol-name (first e2)))))))

(defun match-pattern (pattern value)
  "Creates a list of name value pairs, to be used as first argument for a let block"
  (let ((res '()))
    (if (eq (type-of pattern) 'SYMBOL)
      (append-to-list res `((,pattern ,value)))
      (if (and (eq (type-of pattern) (type-of value)) (eq (type-of pattern) 'CONS))
        (mapcar (lambda (p v)
                  (append-to-list res (match-pattern p v)))
                pattern value)))
    res))

(defun designator->string (desig &optional fields)
  (let ((function-name '())
        (parameter-list '())
        (descr (if fields fields (sorted-description desig))))
    (if (not (null (desig-prop-value desig 'at)))
      (let ((location-desig (desig-prop-value desig 'at)))
        (append-to-list descr (sorted-description location-desig))
        (mapcar (lambda (k)
                  (if (member k descr)
                    (progn
                      (append-to-list function-name `(,(cl-ppcre:regex-replace-all "-" (symbol-name k) "")))
                      (cond
                        ((eq (type-of (desig-prop-value location-desig k)) 'SYMBOL)
                          (append-to-list function-name `(,(symbol-name (desig-prop-value location-desig k)))))
                        ((eq (type-of (desig-prop-value location-desig k)) 'OBJECT-DESIGNATOR)
                          (prog2
                            (append-to-list parameter-list `(,(param->string (desig-prop-value (desig-prop-value location-desig k) 'NAME))))
                            (append-to-list function-name '("OBJECT"))))
                        (t (append-to-list parameter-list `(,(param->string (desig-prop-value location-desig k)))))))))
                (remove-if (lambda (e) (or (eq e 'frame) (eq e 'name)))
                           (sorted-description location-desig)))))
    (if (not (null (remove-if (lambda (j) (eq j 'at)) (sorted-description desig))))
      (let ((other-props (remove-if (lambda (i) (eq i 'at)) (sorted-description desig)))
            (fn-name '("WITH"))
            (pm-list '()))
        (mapcar (lambda (l)
                  (if (member l descr)
                    (progn
                      (append-to-list fn-name `(,(cl-ppcre:regex-replace-all "-" (symbol-name l) "")))
                      (append-to-list pm-list `(,(param->string (desig-prop-value desig l)))))))
                other-props)
        (if (> (length fn-name) 1)
          (progn
            (append-to-list function-name fn-name)
            (append-to-list parameter-list pm-list)))))
    (append-to-list parameter-list '("Out"))
    (concatenate 'string (list->camel function-name) (cl-ppcre:regex-replace-all "\"" (list->params parameter-list) "'"))))

(defun parse-symbol-name (s)
  (let ((ss (symbol-name s)))
    (subseq ss 1 (- (length ss) 1))))

(defun list->designator (jj)
  (format t "~a~%" jj)
  (eval `(bind-pattern (edible name centroid frame-id grip-force unknown use dimensions pose) ,jj
    (make-designator 'object `((edible ,(equal "true" (parse-symbol-name `,edible)))
                                 (name ,(parse-symbol-name `,name))
                                 (grip-force ,grip-force)
                                 (unknown ,(equal "true" (parse-symbol-name `,unknown)))
                                 (use ,(if (equal (parse-symbol-name `,use) "storage-for-food") 'storage-for-food 'storage-for-stuff))
                                 (at ,(make-designator 'location `((coords ,centroid)
                                                                   (frame ,(parse-symbol-name `,frame-id))
                                                                   (pose ,pose))))
                                 (dimensions ,dimensions))))))

(defun json-prolog->designators (jj)
  "Converts a JSON-Prolog return value to object designators"
  (mapcar #'list->designator (subseq (first (first jj)) 1)))

(defun json-prolog->short-designators (jj)
  "Backwards compatibility function for json-prolog->short-designator"
  `(,(json-prolog->short-designator jj)))

(defun json-prolog->short-designator (jj)
  "Converts a JSON-Prolog return value to object designators"
  (format t "jj: ~a~%" jj)
  (let ((e (subseq (first (first jj)) 1)))
    (eval `(bind-pattern (centroid dimensions) ,e
      (make-designator 'object `((at ,(make-designator 'location `((coords ,centroid)
                                                                   (frame "/map"))))
                                 (dimensions ,dimensions)))))))
