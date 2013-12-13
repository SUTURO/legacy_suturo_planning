(in-package :suturo-planning-pmd-knowledge)

(defvar *update-semantic-map-fails* 0)
(defvar *get-boxes-fails* 0)
(defvar *get-objects-fails* 0)
(defvar *determine-box-for-object-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  (if (= *update-semantic-map-fails* 1)
      (progn
        (setq *update-semantic-map-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *update-semantic-map-fails* (+ *update-semantic-map-fails* 1))
        NIL)))

(def-action-handler get-boxes ()
  (if (= *get-boxes-fails* 1)
      (progn
        (setq *get-boxes-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *get-boxes-fails* (+ *get-boxes-fails* 1))
        NIL)))

(def-action-handler get-objects ()
  (if (= *get-objects-fails* 1)
      (progn
        (setq *get-objects-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *get-objects-fails* (+ *get-objects-fails* 1))
        NIL)))

(def-action-handler determine-box-for-object ()
  (if (= *determine-box-for-object-fails* 1)
      (progn
        (setq *determine-box-for-object-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *determine-box-for-object-fails* (+ *determine-box-for-object-fails* 1))
        NIL)))