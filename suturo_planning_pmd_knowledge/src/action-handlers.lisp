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
      (prog1 (vector 1 2 3)
        (setq *update-semantic-map-fails* 0))
      (prog1 nil
        (setq *update-semantic-map-fails* (+ *update-semantic-map-fails* 1)))))

(def-action-handler placed-object-in-box (object box)
  nil)

(def-action-handler get-graspable-objects ()
  nil)

(def-action-handler get-container-objects ()
  nil)
      