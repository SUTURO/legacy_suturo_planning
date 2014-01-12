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

(def-action-handler get-graspable-objects ()
  (if (= *get-boxes-fails* 1)
      (progn
        (setq *get-boxes-fails* 0)
        (with-designators ((loc1 (location '((frame "frame1")
                                             (coords (1 2 3)))))
                           (loc2 (location '((frame "frame1")
                                             (coords (3 4 1))))))
          (with-designators ((obj1 (object `((name "obj1")
                                             (type graspable)
                                             (edible t)
                                             (at ,loc1)))))
            `(,obj1 ,obj1 ,obj1))))
      (progn
        (setq *get-boxes-fails* (+ *get-boxes-fails* 1))
        NIL)))

(def-action-handler get-container-objects ()
  (if (= *get-objects-fails* 1)
      (progn
        (setq *get-objects-fails* 0)
        (vector 1 2 3)
        (with-designators ((loc1 (location '((frame "frame1")
                                             (coords (1 2 3)))))
                           (loc2 (location '((frame "frame1")
                                             (coords (3 4 1))))))
          (with-designators ((box1 (object `((name "box1")
                                             (type container)
                                             (at ,loc1))))
                             (box2 (object `((name "box2")
                                             (type container)
                                             (at ,loc2)))))
            `(,box1 ,box2))))
      (progn
        (setq *get-objects-fails* (+ *get-objects-fails* 1))
        NIL)))