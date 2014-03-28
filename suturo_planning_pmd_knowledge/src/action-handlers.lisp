(in-package :suturo-planning-pmd-knowledge)

(defvar *update-semantic-map-fails* 0)
(defvar *get-boxes-fails* 0)
(defvar *get-objects-fails* 0)
(defvar *determine-box-for-object-fails* 0)

;;dlink
(defparameter *test-loc1* 
  (make-designator 'location `((coords (0.60 -0.40 0.68))
                               (pose (-0.5 0.5 -0.5 0.5))
                               (frame "/map"))))
(defparameter *test-obj1* 
  (make-designator 'object `((name "http://www.suturo.de/ontology/hierarchy#dlink'")
                             (grip-force 12)
                             (edible t)
                             (dim (0.143 0.145 0.036))
                             (at ,*test-loc1*))))

;;cafet
(defparameter *test-loc2* 
  (make-designator 'location `((coords (0.60 -0.40 0.68))
                               (pose (-0.5 0.5 -0.5 0.5))
                               (frame "/map"))))
(defparameter *test-obj2* 
  (make-designator 'object `((name "http://www.suturo.de/ontology/hierarchy#corny'")
                             (grip-force 12)
                             (edible t)
                             (dim (0.143 0.145 0.036))
                             (at ,*test-loc2*))))

;;corny
(defparameter *test-loc3* 
  (make-designator 'location `((coords (0.65 0 0.93))
                               (pose (-0.5 0.5 -0.5 0.5))
                               (frame "/map"))))
(defparameter *test-obj3* 
  (make-designator 'object `((name "http://www.suturo.de/ontology/hierarchy#corny'")
                             (grip-force 12)
                             (edible t)
                             (dim (0.143 0.145 0.036))
                             (at ,*test-loc3*))))

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  nil)

(def-action-handler update-objects-on (object-name)
  (declare (ignore object-name))
  nil)

(def-action-handler placed-object-in-box (object box)
  (declare (ignore object box))
  nil)

(def-action-handler get-objects-with-properties (object props)
  (declare (ignore object props))
  `(,*test-obj1* ,*test-obj2* ,*test-obj3*))
      
