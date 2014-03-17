(in-package :suturo-planning-pmd-knowledge)

(defvar *update-semantic-map-fails* 0)
(defvar *get-boxes-fails* 0)
(defvar *get-objects-fails* 0)
(defvar *determine-box-for-object-fails* 0)

;;cafet-filter
(defvar *test-loc1* (make-designator 'location `((coords (0.60 0.40 0.70))
                                                 (frame "/map"))))
(defvar *test-obj1* (make-designator 'object `((name "cafetfilter")
                                               (grip-force 12)
                                               (at ,*test-loc1*))))

;;corny
(defvar *test-loc2* (make-designator 'location `((coords (0.60 0 0.70))
                                                 (frame "/map"))))
(defvar *test-obj2* (make-designator 'object `((name ,"corny")
                                               (grip-force 12)
                                               (at ,*test-loc2*))))

;;dlink
(defvar *test-loc3* (make-designator 'location `((coords (0.60 -0.40 0.68))
                                                 (frame "/map"))))
(defvar *test-obj3* (make-designator 'object `((name "dlink")
                                               (grip-force 12)
                                               (at ,*test-loc3*))))

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  nil)

(def-action-handler placed-object-in-box (object box)
  (declare (ignore object box))
  nil)

(def-action-handler get-objects-with-properties (object)
  `(,*test-obj1* ,*test-obj2* ,*test-obj3*))
      
