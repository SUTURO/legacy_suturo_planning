(in-package :suturo-planning-pmd-knowledge)

(defvar *update-semantic-map-fails* 0)
(defvar *get-boxes-fails* 0)
(defvar *get-objects-fails* 0)
(defvar *determine-box-for-object-fails* 0)

;;cafet-filter
(defvar *test-loc1* (make-designator 'location `((coords (0.60 0.40 0.70))
                                                 (frame ,"odom_combined"))))
(defvar *test-obj1* (make-designator 'object `((name ,"cafetfilter")
                                               (grip-force 12)
                                               (at ,*test-loc1*))))

;;corny
(defvar *test-loc2* (make-designator 'location `((coords (0.60 0 0.70))
                                                 (frame ,"odom_combined"))))
(defvar *test-obj2* (make-designator 'object `((name ,"corny")
                                               (grip-force 12)
                                               (at ,*test-loc2*))))

;;dlink
(defvar *test-loc3* (make-designator 'location `((coords (0.60 -0.40 0.68))
                                                 (frame ,"odom_combined"))))
(defvar *test-obj3* (make-designator 'object `((name ,"dlink")
                                               (grip-force 12)
                                               (at ,*test-loc3*))))

;;red box
(defvar *test-loc4* (make-designator 'location `((coords (0.60 -0.80 0.68))
                                                 (frame ,"odom_combined"))))
(defvar *test-obj4* (make-designator 'object `((name ,"redbox")
                                               (grip-force 12)
                                               (use storage-for-stuff)
                                               (at ,*test-loc4*))))

;;white box
(defvar *test-loc5* (make-designator 'location `((coords (0.60 0.80 0.68))
                                                 (frame ,"odom_combined"))))
(defvar *test-obj5* (make-designator 'object `((name ,"whitebox")
                                               (grip-force 12)
                                               (use storage-for-food)
                                               (at ,*test-loc5*))))



(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler update-semantic-map ()
  nil)

(def-action-handler placed-object-in-box (object box)
  (declare (ignore object box))
  nil)

(def-action-handler get-graspable-objects ()
  `(,*test-obj1* ,*test-obj2* ,*test-obj3*))

(def-action-handler get-container-objects ()
  `(,*test-obj4* ,*test-obj5*))
      