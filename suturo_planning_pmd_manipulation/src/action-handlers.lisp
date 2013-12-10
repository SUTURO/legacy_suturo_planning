(in-package :suturo-planning-pmd-manipulation)

(defvar *move-fails* 0)
(defvar *move-head-fails* 0)
(defvar *take-fails* 0)
(defvar *box-move-fails* 0)
(defvar *move-closer-fails* 0)
(defvar *touch-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler move(pose)
  (if (= *move-fails* 1) 
      (setq *move-fails* 0) 
      (progn 
        (setq *move-fails* (+ *move-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result pose))))

(def-action-handler touch(arm obj)
  (if (= *touch-fails* 1)
      (setq *touch-fails* 0)
      (progn 
        (setq *touch-fails* (+ *touch-fails* 1))
        (cpl:error 'suturo-planning-common::touch-failed :result obj))))

(def-action-handler move-head(direction)
  (if (= *move-head-fails* 1)
      (setq *move-head-fails* 0) 
      (progn
        (setq *move-head-fails* (+ *move-head-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result direction)))) ;head-already-in-position

(def-action-handler take-object(arm obj)
  (if (= *take-fails* 0)
      (progn
        (setq *take-fails* (+ *take-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result obj))) ;cannot-reach-object  
  (if (= *take-fails* 1)
      (progn 
        (setq *take-fails* (+ *take-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result obj))) ;take-failed
  (if (= *take-fails* 2)
      (setq *take-fails* 0))) 

(def-action-handler move-arm-over-box(arm box)
  (if (= *box-move-fails* 0)
      (progn
        (setq *box-move-fails* (+ *box-move-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result box))) ;cannot-reach-box
  (if (= *box-move-fails* 1)
      (progn 
        (setq *box-move-fails* (+ *box-move-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result box))) ;cannot-move-over-box
  (if (= *box-move-fails* 2)
        (setq *box-move-fails* 0)))

(def-action-handler move-closer-to-other-arm(arm obj)
  (if (= *move-closer-fails* 1)
      (setq *move-closer-fails* 0)
      (progn
        (setq *move-closer-fails* (+ *move-closer-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result obj)))) ;cannot-move-obj