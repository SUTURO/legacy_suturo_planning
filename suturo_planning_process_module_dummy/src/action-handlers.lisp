(in-package :suturo-planning-process-module-dummy)

(defvar *perceive-fails* 0)
(defvar *ground-fails* 0)
(defvar *move-fails* 0)
(defvar *touch-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler perceive ()
  (if (= *perceive-fails* 1)
      (progn
        (setq *perceive-fails* 0)
        (vector 1 2 3))
      (progn
        (setq *perceive-fails* (+ *perceive-fails* 1))
        NIL)))

(def-action-handler ground (objs)
   (if (= *ground-fails* 1)
      (progn
        (setq *ground-fails* 0)
        (list 1 2))
      (progn
        (setq *ground-fails* (+ *ground-fails* 1))
        (list))))

(def-action-handler move (pose)
   (if (= *move-fails* 1)
       (setq *move-fails* 0)
      (progn
        (setq *move-fails* (+ *move-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result pose))))

(def-action-handler touch (arm obj)
   (if (= *touch-fails* 3)
       (setq *touch-fails* 0)
      (progn
        (setq *touch-fails* (+ *touch-fails* 1))
        (cpl:error 'suturo-planning-common::touch-failed :result obj))))
