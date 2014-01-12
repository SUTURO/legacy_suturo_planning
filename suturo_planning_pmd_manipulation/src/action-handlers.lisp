(in-package :suturo-planning-pmd-manipulation)

(defvar *move-fails* 0)
(defvar *move-head-fails* 0)
(defvar *move-arm-fails* 0)
(defvar *grasp-fails* 2)
(defvar *open-fails* 0)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler take-pose(pose)
  (if (= *move-fails* 1) 
      (setq *move-fails* 0) 
      (progn 
        (setq *move-fails* (+ *move-fails* 1))
        (cpl:error 'suturo-planning-common::pose-not-reached :result pose))))

(def-action-handler move-head(direction)
  (if (= *move-head-fails* 1)
      (setq *move-head-fails* 0) 
      (progn
        (setq *move-head-fails* (+ *move-head-fails* 1))
        (cpl:error 'suturo-planning-common::move-head-failed :result direction)))) 

(def-action-handler grasp(obj arm)
  (if (= *grasp-fails* 0)
      (progn
        (setq *grasp-fails* (+ *grasp-fails* 1))
        (cpl:error 'suturo-planning-common::grasping-failed :result obj)))  
  (if (= *grasp-fails* 1)
      (progn 
        (setq *grasp-fails* (+ *grasp-fails* 1))
        (cpl:error 'suturo-planning-common::grasping-failed :result obj)))
  (if (= *grasp-fails* 2)
      (progn
        (setq *grasp-fails* 2)
        (let* ((loc-des (description (desig-prop-value obj 'at)))
               (loc (make-designator 'location 
                                     (update-designator-properties '((in left-gripper))
                                                                   loc-des)))
               (new-obj (make-designator 'object
                                         (update-designator-properties `((at ,loc))
                                                                       (description obj)))))
        (equate obj new-obj))
        (format t "First-desig: ~a~%Current-desig: ~a~%" (first-desig obj) (current-desig obj)))))

(def-action-handler open-hand(arm)
  (if (= *open-fails* 1)
      (setq *open-fails* 0)
      (progn
        (setq *open-fails* (+ *open-fails* 1))
        (cpl:error 'suturo-planning-common::drop-failed :result arm))))

(def-action-handler move-arm(location arm)
  (if (= *move-arm-fails* 0)
      (progn
        (setq *move-arm-fails* (+ *move-arm-fails* 1))
        (cpl:error 'suturo-planning-common::location-not-reached :result arm))) 
  (if (= *move-arm-fails* 1)
      (progn 
        (setq *move-arm-fails* (+ *move-arm-fails* 1))
        (cpl:error 'suturo-planning-common::location-not-reached :result arm))) 
  (if (= *move-arm-fails* 2)
        (setq *move-arm-fails* 0)))

(def-action-handler keep-object-in-hand (arm))

(def-action-handler gripper-is-closed (arm))