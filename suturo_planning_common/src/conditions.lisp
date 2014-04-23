(in-package :suturo-planning-common)

;;; Conditions thrown by plans

(define-condition monitor-not-started (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "monitor-not-started"))

(define-condition ambiguous-description (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "ambiguous-description"))

(define-condition no-object-with-that-description (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "no-object-with-that-description"))

(define-condition objs-in-on-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "objs-in-on-failed"))

;;; Conditions thrown by manipulation

;; The robot couldn't grasp the object
(define-condition grasping-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "grasping-failed"))

;; The robot couldn't take the pose
(define-condition pose-not-reached (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "pose-not-reached"))

;; The given body part isn't specified or can't be used
(define-condition unhandled-body-part (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "unhandled-body-part"))

;; The given object is not being hold by any gripper.
(define-condition object-not-hold-by-any-gripper (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "object-not-hold-by-any-gripper"))

;; The given action isn't specified or can't be used
(define-condition unhandled-grasp-action (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "unhandled-grasp-action"))

;; The head of the robot couldn't be moved
(define-condition move-head-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "move-head-failed"))

;; The arm couldn't be moved
(define-condition move-arm-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "move-arm-failed"))

;; The base coudn't be moved
(define-condition move-base-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "move-base-failed"))

;; Failed to drop the object
(define-condition drop-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "drop-failed"))

;; Dropped the object
(define-condition dropped-object (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "dropped object"))

;; Couldn't reach the given location
(define-condition location-not-reached (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "location-not-reached"))

;; Failed to place a object on something
(define-condition place-failed (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "place-failed"))

(define-condition unhandled-action-answer (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "unhandled-action-answer"))

(define-condition unhandled-value (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "unhandled-value"))


;;; Conditions thrown by knowledge


;;; Misc

(define-condition unhandled-condition (simple-plan-failure)
  ((result :initarg :result :reader result :initform nil))
  (:default-initargs :format-control "unhandled-condition"))










