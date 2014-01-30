(in-package :suturo-planning-pm-gripper-monitor)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(defvar *gripper-state-left* nil) ;(make-fluent :value nil))
(defvar *gripper-state-right* nil)

(defvar *joint-state-subscriber* nil)

(def-action-handler start-monitoring-gripper ()
  "Subscribes to joint_states an monitors the state of the gripper"
  (if (not *joint-state-subscriber*)
      (setf *joint-state-subscriber* 
            (roslisp:subscribe "/joint_states" 
                       "sensor_msgs/JointState" 
                       #'(lambda (state) (monitor-grip state)))))
  t)

(def-action-handler end-monitoring-gripper ()
  "Unsubscribes from joint_states"
  (if *joint-state-subscriber*
      (roslisp:unsubscribe *joint-state-subscriber*))
  (setf *joint-state-subscriber* nil))

(def-action-handler gripper-is-closed (arm)
  "Checks if the gripper is closed"
  (if (not *joint-state-subscriber*)
      (cpl:fail 'suturo-planning-common::monitor-not-started))
  (format t "~a~%" *gripper-state-left*)
  (if (eql arm 'left-arm) 
      (is-closed *gripper-state-left*)
      (is-closed *gripper-state-right*)))

(def-action-handler monitor-gripper (arm)
  "Checks if the gripper is closed, starts a subscriber if necessary"
  (when (not *joint-state-subscriber*)
    (call-action 'start-monitoring-gripper)
    (sleep 1)
    (call-action 'end-monitoring-gripper))
  (if (eql arm 'left-arm) 
      (is-closed *gripper-state-left*)
      (is-closed *gripper-state-right*)))

(def-action-handler get-gripper-state (arm)
  "Returns the state of the gripper joint, starts a subscriber if necessary"
  (when (not *joint-state-subscriber*)
    (call-action 'start-monitoring-gripper)
    (sleep 1)
    (call-action 'end-monitoring-gripper))
  (if (eql arm 'left-arm) 
      *gripper-state-left*
      *gripper-state-right*))

(defun monitor-grip (state)
  "Extracts the gripper position from the ros-message"
  (roslisp:with-fields ((names name) (positions position))
      state
      (map nil (lambda (name position)
                 (if (equal name "l_gripper_joint")
                     (setf *gripper-state-left* position))
                 (if (equal name "r_gripper_joint")
                     (setf *gripper-state-right* position)))
           names positions)))

(defun is-closed (pos)
   (< pos 5.0d-7))


  

