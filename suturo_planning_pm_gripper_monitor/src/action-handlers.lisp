(in-package :suturo-planning-pm-gripper-monitor)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(defvar *gripper-state* nil) ;(make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(def-action-handler start-monitoring-gripper (arm)
  "Subscribes to joint_states an monitors the state of the gripper"
  (if (not *joint-state-subscriber*)
      (setf *joint-state-subscriber* 
            (roslisp:subscribe "/joint_states" 
                       "sensor_msgs/JointState" 
                       #'(lambda (state) (monitor-grip arm state))))))

(def-action-handler end-monitoring-gripper ()
  "Unsubscribes from joint_states"
  (if *joint-state-subscriber*
      (roslisp:unsubscribe *joint-state-subscriber*))
  (setf *joint-state-subscriber* nil))

(def-action-handler gripper-is-closed ()
  "Checks if the gripper is closed"
  (if (not *joint-state-subscriber*)
      (cpl:fail 'suturo-planning-common::monitor-not-started))
  (is-closed *gripper-state*))

(def-action-handler monitor-gripper (arm)
  "Checks if the gripper is closed, starts a subscriber if necessary"
  (when (not *joint-state-subscriber*)
    (call-action 'start-monitoring-gripper arm)
    (sleep 1)
    (call-action 'end-monitoring-gripper))
  (is-closed *gripper-state*))

(def-action-handler get-gripper-state (arm)
  "Returns the state of the gripper joint, starts a subscriber if necessary"
  (when (not *joint-state-subscriber*)
    (call-action 'start-monitoring-gripper arm)
    (sleep 1)
    (call-action 'end-monitoring-gripper))
  *gripper-state*)

(defun monitor-grip (arm state)
  "Extracts the gripper position from the ros-message"
  (let ((side nil) (gripper-pos))
    (if (eql arm 'left-arm)
        (setf side "l")
        (setf side "r"))
    (roslisp:with-fields ((names name) (positions position))
        state
      (map nil (lambda (name position)
                 (if (string= name 
                              (concatenate 'string
                                           side
                                           "_gripper_joint"))
                     (setf gripper-pos position)))
                 names positions))
    (setf *gripper-state* gripper-pos)))

(defun is-closed (pos)
   (< pos 5.0d-7))


  

