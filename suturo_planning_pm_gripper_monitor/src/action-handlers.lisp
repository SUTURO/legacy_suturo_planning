(in-package :suturo-planning-pm-gripper-monitor)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(defvar *gripper-closed* nil) ;(make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(def-action-handler start-monitoring-gripper (arm)
  "Subscribes to joint_states an monitors the state of the gripper"
  (if (not *joint-state-subscriber*)
      (setf *joint-state-subscriber* 
            (roslisp:subscribe "/joint_states" 
                       "sensor_msgs/JointState" 
                       #'(lambda (state) (monitor-grip arm state))))))

(def-action-handler gripper-is-closed ()
  "Checks if the gripper is closed"
  (if (not *joint-state-subscriber*)
      (cpl:fail 'suturo-planning-common::monitor-not-started))
  *gripper-closed*)

(def-action-handler monitor-gripper (arm)
  "Checks if the gripper is closed, starts subscriber if necessary"
  (when (not *joint-state-subscriber*)
    (call-action 'start-monitoring-gripper arm)
    (sleep 1)
    (call-action 'end-monitoring-gripper))
  *gripper-closed*)

(def-action-handler end-monitoring-gripper ()
  (if *joint-state-subscriber*
      (roslisp:unsubscribe *joint-state-subscriber*))
  (setf *joint-state-subscriber* nil))

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
    (format t "Gripper joint: ~a~%" gripper-pos)
    (if (is-closed gripper-pos)
        (setf *gripper-closed* t)
        (setf *gripper-closed* nil))))

(defun is-closed (pos)
   (< pos 5.0d-7))


  

