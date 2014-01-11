(in-package :suturo-planning-pm-manipulation)

(defvar *gripper-closed* nil) ;(make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(def-action-handler keep-object-in-hand (arm)
  "Subscribes to joint_states an monitors the state of the gripper"
  (subscribe-joint-state arm))
  ;(whenever ((pulsed *gripper-closed*))
  ;  (if (value *gripper-closed*)
  ;      (progn
  ;        (unsubscribe-joint-state)
  ;        (cpl:fail 'suturo-planning-common::grasp-fail)))))

(def-action-handler gripper-is-closed (arm)
  "Checks if the gripper is closed"
  (subscribe-joint-state arm)
  (sleep 1)
  (unsubscribe-joint-state))
  ;(value *gripper-closed*))

(defun subscribe-joint-state (arm)
  (if (not *joint-state-subscriber*)
      (setf *joint-state-subscriber* 
            (subscribe "/joint_states" 
                       "sensor_msgs/JointState" 
                       #'(lambda (state) (monitor-grip arm state))))))

(defun monitor-grip (arm state)
  "Checks if the gripper is closed"
  (let ((side nil) (left-finger nil) (right-finger nil))
    (if (eql arm 'left-arm)
        (setf side "l")
        (setf side "r"))
    (roslisp:with-fields ((names name) (positions position))
        state
      (map nil (lambda (name position)
                 (if (string= name 
                              (concatenate 'string
                                           side
                                           "_gripper_l_finger_joint"))
                     (setf left-finger position))
                 (if (string= name 
                              (concatenate 'string
                                           side
                                           "_gripper_r_finger_joint"))
                       (setf right-finger position)))
           names positions))
    (if (and (is-closed left-finger) 
             (is-closed right-finger))
        (setf (value *gripper-closed*) t)
        (setf (value *gripper-closed*) nil))))

(defun is-closed (pos)
   (< pos 0.002100000000000000d0))

(defun unsubscribe-joint-state ()
  (if (not *joint-state-subscriber*)
      (unsubscribe *joint-state-subscriber*))
  (setf *joint-state-subscriber* nil))
  