(in-package :suturo-planning-planlib)

(defvar *gripper-closed* (make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(defun monitor-grip ()
  "Subscribes to joint_states an monitors the state of the gripper"
  (setf *joint-state-subscriber* 
        (subscribe "/joint_states" "sensor_msgs/JointState" #'check-grip)))

(defun check-grip (state)
  "Checks if the gripper is closed"
  (let ((left-finger nil) (right-finger nil))
    (roslisp:with-fields ((names name) (positions position))
        state
      (map nil (lambda (name position)
                 (format t "Name: ~a Position: ~a~%" name position)
                 (if (string= name "l_gripper_l_finger_joint")
                     (progn
                       (setf left-finger position)
                       (format t "Found left-tip position ~a~%" left-finger)))
                 (if (string= name "l_gripper_r_finger_joint")
                     (progn
                       (setf right-finger position)
                       (format t "Found right-tip position ~a~%" right-finger))))
           names positions))
    (if (and (is-closed left-finger) 
             (is-closed right-finger))
        (setf (value *gripper-closed*) t)
        (setf (value *gripper-closed*) nil))))

(defun unsub ()
  (unsubscribe *joint-state-subscriber*)
  (setf *joint-state-subscriber* nil))
        
(defun test-sub ()
  (top-level
    (par
      (whenever ((pulsed *gripper-closed*))
        (format t "OOOOOOOOOO ~a~%" *gripper-closed*)
        (if *joint-state-subscriber* 
            (progn
              (unsub)
              (return))))
      (progn
        (sleep 1)
        (monitor-grip)))))

(defun is-closed (pos)
  (and (> pos 0.002096000000000000d0) (< pos 0.002098000000000000d0)))
        
          
          

