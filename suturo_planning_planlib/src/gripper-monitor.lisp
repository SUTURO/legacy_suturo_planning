(in-package :suturo-planning-planlib)

(defvar *gripper-closed* (make-fluent :value nil))

(defvar *joint-state-subscriber* nil)

(defun monitor-grip (arm)
  "Subscribes to joint_states an monitors the state of the gripper"
  (setf *joint-state-subscriber* 
        (subscribe "/joint_states" 
                   "sensor_msgs/JointState" 
                   #'(lambda (state) (check-grip arm state)))))

(defun check-grip (arm state)
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

(defun gripper-is-closed (arm)
  (monitor-grip arm)
  (sleep 1)
  (unsub-joint-state))

(defun unsub-joint-state ()
  (if (not *joint-state-subscriber*)
      (unsubscribe *joint-state-subscriber*))
  (setf *joint-state-subscriber* nil))
  
;;;zum testen
      
(defun test-sub ()
  (setf (value *gripper-closed*) nil)
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
        (monitor-grip 'left-arm)))))
        
          
          

