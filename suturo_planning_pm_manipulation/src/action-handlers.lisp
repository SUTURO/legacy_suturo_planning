(in-package :suturo-planning-pm-manipulation)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler take-pose (pose body-part)
  "Moves the robot's body part to the specified pose."
  (cond
    ((eq pose 'initial) (call-initial-action body-part))
    (t (roslisp:ros-error (suturo-pm-manipulation take-pose) "Unhandled pose ~a" pose))))

(def-action-handler move-head (loc)
  "Moves the head to look at a specified location." 
  (call-move-head-action loc))

(def-action-handler grasp (obj arm)
  "Moves arm to the object and grasps it."
  (call-grasp-action obj arm))

(def-action-handler open-hand (obj)
  "Opens the hand containing the passed object and drops the object."
  (call-open-hand-action obj))

(def-action-handler move-arm (location arm)
  "Moves the specified arm to the location."
  (call-move-arm-action location arm))

(def-action-handler move-base (pose-stamped)
  "Moves Base to specified location."
  (call-move-base-action location))

; make-goal- and call-action-functions
(defvar *maximum-retry-intents* 2)

(defvar *gripper-tolerance* 0.03)

(defvar *keep-looping* t)

(defvar *move-head-timeout* 5.0)
(defvar *initial-timeout* 10.0)
(defvar *grasp-timeout* 20.0)
(defvar *open-timeout* 7.0)
(defvar *move-arm-timeout* 10.0)
(defvar *move-base-timeout* 30.0)

; move-head
(defvar *action-client-move-head* nil)

(defun make-move-head-goal (pose-stamped)
  (format t "make-move-head-goal pose-stamped: ~a~%" pose-stamped)
  (actionlib:make-action-goal *action-client-move-head* ps pose-stamped))

(defun call-move-head-action (loc)
  (format t "call-move-head-action loc: ~a~%" loc)
  (setf *action-client-move-head* (get-action-client "suturo_man_move_head_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_headAction"))
  (with-lost-in-resultation-workaround
      *action-client-move-head*
      (let* ((frame (desig-prop-value loc 'frame))
             (coords (desig-prop-value loc 'coords))
             (header-msg (roslisp:make-msg "std_msgs/Header"
                                           (stamp) (roslisp:ros-time)
                                           (frame_id) frame))
             (position-msg (roslisp:make-msg "geometry_msgs/Point"
                                             (x) (first coords)
                                             (y) (second coords)
                                             (z) (third coords)))
             (orientation-msg (roslisp:make-msg "geometry_msgs/Quaternion"
                                                (x) 0
                                                (y) 0
                                                (z) 0
                                                (w) 1))
             (pose-msg (roslisp:make-msg "geometry_msgs/Pose"
                                         (position) position-msg
                                         (orientation) orientation-msg))
             (pose-stamped-msg (roslisp:make-msg "geometry_msgs/PoseStamped"
                                                 (header) header-msg
                                                 (pose) pose-msg)))
        (make-move-head-goal pose-stamped-msg))
      *move-head-timeout*
      'suturo-planning-common::move-head-failed))
           


; initial-position
(defvar *action-client-initial* nil)

(defun make-initial-action-goal (body-part)
  (format t "make-initial-action-goal body-part: ~a~%" body-part)
  (actionlib:make-action-goal *action-client-initial* bodypart
    (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart" bodyPart body-part)))

(defun call-initial-action (body-part)
  (setf *action-client-initial* (get-action-client "suturo_man_move_home_server" 
                                                   "suturo_manipulation_msgs/suturo_manipulation_homeAction"))
  (with-lost-in-resultation-workaround
    *action-client-initial*
    (make-initial-action-goal (get-body-part-constant body-part))
    *initial-timeout*
    'suturo-planning-common::pose-not-reached))

; grasp
(defvar *action-client-grasp* nil)

(defun make-grasp-action-goal (obj in-arm)
  (format t "make obj: ~a ~%in-arm:~a~%" obj in-arm)
  (let* ((msg-header (roslisp:make-msg "std_msgs/Header"
                                       (seq) 4
                                       (stamp) (roslisp:ros-time)
                                       (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-arm (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart"
                                    (bodyPart) in-arm))
         (msg-goal (roslisp:make-msg "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
                                     (header) msg-header 
                                     (objectName) (desig-prop-value obj 'name)
                                     (newton) (desig-prop-value obj 'grip-force)
                                     (grasp) t
                                     (bodypart) msg-arm)))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-grasp*
      goal msg-goal)))

(defun call-grasp-action (obj arm)
  (setf *action-client-grasp*  (get-action-client "suturo_man_grasping_server"
                                                  "suturo_manipulation_msgs/suturo_manipulation_graspingAction"))
  (with-lost-in-resultation-workaround
    *action-client-grasp*
    (make-grasp-action-goal obj (get-body-part-constant arm))
    *grasp-timeout*
    'suturo-planning-common::grasping-failed))
  
(defun grasping-succeeded (obj arm)
  (format t "Updating object's location~%")
  (let* ((loc-des (description (desig-prop-value obj 'at)))
         (loc (make-designator 'location 
                               (update-designator-properties 
                                `((in ,(if (eql arm 'left-arm) 'left-gripper 'right-gripper)))
                                loc-des)))
         (new-obj (make-designator 'object
                                   (update-designator-properties 
                                    `((at ,loc))
                                    (description obj)))))
    (equate obj new-obj)))

; open-hand
(defvar *action-client-open-hand* nil)

(defun make-open-hand-goal (obj arm)
  (format t "make-open-hand-goal obj:~a~%" obj)
  (let* ((msg-header
           (roslisp:make-msg "std_msgs/Header"
                             (seq) 4
                             (stamp) (roslisp:ros-time)
                             (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-goal
           (roslisp:make-msg
            "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
            (header) msg-header 
            (objectName) (desig-prop-value obj 'name)
            (grasp) nil
            (bodypart) (roslisp:make-msg
                        "suturo_manipulation_msgs/RobotBodyPart"
                        (bodyPart) (get-body-part-constant arm)))))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-open-hand*
      goal msg-goal)))

(defun call-open-hand-action (obj)
  (format t "call-open-hand-action obj: ~a~%" obj)
  (setf *action-client-open-hand* (get-action-client "suturo_man_grasping_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_graspingAction"))

  (let* ((arm (if (eq (desig-prop-value (desig-prop-value obj 'at) 'in) 'left-gripper) 
                  'left-arm
                  'right-arm))
         (gripper-state (get-gripper-state arm)))   
    (with-lost-in-resultation-workaround
      *action-client-open-hand*
      (make-open-hand-goal obj arm)
      *open-timeout*
      'suturo-planning-common::drop-failed
      :on-timeout-fn #'(lambda ()
                         (let* ((new-gripper-state (get-gripper-state arm))
                                (gripper-difference (difference gripper-state new-gripper-state)))
                           (if (> gripper-difference *gripper-tolerance*)
                               (progn
                                 (format t "Gripper difference: ~a~%" gripper-difference)
                                 (format t "Gripper seems to have moved. Waiting until gripper stops.~%")
                                 (waiting-for-gripper arm)
                                 (format t "Gripper seems to have stopped moving. Assuming grasping succeeded.~%")
                                 nil)
                               (progn
                                 (format t "Gripper difference: ~a~%" gripper-difference)
                                 (format t "Gripper doesn't seem to have moved.~%")
                                 t)))))))

; move-arm
(defvar *action-client-move-arm* nil)

(defun make-move-arm-goal (pose-stamped in-arm)
  (format t "make-move-arm-goal pose-stamped: ~a~% arm:~a~%" pose-stamped in-arm)
  (actionlib:make-action-goal *action-client-move-arm* 
    ps pose-stamped
    bodypart (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart"
                               (bodyPart) in-arm)))

(defun call-move-arm-action (location arm)
  (format t "call-move-arm-action arm:~a~%" arm)
  (setf *action-client-move-arm* (get-action-client "suturo_man_move_arm_server"
                                                    "suturo_manipulation_msgs/suturo_manipulation_moveAction"))
  (format t "getting frame and coords.~%")
  (let* ((frame (desig-prop-value location 'frame))
         (coords (desig-prop-value location 'coords))
         (header-msg (roslisp:make-msg "std_msgs/Header"
                                       (stamp) (roslisp:ros-time)
                                       (frame_id) frame))
         (position-msg (roslisp:make-msg "geometry_msgs/Point"
                                         (x) (first coords)
                                         (y) (second coords)
                                         (z) (third coords)))
         (orientation-msg (roslisp:make-msg "geometry_msgs/Quaternion"
                                            (x) 0
                                            (y) 0
                                            (z) 0
                                            (w) 1))
         (pose-msg (roslisp:make-msg "geometry_msgs/Pose"
                                     (position) position-msg
                                     (orientation) orientation-msg))
         (pose-stamped-msg (roslisp:make-msg "geometry_msgs/PoseStamped"
                                             (header) header-msg
                                             (pose) pose-msg)))
    (format t "created helper messages.~%")
    (with-lost-in-resultation-workaround
      *action-client-move-arm*
      (make-move-arm-goal pose-stamped-msg (get-body-part-constant arm))
      *move-arm-timeout*
      'suturo-planning-common::move-arm-failed)))

; move-base

(defvar *action-client-move-base* nil)

(defun make-move-base-goal (pose-stamped)
  (format t "make-move-base-goal pose-stamped: ~a~%" pose-stamped)
  (actionlib:make-action-goal *action-client-move-base* 
                              ps pose-stamped))

(defun call-move-base-action (pose-stamped)
  (format t "call-move-base-action location:~a~%" location)
  (setf *action-client-move-base* (get-action-client "suturo_man_move_base_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_baseAction"))
  (with-lost-in-resultation-workaround
    *action-client-move-base*
    (make-move-base-goal pose-stamped)
    *move-base-timeout*
    'suturo-planning-common::move-base-failed))
         
     
; Helper functions for actions

(defun get-gripper-state (arm)
  (suturo-planning-pm-gripper-monitor::call-action
                 'get-gripper-state
                 arm))

(defun difference (val-one val-two)
  "Calculates the difference between two values. 0 means no difference, 1 means 100% difference."
  (format t "val-one: ~a, val-two: ~a~%" val-one val-two)
  (if (< val-one val-two)
      (let ((diff (- val-two val-one)))
            (/ diff val-one))
      (let ((diff (- val-one val-two)))
            (/ diff val-one))))

(defun waiting-for-gripper (arm)
  (loop
    (format t "Entering loop.~%")
    (let ((state-one (get-gripper-state arm)))
      (sleep 10)
      (let* ((state-two (get-gripper-state arm))
             (diff (difference state-one state-two)))
        (format t "Gripper difference ~a~%" diff)
        (if (< diff *gripper-tolerance*)
            (progn
              (format t "EXITTING: diff < tolerance: ~a < ~a~%" diff *gripper-tolerance*)
              (return))
            (format t "LOOPING: diff > tolerance: ~a > ~a~%" diff *gripper-tolerance*))))))

(defvar *action-client* nil) 

(defun init-action-client (server goal)
  ;Initialises an action-client for the specified server and goal
  (setf *action-client* (actionlib:make-action-client
                         server
                         goal))
  (roslisp:ros-info (suturo-pm-manipulation action-client)
                    "Waiting for action server ~a  ..." server)
  (loop until
        (actionlib:wait-for-server *action-client*))
  (roslisp:ros-info (suturo-pm-manipulation action-client)
                    "Action client for goal ~a created ..." goal))

(defun get-action-client (server goal)
  "Returns an action-client for the specified server and goal"
  (init-action-client server goal)
  *action-client*)

(defun get-body-part-constant (body-part)
  (string-downcase
   (symbol-name
    (cond
      ((eq body-part 'left-arm)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :LEFT_ARM))
      ((eq body-part 'right-arm)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :RIGHT_ARM))
      ((eq body-part 'both-arms)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :BOTH_ARMS))
      ((eq body-part 'head)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :HEAD))
      (t (roslisp:ros-error
          (suturo-pm-manipulation)
          "Unhandled body part: ~a" body-part)
         (cpl:error 'suturo-planning-common::unhandled-body-part))))))
