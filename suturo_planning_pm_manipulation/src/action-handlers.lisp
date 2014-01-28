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

; make-goal- and call-action-functions
(defvar *maximum-retry-intents* 5)

(defvar *gripper-tolerance* 0.03)

(defvar *keep-looping* t)

(defvar *move-head-timeout* 5.0)
(defvar *initial-timeout* 10.0)
(defvar *grasp-timeout* 20.0)
(defvar *open-timeout* 5.0)
(defvar *move-arm-timeout* 10.0)

; move-head
(defvar *action-client-move-head* nil)

(defun make-move-head-goal (pose-stamped)
  (format t "make-move-head-goal pose-stamped: ~a~%" pose-stamped)
  (actionlib:make-action-goal *action-client-move-head* ps pose-stamped))

(defun call-move-head-action (loc)
  (format t "call-move-head-action loc: ~a~%" loc)
  (setf *action-client-move-head* (get-action-client "suturo_man_move_head_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_headAction"))
  (let ((frame (desig-prop-value loc 'frame))
        (coords (desig-prop-value loc 'coords)))
    (let* ((header-msg (roslisp:make-msg "std_msgs/Header"
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
      (let ((intents 0))
        (setf *keep-looping* t)
        (loop
          (if (eq *keep-looping* nil)
              (return))
          (block continue
            (multiple-value-bind (result status)
                (actionlib:call-goal *action-client-move-head*
                                     (make-move-head-goal pose-stamped-msg)
                                     :timeout *move-head-timeout*
                                     :result-timeout *move-head-timeout*)
              (roslisp:ros-info (suturo-pm-manipulation call-move-head-action)
                                "Result from call-goal move head ~a" result)
              (if (eq result nil)
                  (if (eq status :TIMEOUT)
                      (progn
                        (format t "Timeout reached.~%")
                        (format t "Number intents: ~a~%" intents)
                        (if (< intents *maximum-retry-intents*)
                            (progn
                              (format t "Retrying~%")
                              (incf intents)
                              (return-from continue))
                            (progn
                              (format t "Maximum number of intents reached.~%")
                              (cpl:fail 'suturo-planning-common::move-head-failed)
                              (return))))
                      (progn
                        (format t "Unhandled condition.~%")
                        (cpl:error 'suturo-planning-common::unhandled-condition)
                        (return)))  
                  (roslisp:with-fields (succ) result
                    (if (eq succ nil)
                        (progn
                          (format t "Unknown problem.~%")
                          (cpl:fail 'suturo-planning-common::move-head-failed)
                          (return))
                        (roslisp:with-fields (type) succ
                          (handle-action-answer type 'suturo-planning-common::move-head-failed)
                          (roslisp:ros-info(suturo-pm-manipulation call-move-head-action)
                                           "Action finished. Head is looking at direction.")
                          (return))))))))))))


; initial-position
(defvar *action-client-initial* nil)

(defun make-initial-action-goal (body-part)
  (format t "make-initial-action-goal body-part: ~a~%" body-part)
  (actionlib:make-action-goal *action-client-initial* bodypart
    (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart" bodyPart body-part)))

(defun call-initial-action (body-part)
  (setf *action-client-initial* (get-action-client "suturo_man_move_home_server" 
                                                   "suturo_manipulation_msgs/suturo_manipulation_homeAction"))
  (let ((intents 0))
    (setf *keep-looping* t)
    (loop
      (if (eq *keep-looping* nil)
          (return))
      (block continue
        (multiple-value-bind (result status)
            (actionlib:call-goal
             *action-client-initial*
             (get-body-part-constant body-part)
             :timeout *initial-timeout*
             :result-timeout *initial-timeout*)
          (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                            "Result from call-goal initial position ~a" result)
          (if (eq result nil)
              (if (eq status :TIMEOUT)
                  (progn
                    (format t "Timeout reached.~%")
                    (format t "Number intents: ~a~%" intents)
                    (if (< intents *maximum-retry-intents*)
                        (progn
                          (format t "Retrying~%")
                          (incf intents)
                          (return-from continue))
                        (progn
                          (format t "Maximum number of intents reached.~%")
                          (cpl:fail 'suturo-planning-common::pose-not-reached)
                          (return))))
                  (progn
                    (format t "Unhandled condition.~%")
                    (cpl:error 'suturo-planning-common::unhandled-condition)
                    (return)))
                (roslisp:with-fields (succ) result
                  (if (eq succ nil)
                      (progn
                        (format t "Unknown problem.~%")
                        (cpl:fail 'suturo-planning-common::pose-not-reached)
                        (return))
                      (roslisp:with-fields (type) succ
                        (handle-action-answer type 'suturo-planning-common::pose-not-reached)
                        (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                                          "Action finished. Initial position for ~a reached." body-part)
                        (return))))))))))

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
  (let ((intents 0))
    (let ((gripper-state (get-gripper-state arm)))
      (setf *keep-looping* t)
      (loop
        (if (eq *keep-looping* nil)
            (return))
        (block continue
          (multiple-value-bind (result status)
              (actionlib:call-goal
               *action-client-grasp*
               (make-grasp-action-goal obj (get-body-part-constant arm))
               :timeout *grasp-timeout*
               :result-timeout *grasp-timeout*)
            (roslisp:ros-info (suturo-pm-manipulation call-grasp-action)
                              "Result from call-goal grasp object ~a" result)
            (if (eq result nil)
                (if (eq status :TIMEOUT)
                    (progn
                      (format t "Timeout reached.~%")
                      #|
                      (let* ((new-gripper-state (get-gripper-state arm))
                             (gripper-difference (difference gripper-state new-gripper-state)))
                        (if (> gripper-difference *gripper-tolerance*)
                            (progn
                              (format t "Gripper difference: ~a~%" gripper-difference)
                              (format t "Gripper seems to have moved. Waiting until gripper stops.~%")
                              (waiting-for-gripper arm)
                              (format
                               t
                               "Gripper seems to have stopped moving. Assuming grasping succeeded.~%")
                              (grasping-succeeded obj arm)
                              (return))
                            (progn
                              (format t "Gripper difference: ~a~%" gripper-difference)
                              (format t "Gripper doesn't seem to have moved.~%"))))
                      |#
                      (format t "Number intents: ~a~%" intents)
                      (if (< intents *maximum-retry-intents*)
                          (progn
                            (format t "Retrying~%")
                            (incf intents)
                            (return-from continue))
                          (progn
                            (format t "Maximum number of intents reached.~%")
                            (cpl:fail 'suturo-planning-common::grasping-failed)
                            (return))))
                    (progn
                      (format t "Unhandled condition.~%")
                      (cpl:error 'suturo-planning-common::unhandled-condition)
                      (return)))  
                (roslisp:with-fields (succ) result
                  (if (eq succ nil)
                      (progn
                        (format t "Unknown problem.~%")
                        (cpl:error 'suturo-planning-common::grasping-failed)
                        (return))
                      (roslisp:with-fields (type) succ
                        (handle-action-answer type 'suturo-planning-common::grasping-failed)
                        (grasping-succeeded obj arm)
                        (roslisp:ros-info(suturo-pm-manipulation call-grasp-action)
                                         "Action finished. Object grasped.~%")
                        (return)))))))))))

(defun grasping-succeeded (obj arm)
  (format t "Updating object's location~%")
  (let* ((loc-des (description (desig-prop-value obj 'at)))
         (loc (make-designator 'location 
                               (update-designator-properties 
                                `((in ,(if (eql arm 'left-gripper) 'left-gripper 'right-gripper)))
                                loc-des)))
         (new-obj (make-designator 'object
                                   (update-designator-properties 
                                    `((at ,loc))
                                    (description obj)))))
    (equate obj new-obj)))

; open-hand
(defvar *action-client-open-hand* nil)

(defun make-open-hand-goal (obj)
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
                        (bodyPart) (get-body-part-constant 'left-arm)))))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-open-hand*
      goal msg-goal)))

(defun call-open-hand-action (arm)
  (format t "call-open-hand-action arm: ~a~%" arm)
  (setf *action-client-open-hand* (get-action-client "suturo_man_grasping_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_graspingAction"))
  (let ((intents 0))
    (let ((gripper-state (get-gripper-state arm)))
      (setf *keep-looping* t)
      (loop
        (if (eq *keep-looping* nil)
            (return))
        (block continue
          (multiple-value-bind (result status)
              (actionlib:call-goal
               *action-client-open-hand*
               (make-open-hand-goal arm)
               :timeout *open-timeout*
               :result-timeout *open-timeout*)
            (roslisp:ros-info (suturo-pm-manipulation call-open-hand-action)
                              "Result from call-goal open hand ~a" result)
            (if (eq result nil)
                (if (eq status :TIMEOUT)
                    (progn
                      (format t "Timeout reached.~%")
                      (let* ((new-gripper-state (get-gripper-state arm))
                             (gripper-difference (difference gripper-state new-gripper-state)))
                        (if (> gripper-difference *gripper-tolerance*)
                            (progn
                              (format t "Gripper difference: ~a~%" gripper-difference)
                              (format t "Gripper seems to have moved. Waiting until gripper stops.~%")
                              (waiting-for-gripper arm)
                              (format t "Gripper seems to have stopped moving. Assuming grasping succeeded.~%")
                              (return))
                            (progn
                              (format t "Gripper difference: ~a~%" gripper-difference)
                              (format t "Gripper doesn't seem to have moved.~%"))))
                      (format t "Number intents: ~a~%" intents)
                      (if (< intents *maximum-retry-intents*)
                          (progn
                            (format t "Retrying~%")
                            (incf intents)
                            (return-from continue))
                          (progn
                            (format t "Maximum number of intents reached.~%")
                            (cpl:fail 'suturo-planning-common::drop-failed)
                            (return))))
                    (progn
                      (format t "Unhandled condition.~%")
                      (cpl:error 'suturo-planning-common::unhandled-condition)
                      (return)))
                (roslisp:with-fields (succ) result
                  (if (eq succ nil)
                      (progn
                        (format t "Unknown problem.~%")
                        (cpl:error 'suturo-planning-common::drop-failed)
                        (return))
                      (roslisp:with-fields (type) succ
                        (handle-action-answer type 'suturo-planning-common::drop-failed)
                        (roslisp:ros-info(suturo-pm-manipulation call-open-hand-action)
                                         "Action finisehd. Object dropped.")
                        (return)))))))))))

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
  (let ((frame (desig-prop-value location 'frame))
        (coords (desig-prop-value location 'coords)))
    (format t "creating helper messages.~%")
    (let* ((header-msg (roslisp:make-msg "std_msgs/Header"
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
      (let ((intents 0))
        (setf *keep-looping* t)
        (loop
          (if (eq *keep-looping* nil)
              (return))
          (block continue
            (multiple-value-bind (result status)
                (actionlib:call-goal
                 *action-client-move-arm*
                 (make-move-arm-goal pose-stamped-msg (get-body-part-constant arm))
                 :timeout *move-arm-timeout*
                 :result-timeout *move-arm-timeout*)
              (roslisp:ros-info (suturo-pm-manipulation call-move-arm-action)
                                "Result from call-goal move arm ~a" result)
              (if (eq result nil)
                  (if (eq status :TIMEOUT)
                      (progn
                        (format t "Timeout reached.~%")
                        (format t "Number intents: ~a~%" intents)
                        (if (< intents *maximum-retry-intents*)
                            (progn
                              (format t "Retrying~%")
                              (incf intents)
                              (return-from continue))
                            (progn
                              (format t "Maximum number of intents reached.~%")
                              (cpl:fail 'suturo-planning-common::move-arm-failed)
                              (return))))
                      (progn
                        (format t "Unhandled condition.~%")
                        (cpl:error 'suturo-planning-common::unhandled-condition)
                        (return)))                  
                  (roslisp:with-fields (succ) result
                    (if (eq succ nil)
                        (progn
                          (format t "Unknown problem.~%")
                          (cpl:fail 'suturo-planning-common::move-arm-failed)
                          (return))
                        (roslisp:with-fields (type) succ
                          (handle-action-answer type 'suturo-planning-common::move-arm-failed)          
                          (roslisp:ros-info(suturo-pm-manipulation call-move-arm-action)
                                           "Action finished. Location reached.")
                          (return))))))))))))

; Helper functions for actions

(defun get-gripper-state (arm)
  (suturo-planning-pm-gripper-monitor::call-action
                 'get-gripper-state
                 arm))

(defun difference (val-one val-two)
  "Calculates the difference between two values. 0 means no difference, 1 means 100% difference."
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
              (format t "EXITTING: diff > tolerance: ~a < ~a~%" diff *gripper-tolerance*)
              (return))
            (format t "LOOPING: diff > tolerance: ~a < ~a~%" diff *gripper-tolerance*))))))

(defvar *action-client* nil) 
(defvar *action-server* nil)
(defvar *action-goal* nil)

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
  ; Returns an action-client for the specified aserver and goal
  (if (or (and (not (eq *action-server* server)) 
               (not (eq *action-goal* goal)))
          (null *action-client*))
      (progn 
        (setq *action-server* server)
        (setq *action-goal* goal)
        (init-action-client server goal)))
  ; if no action-client exists a new one is initialized 
  ; if the existing action-client doesnt match the specified server and goal a new one is initialized
  ; if the action-client for the server and goal exists, it will be returned
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
          (suturo-pm-manipulation call-initial-action)
          "Unhandled body part: ~a" body-part)
         (cpl:error 'suturo-planning-common::unhandled-body-part))))))

(defun handle-action-answer (type error-to-throw)
  "Analyses returns from action servers and throws the passed error if necessary."
  (cond
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :SUCCESS))
     (roslisp:ros-info(suturo-planning-pm-manipulation call-move-arm-action) "SUCCESS!")
     (setf *keep-looping* nil))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :FAIL))
     (roslisp:ros-info suturo-planning-pm-manipulation "FAIL!")
     (setf *keep-looping* nil)
     (cpl:error error-to-throw))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :NOPLAN))
     (roslisp:ros-info (suturo-planning-pm-manipulation) "No plan found!")
     (setf *keep-looping* nil)
     (cpl:error 'suturo-planning-common::no-plan-found))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :UNDEFINED))
     (info-out suturo-planning-pm-manipulation "Unknown error. Blame manipulation!")
     (setf *keep-looping* nil)
     (cpl:error error-to-throw))
    (t
     (roslisp:ros-info suturo-planning-pm-manipulation "Unhandled action answer.")
     (setf *keep-looping* nil)
     (cpl:error 'suturo-planning-common::unhandled-action-answer))))
