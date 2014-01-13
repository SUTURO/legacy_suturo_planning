(in-package :suturo-planning-pm-manipulation)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler take-pose (pose)
  "Moves the robot to the specified pose."
  (call-initial-action 'desig-props:left)
  (call-initial-action 'desig-props:right))

(def-action-handler move-head (loc)
  "Moves the head to look at a specified location." 
  (call-move-head-action loc))

(def-action-handler grasp (obj arm)
  "Moves arm to the object and grasp it."
  (call-grasp-action obj arm))

(def-action-handler open-hand (arm)
  "Opens the hand and drops the object."
  (call-open-hand-action arm))

(def-action-handler move-arm (location arm)
  "Moves the specified arm to the location."
  (call-move-head-action location arm))

; make-goal- and call-action-functions
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
      (multiple-value-bind (result status)
          (let ((actionlib:*action-server-timeout* 10.0))
            (let ((result 
                    (actionlib:call-goal *action-client-move-head*
                                         (make-move-head-goal pose-stamped-msg))))
              (roslisp:ros-info (suturo-pm-manipulation call-move-head-action)
                                "Result from call-goal move head ~a"
                                result)
              (roslisp:with-fields (succ) result
                (roslisp:with-fields (type) succ
                  (cond ((eql type 1))
                        (t (cpl:error 'suturo-planning-common::move-head-failed))))))) 
        (roslisp:ros-info(suturo-pm-manipulation call-move-head-action)
                         "Action finished. Head is looking at direction.")
        (values result status)))))
  

; initial-position
(defun make-initial-action-goal (in-arm)
  (actionlib:make-action-goal (get-action-client "suturo_man_move_home_server" 
                                                 "suturo_manipulation_msgs/suturo_manipulation_homeAction" )
                              arm in-arm))

(defun call-initial-action (arm)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result 
                (actionlib:call-goal
                 (get-action-client "suturo_man_move_home_server" 
                                    "suturo_manipulation_msgs/suturo_manipulation_homeAction" )
                 (if (eql arm 'desig-props:left)
                     (make-initial-action-goal "left_arm")
                     (make-initial-action-goal "right_arm")))))
          (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                            "Result from call-goal initial position ~a"
                            result)
          (roslisp:with-fields (succ) result
            (roslisp:with-fields (type) succ
              (cond ((eql type 1))
                    (t (cpl:error 'suturo-planning-common::pose-not-reached)))))))
          (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                            "Action finished. Initial position reached.")
          (values result status)))

; grasp
(defvar *action-client-grasp* nil)

(defun make-grasp-action-goal (obj in-arm)
  (format t "make obj: ~a ~%in-arm:~a~%" obj in-arm)
  (let* ((msg-header (roslisp:make-msg "std_msgs/Header"
                                    (seq) 4
                                    (stamp) (roslisp:ros-time)
                                    (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-goal (roslisp:make-msg "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
                                  (header) msg-header 
                                  (objectName) (desig-prop-value obj 'name)
                                  (grasp) t
                                  (arm) in-arm)))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-grasp*
      goal msg-goal)))

(defun call-grasp-action (obj arm)
  (format t "FIert~%")
  (setf *action-client-grasp*  (get-action-client "suturo_man_grasping_server"
                                                  "suturo_manipulation_msgs/suturo_manipulation_graspingAction"))
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result
                (actionlib:call-goal
                 *action-client-grasp*
                 (make-grasp-action-goal obj arm)
                 :timeout 120.0)))
          (roslisp:ros-info (suturo-pm-manipulation call-grasp-action)
                            "Result from call-goal grasp object ~a"
                            result)
          (roslisp:with-fields (succ) result
            (roslisp:with-fields (type) succ
              (cond ((eql type 1))
                    (t (cpl:error 'suturo-planning-common::grasp-failed)))))))
          (roslisp:ros-info(suturo-pm-manipulation call-grasp-action)
                           "Action finished. Object grasped.")
          (values result status)))

; open-hand
(defvar *action-client-open-hand* nil)

(defun make-open-hand-goal (obj)
  (format t "make-open-hand-goal obj:~a~%" obj)
  (let* ((msg-header (roslisp:make-msg "std_msgs/Header"
                                    (seq) 4
                                    (stamp) (roslisp:ros-time)
                                    (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-goal (roslisp:make-msg "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
                                  (header) msg-header 
                                  (objectName) (desig-prop-value obj 'name)
                                  (grasp) nil
                                  (arm) "")))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-open-hand*
      goal msg-goal)))

(defun call-open-hand-action (arm)
  (format t "call-open-hand-action arm: ~a~%" arm)
  (setf *action-client-open-hand* (get-action-client "suturo_man_grasping_server"
                                                     "suturo_manipulation_msgs/suturo_manipulation_graspingAction"))
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result
                (actionlib:call-goal
                 *action-client-open-hand*
                 (make-open-hand-goal arm)
                 :timeout 60.0)))
          (roslisp:ros-info (suturo-pm-manipulation call-open-hand-action)
                            "Result from call-goal open hand ~a"
                            result)
          (roslisp:with-fields (succ) result 
            (roslisp:with-fields (type) succ
              (cond ((eql type 1))
                    (t (cpl:error 'suturo-planning-common::drop-failed)))))))
          (roslisp:ros-info(suturo-pm-manipulation call-open-hand-action)
                           "Action finisehd. Object droped.")
          (values result status)))

; move-arm
(defun make-move-arm-goal (location in-arm)
  (actionlib:make-action-goal (get-action-client "suturo_man_move_arm_server"
                                                 "suturo_manipulation_move_armAction")
                              header (desig-prop-value location 'frame)
                              arm in-arm))

(defun call-move-arm-action (location arm)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result 
                (actionlib:call-goal
                 (get-action-client "suturo_man_move_arm_server"
                                    "suturo_manipulation_move_armAction")
                 (make-move-arm-action location arm))))
          (roslisp:ros-info (suturo-pm-manipulation call-move-arm-action)
                            "Result from call-goal move arm ~a"
                            result)
          (roslisp:with-fields (succ) result
            (roslisp:with-fields (type) succ
              (cond ((eql type 1))
                    (t (cpl:error 'suturo-planning-common::location-not-reached)))))))
            (roslisp:ros-info(suturo-pm-manipulation call-move-arm-action)
                             "Action finished. Location reached.")
            (values result status)))

; Helper functions for actions

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

