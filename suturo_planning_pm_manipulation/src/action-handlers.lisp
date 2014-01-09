(in-package :suturo-planning-pm-manipulation)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler take-pose (pose)
  "Moves the robot to the specified pose."
  (call-initial-action 'desig-props:left)
  (call-initial-action 'desig-props:right))

(def-action-handler move-head (direction)
  "Moves the head to look in a specified direction." 
  (call-move-head-action direction))

(def-action-handler grasp (obj arm)
  "Moves arm to the object and grasp it."
  (call-grasp-action obj arm))

(def-action-handler open-hand (arm)
  "Opens the hand and drops the object."
  (call-open-hand-action arm))

; make-goal- and call-action-functions
; move-head

(defun make-move-head-goal (direction)
  (actionlib:make-action-goal (get-action-client "suturo_man_move_head_server"
                                                 "suturo_manipulation_msgs/suturo_manipulation_move_head_serverAction")
                              p direction))

(defun call-move-head-action (direction)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result 
                (actionlib:call-goal
                 (get-action-client "suturo_man_move_head_server"
                                    "suturo_manipulation_msgs/suturo_manipulation_move_head_serverAction")
                 (make-move-head-goal direction))))
          (roslisp:ros-info (suturo-pm-manipulation call-move-head-action)
                            "Result from call-goal move head ~a"
                            result)
          (roslisp:with-fields (succ) result
            (roslisp:with-fields (type) succ
              (cond ((eql type 1))
                    (t (cpl:error 'suturo-planning-common::move-head-fails))))))) 
          (roslisp:ros-info(suturo-pm-manipulation call-move-head-action)
                           "Action finished. Head is looking at direction.")
          (values result status)))
  

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
(defun make-grasp-action-goal (obj in-arm)
  (actionlib:make-action-goal (get-action-client "suturo_man_grasping_server"
                                                 "suturo_manipulation_graspingAction")
                              header (desig-prop-value (desig-prop-value obj 'at)  'frame)
                              objectName (desig-prop-value obj 'name)
                              grasp t
                              arm in-arm))

(defun call-grasp-action (obj arm) 
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result
                (actionlib:call-goal
                 (get-action-client "suturo_man_grasping_server"
                                    "suturo_manipulation_graspingAction")
                 (make-grasp-action-goal obj arm))))
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
(defun make-open-hand-goal (in-arm)
  (actionlib:make-action-goal (get-action-client "suturo_man_grasping_server"
                                                 "suturo_manipulation_graspingAction")
                              header nil
                              objectName nil
                              grasp nil
                              arm in-arm))

(defun call-open-hand-action (arm)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result
                (actionlib:call-goal
                 (get-action-client "suturo_man_grasping_server"
                                    "suturo_manipulation_graspingAction")
                 (make-open-hand-goal arm))))
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

