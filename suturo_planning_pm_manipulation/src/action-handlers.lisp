(in-package :suturo-planning-pm-manipulation)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler move (pose)
  "Moves thr robot to the specified pose."
  (call-initial-action 'suturo-planning-common:left)
  (call-initial-action 'suturo-planning-common:right))

(def-action-handler touch (arm obj)
  "Moves the arm to the object"
  (call-touch-action arm obj))

; make-goal- and call-action-functions
; initial-position
(defun make-initial-action-goal (in-arm)
  (actionlib:make-action-goal (get-action-client "home_action_server" 
                                                 "suturo_manipulation_msgs/suturo_manipulation_homeAction" )
                              arm in-arm))

(defun call-initial-action (arm)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((result 
                (actionlib:call-goal
                 (get-action-client "home_action_server" 
                                                 "suturo_manipulation_msgs/suturo_manipulation_homeAction" )
                 (if (eql arm 'suturo-planning-common:left)
                     (make-initial-action-goal "left_arm")
                     (make-initial-action-goal "right_arm"))))
              (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                                "Result from call-goal initial position ~a"
                                result)
              (roslisp:with-fields (succ) result
                (roslisp:with-fields (type) succ
                  (cond ((eql type 1))
                        (t (cpl:error 'suturo-planning-common::pose-not-reached))))))))
        (roslisp:ros-info (suturo-pm-manipulation call-initial-action)
                          "Action finished. Initial position reached.")
        (values result status)))

;touch-object
(defun make-touch-action-goal (in-arm in-target)
  (roslisp:ros-info (suturo-pm-manipulation touch-action-goal)
                    "Creating goal 'touch' for ~a arm" in-arm)
  (actionlib:make-action-goal (get-action-client "move_action_server"
                                                 "suturo_manipulation_msgs/suturo_manipulation_moveAction")
                              arm in-arm
                              p in-target))

(defun call-touch-action (arm obj)
  (multiple-value-bind (result status)
      (let ((actionlib:*action-server-timeout* 10.0))
        (let ((actionresult 
                (actionlib:call-goal
                 (get-action-client "move_action_server"
                                    "suturo_manipulation_msgs/suturo_manipulation_moveAction")
                 (if (eql arm 'suturo-planning-common:left)
                     (make-touch-action-goal "left_arm" obj)
                     (make-touch-action-goal "right_arm" obj)))))
          (format t "Result from call-goal touch: ~a" actionresult)
          (roslisp:with-fields (succ) actionresult
            (roslisp:with-fields (type) succ
              (format t "result type: ~a" type)
              (cond 
                ((eql type 1))
                (t (cpl:error 'suturo-planning-common::touch-failed)))))))
    (roslisp:ros-info (sututro-pm-manipulation call-touch-action)
                      "Action finished. Object touched.")
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
  (roslisp: ros-info (suturo-pm-manipulation action-client)
            "Waiting for action server ~a  ..." server)
  (loop until
        (actionlib:wait-for-server *action-client*))
  (roslisp:ros-info (suturo-pm-manipulation action-client)
                    "Action client for goal ~a created ..." goal))

(defun get-action-client (server goal)
  ; Returns an action-client for the specified aserver and goal
  (if (or (and (!= *action-server* server) 
               (!= *action-goal* goal))
          (null *action-client*))
      (progn 
        (setq *action-server* server)
        (setq *action-goal* goal)
        (init-action-client server goal)))
   ; if no action-client exists a new one is initialized 
   ; if the existing action-client doesnt match the specified server and goal a new one is initialized
   ; if the action-client for the server and goal exists, it will be returned
  *action-client*)

