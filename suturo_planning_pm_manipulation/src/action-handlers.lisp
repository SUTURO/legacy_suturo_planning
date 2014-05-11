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

(def-action-handler grasp (obj arm grasp-action tolerance)
  "Moves arm to the object and grasps it."
  (call-grasp-action obj arm grasp-action tolerance))

(def-action-handler open-hand (obj target-on)
  "Opens the hand containing the passed object and drops the object."
  (call-open-hand-action obj target-on))

(def-action-handler move-arm (location arm)
  "Moves the specified arm to the location."
  (call-move-arm-action location arm))

(def-action-handler move-base (pose-stamped)
  "Moves Base to specified location."
  (call-move-base-action pose-stamped))

; make-goal- and call-action-functions
(defvar *maximum-retry-intents* 2)

(defvar *gripper-tolerance* 0.03)

(defvar *keep-looping* t)

(defvar *move-head-timeout* 5.0)
(defvar *initial-timeout* 10.0)
(defvar *grasp-timeout* 25.0)
(defvar *open-timeout* 10.0)
(defvar *move-arm-timeout* 10.0)
(defvar *move-base-timeout* 30.0)

; move-head
(defvar *action-client-move-head* nil)
(defvar *action-client-move-head-server*  "suturo_man_move_head_server")
(defvar *action-client-move-head-goal*  "suturo_manipulation_msgs/suturo_manipulation_headAction")
(defvar *move-head-cancel-topic-type*  "actionlib_msgs/GoalID")
(defvar *move-head-result-topic-type*
  "suturo_manipulation_msgs/suturo_manipulation_headActionResult")
(defvar *move-head-status-topic-type* "actionlib_msgs/GoalStatusArray")

(defun make-move-head-goal (pose-stamped)
  (format t "make-move-head-goal pose-stamped: ~a~%" pose-stamped)
  (actionlib:make-action-goal *action-client-move-head* ps pose-stamped))

(defun call-move-head-action (loc)
  (format t "call-move-head-action loc: ~a~%" loc)
  (setf *action-client-move-head* (get-action-client 'move-head))
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
      'suturo-planning-common::move-head-failed
    *action-client-move-head-server*
    *move-head-cancel-topic-type*
    *move-head-result-topic-type*
    *move-head-status-topic-type*))
           


; initial-position
(defvar *action-client-initial* nil)
(defvar *action-client-initial-server* "suturo_man_move_home_server")
(defvar *action-client-initial-goal* "suturo_manipulation_msgs/suturo_manipulation_homeAction")
(defvar *initial-cancel-topic-type*  "actionlib_msgs/GoalID")
(defvar *initial-result-topic-type*
  "suturo_manipulation_msgs/suturo_manipulation_homeActionResult")
(defvar *initial-status-topic-type* "actionlib_msgs/GoalStatusArray")

(defun make-initial-action-goal (body-part)
  (format t "make-initial-action-goal body-part: ~a~%" body-part)
  (actionlib:make-action-goal *action-client-initial* bodypart
    (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart" bodyPart body-part)))

(defun call-initial-action (body-part)
  (setf *action-client-initial* (get-action-client 'initial))
  (with-lost-in-resultation-workaround
    *action-client-initial*
    (make-initial-action-goal (get-body-part-constant body-part))
    *initial-timeout*
    'suturo-planning-common::pose-not-reached
    *action-client-initial-server*
    *initial-cancel-topic-type*
    *initial-result-topic-type*
    *initial-status-topic-type*
    :intents 3))

; grasp
(defvar *action-client-grasp* nil)
(defvar *action-client-grasp-server* "suturo_man_grasping_server")
(defvar *action-client-grasp-goal* "suturo_manipulation_msgs/suturo_manipulation_graspingAction")
(defvar *grasp-cancel-topic-type*  "actionlib_msgs/GoalID")
(defvar *grasp-result-topic-type*
  "suturo_manipulation_msgs/suturo_manipulation_graspingActionResult")
(defvar *grasp-status-topic-type* "actionlib_msgs/GoalStatusArray")

(defun make-grasp-action-goal (obj in-arm grasp-action tolerance)
  (format t "make obj: ~a ~%in-arm:~a~%" obj in-arm)
  (let* ((time (roslisp:ros-time))
         (msg-header (roslisp:make-msg "std_msgs/Header"
                                       (stamp) time
                                       (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-arm (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart"
                                    (bodyPart) in-arm))
         (msg-grasp-header (roslisp:make-msg "std_msgs/Header"
                                       (stamp) time
                                       (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-action (cond
                       (tolerance (roslisp:make-msg "suturo_manipulation_msgs/GraspingAndDrop"
                                                    (header) msg-grasp-header
                                                    (action) (get-grasp-constant grasp-action)
                                                    (tolerance) (degrees->radians tolerance)))
                       (t (roslisp:make-msg "suturo_manipulation_msgs/GraspingAndDrop"
                                            (header) msg-grasp-header
                                            (action) (get-grasp-constant grasp-action)))))
         (msg-goal (roslisp:make-msg "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
                                     (header) msg-header 
                                     (objectName) (desig-prop-value obj 'name)
                                     (newton) (desig-prop-value obj 'grip-force)
                                     (action) msg-action
                                     (bodypart) msg-arm)))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-grasp*
      goal msg-goal)))

(defun call-grasp-action (obj arm grasp-action tolerance)
  (setf *action-client-grasp*  (get-action-client 'grasp))
  (with-lost-in-resultation-workaround
      *action-client-grasp*
    (make-grasp-action-goal obj (get-body-part-constant arm) grasp-action tolerance)
    *grasp-timeout*
    'suturo-planning-common::grasping-failed
    *action-client-grasp-server*
    *grasp-cancel-topic-type*
    *grasp-result-topic-type*
    *grasp-status-topic-type*
    :on-success-fn #'(lambda () (grasping-succeeded obj arm))))
  
(defun grasping-succeeded (obj arm)
  (format t "Calling grasping-succeeded.~%")
  (format t "Updating object's location and pose~%")
  (sleep 4)
  (let* ((curr-obj (current-desig obj))
         (obj-name (desig-prop-value curr-obj 'name))
         (obj-on-name (desig-prop-value-concat curr-obj '(at on name)))
         (obj-on (suturo-planning-pm-knowledge::call-action
                     'suturo-planning-pm-knowledge::get-static-object obj-on-name))
         (gripper-frame (get-gripper-frame arm))
         (object-on-height (first (desig-prop-value obj-on 'dimensions)))
         (object-on-origin (transform-get-origin obj-on-name "/map" :timeout 2))
         (object-origin (transform-get-origin (desig-prop-value curr-obj 'name) "/map" :timeout 2))
         (object-height
           (* 2 (- (- (cl-transforms:z object-origin)
                      (cl-transforms:z object-on-origin))
                   (/ object-on-height 2.0))))
         (obj-on-to-object (transform obj-name obj-on-name :timeout 2))
         (target-to-object (cl-tf:make-stamped-transform
                            "/target-point"
                            obj-name
                            0.0
                            (cl-transforms:make-3d-vector 0 0 (/ object-height 2))
                            (cl-transforms:rotation obj-on-to-object)))
         (object-to-gripper (transform gripper-frame obj-name :timeout 2))
         (target-to-gripper (cl-transforms:transform* target-to-object object-to-gripper))
         (loc (make-designator 'location 
                               (update-designator-properties 
                                `((in ,(cond
                                         ((eq arm 'left-arm) 'left-gripper)
                                         ((eq arm 'right-arm) 'right-gripper)
                                         (t nil)))
                                  (frame ,gripper-frame)
                                  (target-to-gripper ,target-to-gripper)
                                  (pose ,(transform-get-pose-stamped
                                          obj-name
                                          gripper-frame
                                          :timeout 2)))
                                (description (desig-prop-value curr-obj 'at))))))
    (format t "~%obj-on: ~a~%~%" obj-on)
    (format t "~%obj-on-to-object: ~a~%~%" obj-on-to-object)
    (format t "~%target-to-gripper: ~a~%" target-to-gripper)
    (format t "~%target-to-object: ~a~%" target-to-object)
    (format t "~%object-to-gripper: ~a~%" object-to-gripper)
    (format t "object-on-height: ~a~%" object-on-height)
    (format t "object-height: ~a~%" object-height)
    (make-designator 'object
                     (update-designator-properties 
                      `((height ,object-height)
                        (at ,loc))
                      (description obj))
                     curr-obj)))

(defun grasped-sideways (obj arm)
  (let* ((obj-name (desig-prop-value obj 'name))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (object-on (suturo-planning-pm-knowledge::call-action
                     'suturo-planning-pm-knowledge::get-static-object obj-on-name))
         (object-on-height (first (desig-prop-value object-on 'dimensions)))
         (object-on-height-half (/ object-on-height 2.0))
         (gripper-frame (get-gripper-frame arm))
         (object-on-origin (transform-get-origin obj-on-name "/map" :timeout 2))
         (object-origin (transform-get-origin obj-name "/map" :timeout 2))
         (object-height
           (* 2 (- (- (cl-transforms:z object-origin)
                      (cl-transforms:z object-on-origin))
                   object-on-height-half)))
         (object-height-half (/ object-height 2.0))
         ;;(gripper-to-obj (transform obj-name gripper-frame :timeout 2 :intents 10))
         (obj-on-to-gripper (transform gripper-frame obj-on-name :timeout 2))
         (obj-z (cl-transforms:z (transform-get-origin obj-name obj-on-name :timeout 2)))
         (gripper-z (cl-transforms:z (transform-get-origin gripper-frame obj-on-name :timeout 2)))
         (gripper-to-obj-distance (- gripper-z obj-z))
         (obj-on-to-gripper-distance-actual (cl-transforms:z (transform->origin obj-on-to-gripper)))
         (obj-on-to-gripper-distance-max (* (+ object-on-height-half
                                               object-height-half
                                               gripper-to-obj-distance)
                                            0.97)))
    (format t "object-on-height-half: ~a~%" object-on-height-half)
    (format t "object-height-half: ~a~%" object-height-half)
    (format t "gripper-to-obj-distance: ~a~%" gripper-to-obj-distance)
    (format t "obj-on-to-gripper-distance-actual: ~a~%" obj-on-to-gripper-distance-actual)
    (format t "obj-on-to-gripper-distance-max: ~a~%" obj-on-to-gripper-distance-max)
    (< obj-on-to-gripper-distance-actual obj-on-to-gripper-distance-max)))

; open-hand

(defun make-open-hand-goal (obj arm)
  (format t "make-open-hand-goal obj:~a~%" obj)
  (let* ((time (roslisp:ros-time))
         (msg-header (roslisp:make-msg "std_msgs/Header"
                                       (stamp) time
                                       (frame_id) (desig-prop-value (desig-prop-value obj 'at)  'frame)))
         (msg-action (roslisp:make-msg "suturo_manipulation_msgs/GraspingAndDrop"
                                       (header) msg-header
                                       (action) (get-grasp-constant 'grasp-action-open)))
         (msg-goal
           (roslisp:make-msg
            "suturo_manipulation_msgs/suturo_manipulation_grasping_goal"
            (header) msg-header
            (action) msg-action
            (bodypart) (roslisp:make-msg
                        "suturo_manipulation_msgs/RobotBodyPart"
                        (bodyPart) (get-body-part-constant arm)))))
    (format t "msg: ~a~%" msg-goal)
    (actionlib:make-action-goal *action-client-grasp*
      goal msg-goal)))

(defun call-open-hand-action (obj target-on)
  (format t "call-open-hand-action obj: ~a~%" obj)
  (setf *action-client-grasp* (get-action-client 'grasp))
  (let* ((in-desig (desig-prop-value (desig-prop-value (current-desig obj) 'at) 'in))
         (arm (cond
                ((eq in-desig 'left-gripper) 'left-arm)
                ((eq in-desig 'right-gripper) 'right-arm)
                (t (cpl:error 'suturo-planning-common::unhandled-body-part)))))
    (with-lost-in-resultation-workaround
      *action-client-grasp*
      (make-open-hand-goal obj arm)
      *open-timeout*
      'suturo-planning-common::drop-failed
      *action-client-grasp-server*
      *grasp-cancel-topic-type*
      *grasp-result-topic-type*
      *grasp-status-topic-type*
      :on-success-fn #'(lambda () (opening-hand-succeeded obj target-on)))))

(defun opening-hand-succeeded (obj target-on)
  (format t "Calling opening-hand-succeeded.~%")
  (format t "Updating object's on-location to: ~a~%" target-on)
  (let* ((loc-old (desig-prop-value obj 'at))
         (loc (make-designator 'location 
                               (update-designator-properties
                                `((on ,target-on)
                                  (in nil)) 
                                (description loc-old)))))
    (make-designator 'object
                     (update-designator-properties 
                      `((at ,loc))
                      (description obj))
                     obj)))

; move-arm
(defvar *action-client-move-arm* nil)
(defvar *action-client-move-arm-server* "suturo_man_move_arm_server")
(defvar *action-client-move-arm-goal* "suturo_manipulation_msgs/suturo_manipulation_moveAction")
(defvar *move-arm-cancel-topic-type*  "actionlib_msgs/GoalID")
(defvar *move-arm-result-topic-type*
  "suturo_manipulation_msgs/suturo_manipulation_moveActionResult")
(defvar *move-arm-status-topic-type* "actionlib_msgs/GoalStatusArray")

(defun make-move-arm-goal (pose-stamped in-arm)
  ;;(format t "make-move-arm-goal pose-stamped: ~a~% arm:~a~%" pose-stamped in-arm)
  (actionlib:make-action-goal *action-client-move-arm* 
    ps pose-stamped
    bodypart (roslisp:make-msg "suturo_manipulation_msgs/RobotBodyPart"
                               (bodyPart) in-arm)))

(defun call-move-arm-action (location arm)
  ;;(format t "call-move-arm-action arm:~a~%" arm)
  (setf *action-client-move-arm* (get-action-client 'move-arm))
  ;;(format t "getting frame and coords.~%")
  (let* ((frame (cond
                  ((typep location 'location-designator) (desig-prop-value location 'frame))
                  ((typep location 'cl-tf:pose-stamped) (cl-tf:frame-id location))))
         (coords (cond
                   ((typep location 'location-designator) (desig-prop-value location 'coords))
                   ((typep location 'cl-tf:pose-stamped) (cl-tf:origin location))))
         (pose (cond
                 ((typep location 'location-designator) (desig-prop-value location 'pose))
                 ((typep location 'cl-tf:pose-stamped) (cl-tf:orientation location))))
         (header-msg (roslisp:make-msg "std_msgs/Header"
                                       (stamp) (roslisp:ros-time)
                                       (frame_id) frame))
         (position-msg (roslisp:make-msg "geometry_msgs/Point"
                                         (x) (cond
                                               ((typep location 'location-designator) (first coords))
                                               ((typep location 'cl-tf:pose-stamped) (cl-tf:x coords)))
                                         (y) (cond
                                               ((typep location 'location-designator) (second coords))
                                               ((typep location 'cl-tf:pose-stamped) (cl-tf:y coords)))
                                         (z) (cond
                                               ((typep location 'location-designator) (third coords))
                                               ((typep location 'cl-tf:pose-stamped) (cl-tf:z coords)))))
         (orientation-msg (roslisp:make-msg "geometry_msgs/Quaternion"
                                            (x) (cond
                                                  ((typep location 'location-designator) (first pose))
                                                  ((typep location 'cl-tf:pose-stamped) (cl-tf:x pose)))
                                            (y) (cond
                                                  ((typep location 'location-designator) (second pose))
                                                  ((typep location 'cl-tf:pose-stamped) (cl-tf:y pose)))
                                            (z) (cond
                                                  ((typep location 'location-designator) (third pose))
                                                  ((typep location 'cl-tf:pose-stamped) (cl-tf:z pose)))
                                            (w) (cond
                                                  ((typep location 'location-designator) (fourth pose))
                                                  ((typep location 'cl-tf:pose-stamped) (cl-tf:w pose)))))
         (pose-msg (roslisp:make-msg "geometry_msgs/Pose"
                                     (position) position-msg
                                     (orientation) orientation-msg))
         (pose-stamped-msg (roslisp:make-msg "geometry_msgs/PoseStamped"
                                             (header) header-msg
                                             (pose) pose-msg)))
    ;;(format t "created helper messages.~%")
    (with-lost-in-resultation-workaround
        *action-client-move-arm*
      (make-move-arm-goal pose-stamped-msg (get-body-part-constant arm))
      *move-arm-timeout*
      'suturo-planning-common::move-arm-failed
      *action-client-move-arm-server*
      *move-arm-cancel-topic-type*
      *move-arm-result-topic-type*
      *move-arm-status-topic-type*
      :intents 1)))

; move-base

(defvar *action-client-move-base* nil)
(defvar *action-client-move-base-server* "suturo_man_move_base_server")
(defvar *action-client-move-base-goal* "suturo_manipulation_msgs/suturo_manipulation_baseAction")
(defvar *move-base-cancel-topic-type*  "actionlib_msgs/GoalID")
(defvar *move-base-result-topic-type*
  "suturo_manipulation_msgs/suturo_manipulation_baseActionResult")
(defvar *move-base-status-topic-type* "actionlib_msgs/GoalStatusArray")

(defun make-move-base-goal (pose-stamped tolerance)
  (format t "make-move-base-goal pose-stamped: ~a, tolerance: ~a~%"
          pose-stamped tolerance)
  (actionlib:make-action-goal *action-client-move-base* 
                              ps pose-stamped
                              range tolerance))

(defun call-move-base-action (pose-stamped &key (tolerance 0.025))
  (format t "call-move-base-action. pose-stamped:~a, tolerance: ~a~%"
          pose-stamped tolerance)
  (setf *action-client-move-base* (get-action-client 'move-base))
  (with-lost-in-resultation-workaround
      *action-client-move-base*
    (make-move-base-goal (cl-tf:pose-stamped->msg pose-stamped) tolerance)
    *move-base-timeout*
    'suturo-planning-common::move-base-failed
    *action-client-move-base-server*
    *move-base-cancel-topic-type*
    *move-base-result-topic-type*
    *move-base-status-topic-type*))
     
; Helper functions for actions

(defun get-gripper-state (arm)
  (suturo-planning-pm-gripper-monitor::call-action
                 'get-gripper-state
                 arm))

(defun difference (val-one val-two)
  "Calculates the difference between two values. 0 means no difference, 1 means 100% difference."
  (format t "Calling difference.~%val-one: ~a, val-two: ~a~%" val-one val-two)
  (if (< val-one val-two)
      (let ((diff (- val-two val-one)))
        (/ diff val-one))
      (let ((diff (- val-one val-two)))
        (/ diff val-one))))

(defun waiting-for-gripper (arm)
  (loop
    (format t "Waiting for gripper.~%")
    (let ((state-one (get-gripper-state arm)))
      (sleep 3)
      (let ((state-two (get-gripper-state arm)))
        (if (gripper-movement-significant? state-one state-two)
            (format t "Gripper seems to be still moving.~%")
            (progn
              (format t "Gripper seems to have stopped moving.~%")
              (return)))))))

(defun gripper-movement-significant? (val-one val-two)
  "Determines if there is a significant difference between two gripper states."
  (let ((absolute-diff (abs (- val-one val-two)))
        (relative-diff (difference val-one val-two)))
    (if (< absolute-diff 0.0001)
        (progn
          (format t "absolute-diff < 0.0001: ~a~%" absolute-diff)
          nil)
        (progn
          (format t "relative-diff > *gripper-tolerance*?: ~a~%" (> relative-diff *gripper-tolerance*))
          (> relative-diff *gripper-tolerance*)))))

(defvar *action-client* nil) 

(defun init-action-client (server goal)
  ;Initialises an action-client for the specified server and goal
  (let ((client (actionlib:make-action-client
                         server
                         goal)))
    (roslisp:ros-info (suturo-pm-manipulation action-client)
                      "Waiting for action server ~a  ..." server)
    (loop until
          (actionlib:wait-for-server client))
    (roslisp:ros-info (suturo-pm-manipulation action-client)
                      "Action client for goal ~a created ..." goal)
    client))

(defun get-action-client (type)
  "Returns an action-client for the specified server and goal"
  (cond
    ((eq type 'move-head)
     (if *action-client-move-head*
         *action-client-move-head*
         (init-action-client *action-client-move-head-server* *action-client-move-head-goal*)))
    ((eq type 'initial)
     (if *action-client-initial*
         *action-client-initial*
         (init-action-client *action-client-initial-server* *action-client-initial-goal*)))
    ((eq type 'grasp)
     (if *action-client-grasp*
         *action-client-grasp*
         (init-action-client *action-client-grasp-server* *action-client-grasp-goal*)))
    ((eq type 'move-arm)
     (if *action-client-move-arm*
         *action-client-move-arm*
         (init-action-client *action-client-move-arm-server* *action-client-move-arm-goal*)))
    ((eq type 'move-base)
     (if *action-client-move-base*
         *action-client-move-base*
         (init-action-client *action-client-move-base-server* *action-client-move-base-goal*)))
    (t nil)))

(defun get-grasp-constant (action)
  (string-downcase
   (symbol-name
    (cond
      ((eq action 'grasp-action-grasp)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:GraspingAndDrop :GRASP))
      ((eq action 'grasp-action-drop)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:GraspingAndDrop :DROP_OBJECT))
      ((eq action 'grasp-action-open)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:GraspingAndDrop :OPEN_GRIPPER))
      ((eq action 'grasp-action-above)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:GraspingAndDrop :GRASP_ABOVE))
      (t (roslisp:ros-error
          (suturo-pm-manipulation)
          "Unhandled grasp/drop action: ~a" action)
         (cpl:error 'suturo-planning-common::unhandled-grasp-action))))))

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
      ((eq body-part 'both-arms-move)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :BOTH_ARMS_MOVE))
      ((eq body-part 'head)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :HEAD))
      ((eq body-part 'left-arm-campose)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :LEFT_ARM_CAM_POSE))
      ((eq body-part 'all)
       (roslisp-msg-protocol:symbol-code
        'suturo_manipulation_msgs-msg:RobotBodyPart :ALL))
      (t (roslisp:ros-error
          (suturo-pm-manipulation)
          "Unhandled body part: ~a" body-part)
         (cpl:error 'suturo-planning-common::unhandled-body-part))))))
