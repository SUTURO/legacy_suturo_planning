(in-package :suturo-planning-planlib)

 ;;(define-policy dont-drop-object (arm))
#|
  "Monitors the griper of the given arm and checks if it closes
   completely"
  (:init ;;(perform (make-designator 'action 
         ;;                          '((to start-monitoring-gripper)))))
   (sp-gripper-monitor::call-action 'start-monitoring-gripper))
  (:check (sleep* 0.5)
          ;;(perform (make-designator 'action 
          ;;                          `((to gripper-is-closed)
          ;;                            (arm ,arm)))))
          (sp-gripper-monitor::call-action 'gripper-is-closed arm))
  (:recover ;;(perform (make-designator 'action 
            ;;                          '((to end-monitoring-gripper))))
            (sp-gripper-monitor::call-action 'end-monitoring-gripper)
            (cpl:fail 'dropped-object))
  (:clean-up ;;(perform (make-designator 'action 
             ;;                          '((to end-monitoring-gripper))))))
   (sp-gripper-monitor::call-action 'end-monitoring-gripper))) |#
 
(defun publish-visualization-marker (pose-stamped &key type)
  (let ((origin (cl-tf:origin pose-stamped))
        (orientation (cl-tf:orientation pose-stamped)))
    (publish (advertise "/visualization_marker" "visualization_msgs/Marker")
             (make-message "visualization_msgs/Marker"
                           (frame_id header) "/map"
                           (stamp header)  (ros-time)
                           ns "my_ns"
                           id 0
                           type (if type type 0)
                           action 0
                           (x position pose) (cl-tf:x origin)
                           (y position pose) (cl-tf:y origin)
                           (z position pose) 1.5
                           (x orientation pose) (cl-tf:x orientation)
                           (y orientation pose) (cl-tf:y orientation)
                           (z orientation pose) (cl-tf:z orientation)
                           (w orientation pose) (cl-tf:w orientation)
                           (x scale) 0.3
                           (y scale) 0.15
                           (z scale) 0.15
                           (a color) 1
                           (r color) 1
                           (g color) 0
                           (b color) 1
                           lifetime 50))))

(defun publish-visualization-marker2 (loc)
  (let ((origin (desig-prop-value loc 'coords))
        (orientation (desig-prop-value loc 'pose)))
    (publish (advertise "/visualization_marker" "visualization_msgs/Marker")
             (make-message "visualization_msgs/Marker"
                           (frame_id header) "/map"
                           (stamp header)  (ros-time)
                           ns "my_ns"
                           id 0
                           type 0
                           action 0
                           (x position pose) (first origin)
                           (y position pose) (second origin)
                           (z position pose) (third origin)
                           (x orientation pose) (first orientation)
                           (y orientation pose) (second orientation)
                           (z orientation pose) (third orientation)
                           (w orientation pose) (fourth orientation)
                           (x scale) 0.2
                           (y scale) 0.15
                           (z scale) 0.15
                           (a color) 1
                           (r color) 0
                           (g color) 1
                           (b color) 1
                           lifetime 50))))
  
(def-goal (achieve (robot-at ?loc))
  "Moves the robot to a position described by ?loc"
  (with-failure-handling 
      ((move-base-failed (f)
         (declare (ignore f))
         (retry-with-next-solution ?loc)))
    (format t "Performing move~%")
    (let ((pose-stamped (reference ?loc)))
      (publish-visualization-marker pose-stamped)
      (sp-manipulation::call-action 'move-base pose-stamped))))
    ;(perform (make-designator 'action `((to move-base) 
    ;                                    (pose ,(reference ?loc)))))))
 
                     
(def-goal (achieve (home-pose))
  "Moves the robot in the initial position"
  (info-out (suturo planlib) "Taking hoe pose")
  (achieve '(home-pose both-arms))
  (achieve '(home-pose head)))

(def-goal (achieve (home-pose ?body-part))
  "Brings the specified robotpart in the home pose"
  (with-retry-counters ((retry-counter 2))
     (with-designators ((take-home-pose (action 
                                          `((to take-pose)
                                            (pose initial)
                                            (body-part ,?body-part)))))
       (with-failure-handling 
           ((pose-not-reached (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to bring ~a into the initial pose"
                         ?body-part)
              (do-retry retry-counter
                (info-out (suturo planlib) "Trying again")
                (retry))))
         (perform take-home-pose)))))

(def-goal (achieve (arm-at ?arm ?loc))
  "Brings the specified arm `?arm' to the location `?loc'"
  (format t "Achieving ~a at ~a~%" ?arm ?loc)
  (with-retry-counters ((retry-counter 1))
     (with-designators ((move (action 
                               `((to move-arm)
                                 (loc ,?loc)
                                 (arm ,?arm)))))
       (with-failure-handling 
           ((move-arm-failed (f)
              (declare (ignore f))
              (error-out (suturo planlib) 
                         "Failed to move ~a to location ~a~%"
                         ?arm
                         ?loc)
              (do-retry retry-counter
                (info-out (suturo planlib) "Trying again")
                (retry))))
         (publish-visualization-marker2 ?loc)
         (perform move)))))

  

(def-goal (achieve (in-gripper ?obj))
  "Grasps the object with one gripper"
  (when (not (eql-or (desig-prop-value (desig-prop-value ?obj 'at) 'in)
                     'left-gripper
                     'right-gripper))
    (achieve '(home-pose both-arms))
    (let ((arm 'left-arm)
          (loc-to-reach (make-designator 'location 
                                         `((to reach) (obj ,?obj))))
          (retry-counter 0))
      (format t "Grasp position0~%")
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (incf retry-counter)
             (if (< retry-counter 8)
                 (cond
                   ((eql (mod retry-counter 2) 1)
                    (error-out (suturo planlib) 
                               "Grasping failed retry with other arm")
                    (achieve `(home-pose ,arm))
                    (setf arm (switch-arms arm))
                    (retry))
                   ((eql (mod retry-counter 2) 0)
                    (format t "Move around~%")
                    (retry-with-next-solution loc-to-reach))))
             (achieve `(home-pose ,arm))))
        (format t "Grasp position~%")
        (achieve `(robot-at ,loc-to-reach))
        (format t "Grasping~%")
        (achieve `(object-in-hand ,?obj ,arm))))))
  

(def-goal (achieve (object-in-hand ?obj ?arm))
  "Takes the object in one hand"
  (info-out (suturo planlib) "Grasping object with ~a" ?arm)
  (with-designators ((grasp-obj (action `((to grasp)
                                          (obj ,?obj)
                                          (arm ,?arm))))
                     (monitor-gripper (action `((to monitor-gripper)
                                                (arm ,?arm)))))
    (perform grasp-obj)
    (if (perform monitor-gripper) 
        (cpl:fail 'grasping-failed))))
  

(def-goal (achieve (hand-over ?obj ?arm))
  "Moves the selected hand over the object"
  (info-out (suturo planlib) "Moving ~a" ?arm)
  (with-retry-counters ((move-retry-counter 1))
    (with-failure-handling
        ((move-arm-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to move arm")
           (do-retry move-retry-counter
             (info-out (suturo planlib) "Trying again.")
             (retry))))
      (let* ((loc (desig-prop-value ?obj 'at))
             (loc-pose-stamp (reference loc))
             (loc-origin (cl-tf:origin loc-pose-stamp))
             (coords-over `(,(cl-tf:x loc-origin) ,(cl-tf:y loc-origin) 
                                                  ,(+ (cl-tf:z loc-origin) 0.3)))
             (loc-over (make-designator 'location 
                                        (update-designator-properties 
                                         `((coords ,coords-over)
                                           (frame "/map")
                                           (pose (1 0 0 0)))
                                         (description loc)))))
        (achieve `(arm-at ,?arm ,loc-over))))))

(def-goal (achieve (empty-hand ?obj ?target-on))
  "Opens the hand of the given arm"
  (info-out (suturo planlib) "Opening, hand")
  (sleep 1.5)
  (with-retry-counters ((open-retry-counter 1))
    (with-failure-handling
        ((drop-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to open hand")
           (sleep 1.5)
           (do-retry open-retry-counter
             (info-out (suturo planib) "Trying again.")
             (sleep 1.5)
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (obj ,?obj)
                                              (target-on ,?target-on)))))
        ;(perform open-hand))))
        (sp-manipulation::call-open-hand-action ?obj ?target-on))))      
  (info-out (suturo planlib) "Dropped, object"))

(def-goal (achieve (object-in-box ?obj ?box))
  "The object should be in the box"
  (let ((arm (get-best-arm ?box)))
    (achieve `(object-in-hand ,?obj ,arm))
    (with-failure-handling
        ((move-arm-failed (f)
           (declare (ignore f))
           (achieve `(empty-hand ,?obj))
           (achieve '(home-pose))))
      ;;(with-named-policy 'dont-drop-object (arm)
        (achieve `(hand-over ,?box ,arm)))
    (achieve `(empty-hand ,?obj))
    (with-designators ((placed-object-in-box 
                          (action `((to placed-object-in-box) 
                                    (obj ,?obj) 
                                    (container ,?box)))))
      (perform placed-object-in-box))))

(def-goal (achieve (face-loc ?loc))
  "Let the Head point to the given Location"
  (with-retry-counters ((head-retry-counter 3))
    (with-failure-handling
        ((simple-plan-failure (f)
           (declare (ignore f))
           (do-retry head-retry-counter
             (retry))))
      (with-designators ((move-head (action `((to move-head)
                                              (loc ,?loc)))))
        (perform move-head)))))
