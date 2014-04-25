(in-package :suturo-planning-planlib)

(def-goal (achieve (know-unknown-object ?obj))
  "The passed object `?obj' has to be positioned at different angles (90°, 135°, 225°, 270°, 315°, 405°) to the PR2's view axis. In every position the object has to be released from the holding gripper and the PR2 has to scan/take a picture of the object.

Initially the PR2 has to be positioned in front of the object. The object has to be placed on a table/surface with with enough space to manipulate the object's position/pose."
  (format t "Getting to know unknown object.~%")
  (let* ((obj (current-desig ?obj))
         (obj-name (desig-prop-value ?obj 'name))
         (obj-in-base-link-origin (transform-get-origin obj-name "/base_link" :timeout 2))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (obj-on (suturo-planning-pm-knowledge::call-action
                      'suturo-planning-pm-knowledge::get-static-object obj-on-name))
         (arm 'right-arm)
         (gripper-frame (get-gripper-frame arm))
         (rotations '(45.0 90.0 45.0 45.0 90.0)))
    (format t "obj-on-name: ~a~%" obj-on-name)
    (format t "obj-on: ~a~%" obj-on)
    (format t "Moving in front of unknown object.~%")
#|
    (achieve `(robot-at ,(cl-tf:make-pose-stamped "/base_link" 0.0
                                                  (cl-tf:make-3d-vector 0 (cl-transforms:y obj-in-base-link-origin) 0)
                                                  (cl-tf:make-identity-rotation))))
|#
    (format t "Grasping unknown object.~%")
    (with-retry-counters ((grasp-obj-counter 1))
      (with-failure-handling
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (format t "Failed to grasp object.~%")
             (do-retry grasp-obj-counter
               (format t "Trying again.~%")
               (retry))))
        (achieve `(object-in-hand ,obj ,arm sp-manipulation::grasp-action-above 10))))
    (let* ((gripper-origin (transform-get-origin gripper-frame "/base_link" :timeout 2))
           (initial-lifted-location
             (cond
               ((eq arm 'left-arm)
                (cl-tf:make-pose-stamped "/base_link" 0.0
                                         (cl-tf:v+ gripper-origin
                                                   (cl-tf:make-3d-vector 0 0 0.02))
                                         (cl-tf:make-quaternion 0.5000285573285219d0
                                                                0.4990192438614538d0
                                                                -0.5000920419769876d0
                                                                0.500858448728967d0)))
               ((eq arm 'right-arm)
                (cl-tf:make-pose-stamped "/base_link" 0.0
                                         (cl-tf:v+ gripper-origin
                                                   (cl-tf:make-3d-vector 0 0 0.02))
                                         (cl-tf:make-quaternion -0.4979847991023766d0
                                                                0.5021521617962537d0
                                                                0.49771687296865463d0
                                                                0.5021277333792858d0)))
              (t (cpl:error 'suturo-planning-common::unhandled-body-part)))))
      (format t "Taking initial scan pose.~%")
      (format t "Lifting and rotating arm.~%")
      (achieve `(arm-at ,arm ,initial-lifted-location))
      (format t "Lowering arm.~%")
      (let ((x-val 0.02))
        (with-retry-counters ((move-arm-counter 1))
          (with-failure-handling
              ((suturo-planning-common::move-arm-failed (e)
                 (declare (ignore e))
                 (format t "Failed to lower arm.~%")
                 (do-retry move-arm-counter
                   (format t "Trying again. This time a little bit higher.~%")
                   (setf x-val 0.01)
                   (retry))
                 (return)))
            (achieve `(arm-at ,arm
                              ,(pose->pose-stamped
                                (cl-transforms:transform (transform gripper-frame "/base_link" :timeout 2)
                                                         (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                                  (cl-tf:make-3d-vector x-val 0 0)
                                                                                  (cl-tf:make-identity-rotation)))
                                "/base_link"))))))
      (achieve `(empty-hand ,obj ,(make-designator 'object `((name ,obj-on-name)))))
      (achieve `(home-pose ,arm))
      (format t "Updating planning scene.~%")
      (achieve `(home-pose head))
      ;;(exec::update-on-table)
      (format t "~%~%### Knowledge is doing its magic now... ###~%~%")
      (sleep 3)
      (loop for rotation in rotations
            do (format t "Grasping unknown object.~%")
               (with-retry-counters ((grasp-obj-counter 1))
                 (with-failure-handling
                     ((suturo-planning-common::grasping-failed (f)
                        (declare (ignore f))
                        (format t "Failed to grasp object.~%")
                        (do-retry grasp-obj-counter
                          (format t "Trying again.~%")
                          (retry))))
                   (achieve `(object-in-hand ,obj ,arm sp-manipulation::grasp-action-above 10))))
               (let* ((gripper-in-base-link (transform gripper-frame "/base_link" :timeout 2))
                      (lifted-location
                        (pose->pose-stamped
                         (cl-transforms:transform
                          gripper-in-base-link
                          (cl-tf:make-pose-stamped gripper-frame 0.0
                                                   (cl-tf:make-3d-vector -0.02 0 0)
                                                   (cl-transforms-euler-degree->quaternion :ax rotation)))
                         "/base_link")))
                 (format t "Lifiting arm and rotating object in ~a ~a degrees.~%" arm rotation)
                 (achieve `(arm-at ,arm ,lifted-location))
                 (format t "Lowering arm.~%")
                 (let* ((x-val 0.02))
                   (with-retry-counters ((move-arm-counter 1))
                     (with-failure-handling
                         ((suturo-planning-common::move-arm-failed (e)
                            (declare (ignore e))
                            (format t "Failed to lower arm.~%")
                            (do-retry move-arm-counter
                              (format t "Trying again. This time a little bit higher.~%")
                              (setf x-val 0.01)
                              (retry))
                            (return)))
                       (achieve `(arm-at ,arm
                                         ,(pose->pose-stamped
                                           (cl-transforms:transform
                                            gripper-in-base-link
                                            (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                     (cl-tf:make-3d-vector x-val 0 0)
                                                                     (cl-transforms-euler-degree->quaternion :ax rotation)))
                                           "/base_link"))))))
                 (achieve `(empty-hand ,obj ,(make-designator 'object `((name ,obj-on-name)))))
                 (achieve `(home-pose ,arm))
                 (format t "Updating planning scene.~%")
                 (achieve `(home-pose head))
                 ;;(exec::update-on-table)
                 (format t "~%~%### Knowledge is doing its magic now... ###~%~%")
                 (sleep 3))))))