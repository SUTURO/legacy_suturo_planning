(in-package :suturo-planning-planlib)

(def-goal (achieve (?obj know-unknown-object ?loc))
  "The passed object `?obj' has to be positioned at different angles (90°, 135°, 225°, 270°, 315°, 405°) to the PR2's view axis. In every position the object has to be released from the holding gripper and the PR2 has to scan/take a picture of the object.

Initially the PR2 has to be positioned in front of the object. The object has to be placed on a table/surface with with enough space to manipulate the object's position/pose."
  (format t "Getting to know unknown object.~%")
  (let* ((arm 'left-arm)
         ;;(arm (get-best-arm ?obj))
         (initial-location
           (cond
             ((eq arm 'left-arm)
              (make-designator
               'location
               `((frame "/base_link")
                 (coords
                  ,(transform-coords-to-frame
                    (desig-prop-value ?loc 'frame)
                    "/base_link"
                    (desig-prop-value ?loc 'coords)))
                 (pose (0.5000285573285219d0 0.4990192438614538d0 -0.5000920419769876d0 0.500858448728967d0)))))
             ((eq arm 'left-arm)
              (make-designator
               'location
               `((frame "/base_link")
                 (coords
                  ,(transform-coords-to-frame
                    (desig-prop-value ?loc 'frame)
                    "/base_link"
                    (desig-prop-value ?loc 'coords)))
                 (pose (-0.4979847991023766d0 0.5021521617962537d0 0.49771687296865463d0 0.5021277333792858d0)))))
             (t (cpl:error 'suturo-planning-common::unhandled-body-part))))
         (rotations '(45.0 90.0 45.0 45.0 90.0)))
    (achieve `(object-in-hand ,?obj ,arm))
    (format t "Taking initial scan-pose.~%")
    (achieve `(arm-at ,arm ,initial-location))
    (loop for rotation in rotations
          do (format t "Rotating object in ~a ~a degrees.~%" arm rotation)
             (let ((new-location
                     (make-designator
                      'location
                      `((frame ,(get-gripper-frame arm))
                        (coords (0 0 0))
                        (pose ,(cl-transforms-euler-degree->quaternion-as-list :ax rotation))))))
               (achieve `(arm-at ,arm ,new-location))))))


  ;;(achieve `(,?obj placed-gently ,?loc))





#|
(with-retry-counters ((grasp-obj-counter 2))
      (with-failure-handling
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (format t "Failed to grasp object.~%")
             (do-retry grasp-obj-counter
               (format t "Trying again.~%")
               (retry))))
        (achieve `(object-in-hand ,?obj ,arm))))
|#