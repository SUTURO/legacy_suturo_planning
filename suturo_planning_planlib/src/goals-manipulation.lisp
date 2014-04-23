(in-package :suturo-planning-planlib)

;(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.03)

(def-goal (achieve (?obj placed-gently ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (format t "Placing object gently: ~a~%" ?obj)
  (with-retry-counters ((place-object-counter 2))
    (with-failure-handling
        ((suturo-planning-common::move-arm-failed (e)
           (declare (ignore e))
           (format t "Failed to move arm to referenced location.~%")
           (do-retry place-object-counter
             (format t "Referencing again.~%")
             (retry))))
      (let* ((obj (current-desig ?obj))
             (tgt-on-name (desig-prop-value ?loc 'name))
             (tgt-on (suturo-planning-pm-knowledge::call-action
                      'suturo-planning-pm-knowledge::get-static-object tgt-on-name))
             ;;(tgt-on-height (first(desig-prop-value tgt-on 'dimensions)))
             (arm (get-holding-arm ?obj))
             (tgt-pose-stamped (cond
                                 ((typep ?loc 'location-designator) (reference ?loc))
                                 ((typep ?loc 'cl-tf:pose-stamped) ?loc)
                                 (t (cpl:error 'suturo-planning-common::unhandled-value))))
             (tgt-frame (cl-tf:frame-id tgt-pose-stamped))
             (tgt-to-gripper-tmp (desig-prop-value-concat obj '(at target-to-gripper)))
             (tgt-to-tgt-on (transform tgt-frame tgt-on-name :timeout 2 :intents 10))
             (tgt-to-gripper (cl-tf:make-transform
                              (cl-transforms:translation tgt-to-gripper-tmp)
                              (cl-transforms:make-identity-rotation)))
             ;;(cl-transforms:rotation tgt-to-gripper-tmp)))
             (tgt-in-tgt-on (cl-transforms:transform tgt-to-tgt-on tgt-pose-stamped))
             (tgt-in-tgt-on-stamped (cl-tf:make-pose-stamped tgt-on-name
                                                             0
                                                             (cl-tf:origin tgt-in-tgt-on)
                                                             (cl-tf:orientation tgt-in-tgt-on)))
             (gripper-pose (cl-transforms:transform tgt-to-gripper tgt-in-tgt-on-stamped))
             (gripper-pose-stamped (cl-tf:make-pose-stamped tgt-on-name
                                                            0
                                                            (cl-tf:origin gripper-pose)
                                                            (cl-transforms:rotation tgt-to-gripper-tmp))))
        (format t "~%tgt-pose-stamped: ~a~%" tgt-pose-stamped)
        (format t "~%tgt-to-tgt-on: ~a~%" tgt-to-tgt-on)
        (format t "~%tgt-in-tgt-on-stamped: ~a~%" tgt-in-tgt-on-stamped)
        (format t "~%tgt-to-gripper: ~a~%" tgt-to-gripper)
        ;;(format t "~%gripper-pose: ~a~%" gripper-pose)
        (format t "~%gripper-pose-stamped: ~a~%" gripper-pose-stamped)
        (let ((z-offset 0))
          (with-retry-counters ((move-arm-counter 2))
            (with-failure-handling
                ((suturo-planning-common::move-arm-failed (e)
                   (declare (ignore e))
                   (format t "Failed to move arm.~%")
                   (do-retry move-arm-counter
                     (format t "Trying again. This time a little bit higher.~%")
                     (setf z-offset (+ z-offset 0.015))
                     (retry))))
              (with-designators
                  ((location (location `((frame ,(cl-tf:frame-id gripper-pose-stamped))
                                         (coords (,(cl-transforms:x (cl-tf:origin gripper-pose-stamped))
                                                   ,(cl-transforms:y (cl-tf:origin gripper-pose-stamped))
                                                   ,(+ (cl-transforms:z (cl-tf:origin gripper-pose-stamped))
                                                       z-offset)))
                                         (pose (,(cl-transforms:x (cl-tf:orientation gripper-pose-stamped))
                                                 ,(cl-transforms:y (cl-tf:orientation gripper-pose-stamped))
                                                 ,(cl-transforms:z (cl-tf:orientation gripper-pose-stamped))
                                                 ,(cl-transforms:w (cl-tf:orientation gripper-pose-stamped))))))))
                (achieve `(arm-at ,arm ,location))
                (achieve `(empty-hand ,?obj ,tgt-on))
                (info-out (suturo planlib) "Placed object gently as you requested, master!~%")))))))))