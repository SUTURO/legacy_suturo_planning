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
             (tgt-pose-stamped (reference ?loc))
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

;;(achieve `(,?obj placed-gently ,location))

(def-goal (achieve (?obj placed-gently-location ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (format t "Placing object gently: ~a~%" ?obj)
  (let* ((target-on (desig-prop-value ?loc 'on))
         (coords (desig-prop-value ?loc 'coords))
         (quat (desig-prop-value ?loc 'pose))
         (pose (cond
                 (quat quat)
                 (t (suturo-planning-common:get-last-gripper-pose ?obj))))
         (frame (desig-prop-value ?loc 'frame))
         (x (first coords))
         (y (second coords))
         (z (third coords))
         (object-dimensions (desig-prop-value ?obj 'dimensions))
         (object-max-dimension (/ (maximum object-dimensions) 2.0))
         (object-second-max-dimension (/ (second
                                          (stable-sort
                                           (copy-list object-dimensions)
                                           #'>))
                                         2.0))
         (object-offset object-max-dimension)
         (arm (get-holding-arm ?obj))
         (obj-name (desig-prop-value ?obj 'name))
         (obj-location (desig-prop-value (current-desig ?obj) 'at))
         (offset-loc (desig-prop-value obj-location 'in-offset))
         (sdgsdg (format t "offset-loc: ~a~%" offset-loc))
         (alternate-x (+ x (cl-transforms:x offset-loc)))
         (alternate-y (+ y (cl-transforms:y offset-loc)))
         (alternate-z (+ z object-offset (cl-transforms:z offset-loc)))
         (alternate-loc (make-designator
                         'location `((frame ,frame)
                                     (coords (,alternate-x
                                              ,alternate-y
                                              ,(+ alternate-z
                                                  *put-over-offset*)))
                                     (pose ,pose)))))
    (format t "Created variables~%")
    (format t "z: ~a~%" z)
    (format t "object-offset: ~a~%" object-offset)
    (format t "offset-loc: ~a~%" offset-loc)
    (format t "alternate-z: ~a~%" alternate-z)
    #|
    (format t "Moving ~a over given location: ~a~%" arm alternate-loc)
    (achieve `(arm-at ,arm ,alternate-loc))
    |#
    (format t "Lowering arm. Guessing z with object-second-max-dimension (horizontal).~%")
    (let* ((guessed-z (+ z object-second-max-dimension (cl-transforms:z offset-loc))))
      (with-failure-handling
          ((suturo-planning-common::move-arm-failed (g)
             (declare (ignore g))
             (format t "finally loosing object.~%")
             (achieve `(empty-hand ,?obj ,target-on))
             (return)))
        (with-retry-counters ((lower-arm-counter 3))
          (with-failure-handling
              ((suturo-planning-common::move-arm-failed (f)
                 (declare (ignore f))
                 (format t "Failed to lower arm.~%.")
                 (setf guessed-z (+ guessed-z 0.01))
                 (do-retry lower-arm-counter
                   (format t "Trying again. This time a little bit higher. guessed-z: ~a~%" guessed-z)
                   (retry))
                 (format t "Trying with object-max-dimension (vertical).~%")
                 (let* ((guessed-z (+ z object-max-dimension (cl-transforms:z offset-loc))))
                   (with-retry-counters ((lower-arm-counter-2 3))
                     (with-failure-handling
                         ((suturo-planning-common::move-arm-failed (e)
                            (declare (ignore e))
                            (format t "Failed to lower arm.~%.")
                            (setf guessed-z (+ guessed-z 0.01))
                            (do-retry lower-arm-counter-2
                              (format t "Trying again. This time a little bit higher. guessed-z: ~a~%" guessed-z)
                              (retry))))
                       (with-designators
                           ((location (location `((frame ,frame)
                                                  (coords (,alternate-x
                                                           ,alternate-y
                                                           ,guessed-z))
                                                  (pose ,pose)))))
                         (achieve `(arm-at ,arm ,location))
                         (achieve `(empty-hand ,?obj ,target-on))))))))
            (with-designators
                ((location (location `((frame ,frame)
                                       (coords (,alternate-x
                                                ,alternate-y
                                                ,guessed-z))
                                       (pose ,pose)))))
              (achieve `(arm-at ,arm ,location))
              (achieve `(empty-hand ,?obj ,target-on)))))))
      (info-out (suturo planlib) "Placed object gently as you requested, master!~%")))