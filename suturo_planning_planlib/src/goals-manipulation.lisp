(in-package :suturo-planning-planlib)

;(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.03)

(def-goal (achieve (?obj placed-gently ?loc))
     "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (format t "Placing object gently: ~a~%" ?obj)
  (let* ((obj (current-desig ?obj))
         (tgt-name (desig-prop-value ?loc 'name))
         (aaa (format t "referencing...~%"))
         (tgt-pose-stamped (reference ?loc))
         (tgt-frame (cl-tf:frame-id tgt-pose-stamped))
         (aab (format t "referencing done.~%"))
         #|
         (tgt-pose-stamped (cl-tf:make-pose-stamped
                            "table2"
                            ;;sp-planlib::*table-name*
                            0
                            (cl-transforms:make-3d-vector -0.15 0.0 0.015)
                            (cl-transforms:make-quaternion 0 0 0 1)))
         |#
         (obj-height (desig-prop-value obj 'height))
         (obj-height-vector (cl-transforms:make-3d-vector 0 0 (/ obj-height 2.0)))
         (tgt-to-obj-stamped-transform
           (cl-tf:make-stamped-transform tgt-frame
                                         "/tgt-to-obj"
                                         0                                         
                                         obj-height-vector
                                         (cl-transforms:make-identity-rotation)))
         (tgt-to-obj-transform (stamped-transform->transform tgt-to-obj-stamped-transform))
         (obj-name (desig-prop-value obj 'name))
         (gripper-frame (get-gripper-frame obj))
         (obj-to-gripper-transform (transform gripper-frame obj-name :timeout 2 :intents 10))
         (gripper-pose-stamped
           (cl-transforms:transform*
            (pose-stamped->transform tgt-pose-stamped)
            tgt-to-obj-stamped-transform
            obj-to-gripper-transform)))
    (format t "obj-height: ~a~%~%" obj-height)
    (format t "tgt-pose-stamped: ~a~%~%" tgt-pose-stamped)
    ;;(format t "tgt-to-obj-transform: ~a~%~%" tgt-to-obj-transform)
    (format t "tgt-to-obj-stamped-transform: ~a~%~%" tgt-to-obj-stamped-transform)
    (format t "obj-to-gripper-transform: ~a~%~%" obj-to-gripper-transform)
    (format t "gripper-pose-stamped: ~a~%" gripper-pose-stamped)))

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