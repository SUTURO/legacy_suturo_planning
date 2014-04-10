(in-package :suturo-planning-planlib)

;(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.03)

(def-goal (achieve (?obj placed-gently-reference ?loc))
    (let* ((pose-stamped (reference ?loc))
           (target-on (desig-prop-value ?loc 'on))
           (vector (cl-tf:origin pose-stamped))
           (location (make-designator
                      'location `((frame ,(cl-tf:frame-id pose-stamped))
                                  (coords (,(cl-tf:x vector)
                                           ,(cl-tf:y vector)
                                           ,(cl-tf:z vector)))
                                  (on ,target-on)))))
      (achieve `(,?obj placed-gently ,location))))

(def-goal (achieve (?obj placed-gently ?loc))
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
         (offset-loc (calc-gripper-offset arm obj-name))
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