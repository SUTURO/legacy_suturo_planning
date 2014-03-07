(in-package :suturo-planning-planlib)

;(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.03)

(def-goal (achieve (?obj placed-gently ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (format t "Placing object gently: ~a~%" ?obj)
  (let* (;(pose-stamped (reference ?loc))
         (pose-stamped (cl-tf:make-pose-stamped
                        ;"/map" 0.0
                        ;(cl-transforms:make-3d-vector -0.9595373 1.2592965 0.8609399)
                        "/base_footprint" 0.0
                        (cl-transforms:make-3d-vector 0.5 -0.1 0.63)
                        (cl-transforms:make-quaternion 0 0 0 1)))
         (frame (cl-tf:frame-id pose-stamped))
         (vector (cl-tf:origin pose-stamped))
         (x (cl-tf:x vector))
         (y (cl-tf:y vector))
         (z (cl-tf:z vector))
         (object-dimensions (desig-prop-value ?obj 'dimensions))
         (object-max-dimension (/ (maximum object-dimensions) 2.0))
         (object-second-max-dimension (/ (second
                                          (stable-sort
                                           (copy-list object-dimensions)
                                           #'>))
                                         2.0))
         (object-offset object-max-dimension)
         (pose (suturo-planning-common:get-last-gripper-pose ?obj))
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
    (format t "pose-stamped: ~a~%" pose-stamped)
    (format t "z: ~a~%" z)
    (format t "object-offset: ~a~%" object-offset)
    (format t "offset-loc: ~a~%" offset-loc)
    (format t "alternate-z: ~a~%" alternate-z)    
    (format t "Moving ~a over given location: ~a~%" arm alternate-loc)
    (achieve `(arm-at ,arm ,alternate-loc))
    (format t "Lowering arm. Guessing z with object-second-max-dimension.~%")
    (let* ((guessed-z (+ z object-second-max-dimension (cl-transforms:z offset-loc))))
      (with-failure-handling
          ((suturo-planning-common::move-arm-failed (g)
             (declare (ignore g))
             (format t "finally loosing object.~%")
             (achieve `(emtpy-hand ?obj))
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
                 (format t "Trying with object-max-dimension~%")
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
                         (achieve `(emtpy-hand ?obj))))))))
            (with-designators
                ((location (location `((frame ,frame)
                                       (coords (,alternate-x
                                                ,alternate-y
                                                ,guessed-z))
                                       (pose ,pose)))))
              (achieve `(arm-at ,arm ,location))
              (achieve `(emtpy-hand ?obj)))))))
      (info-out (suturo planlib) "Placed object gently as you requested, master!~%")))