(in-package :suturo-planning-planlib)

(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.02)

(def-goal (achieve (?obj placed-gently ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (format t "Placing object gently: ~a~%" ?obj)
  (let* (;(pose-stamped (reference ?loc))
         
         (pose-stamped (cl-tf:make-pose-stamped
                        "/map" 0.0
                        (cl-transforms:make-3d-vector -0.9595373 1.2592965 0.9259399)
                        (cl-transforms:make-quaternion 0 0 0 1)))
         
         (ag (format t "pose-stamped: ~a~%" pose-stamped))
         (frame (cl-tf:frame-id pose-stamped))
         (vector (cl-tf:origin pose-stamped))
         (x (cl-tf:x vector))
         (y (cl-tf:y vector))
         (z (cl-tf:z vector))
         (ad (format t "z: ~a~%" z))
         (object-dimensions (desig-prop-value ?obj 'dimensions))
         (object-max-dimension (/ (maximum object-dimensions) 2.0))
         (object-second-max-dimension (/ (second (sort object-dimensions #'>)) 2.0))
         (object-offset object-max-dimension)
         (ac (format t "object-offset: ~a~%" object-offset))
         (pose (suturo-planning-common:get-last-gripper-pose ?obj))
         (arm (get-holding-arm ?obj))
         (obj-name (desig-prop-value ?obj 'name))
         (offset-loc (calc-gripper-offset arm obj-name))
         (alternate-x (+ x (cl-transforms:x offset-loc)))
         (alternate-y (+ y (cl-transforms:y offset-loc)))
         (alternate-z (+ z object-offset (cl-transforms:z offset-loc)))
         (ab (format t "offset-loc: ~a~%" offset-loc))
         (aa (format t "alternate-z: ~a~%" alternate-z))
         (alternate-loc (make-designator
                         'location `((frame ,frame)
                                     (coords (,alternate-x
                                              ,alternate-y
                                              ,(+ alternate-z
                                                  *put-over-offset*)))
                                     (pose ,pose)))))
    (format t "Created variables~%")
    (format t "Moving ~a over given location: ~a~%" arm alternate-loc)
    (achieve `(arm-at ,arm ,alternate-loc))
    (format t "Lowering arm...~%")
    (let* ((guessed-z (+ z object-max-dimension (cl-transforms:z offset-loc))))
      (with-retry-counters ((lower-arm-counter 3))
        (with-failure-handling
            ((suturo-planning-common::move-arm-failed (f)
               (declare (ignore f))
               (format t "Failed to lower arm.~%.")
               (setf guessed-z (+ guessed-z 0.01))
               (do-retry lower-arm-counter
                 (format t "Trying again. This time a little bit higher.guessed-z: ~a~%" guessed-z)
                 (retry))
               (format t "Trying with second-max-dimension~%")
               (let* ((guessed-z (+ z object-second-max-dimension (cl-transforms:z offset-loc))))
                 (with-retry-counters ((lower-arm-counter 3))
                   (with-failure-handling
                       ((suturo-planning-common::move-arm-failed (f)
                          (declare (ignore f))
                          (format t "Failed to lower arm.~%.")
                          (setf guessed-z (+ guessed-z 0.01))
                          (do-retry lower-arm-counter
                            (format t "Trying again. This time a little bit higher.guessed-z: ~a~%" guessed-z)
                            (retry))
                          ))
                     (with-designators
                         ((location (location `((frame ,frame)
                                                (coords (,alternate-x
                                                         ,alternate-y
                                                         ,guessed-z))
                                                (pose ,pose)))))
                       (achieve `(arm-at ,arm ,location))))))))
          (with-designators
              ((location (location `((frame ,frame)
                                     (coords (,alternate-x
                                              ,alternate-y
                                              ,guessed-z))
                                     (pose ,pose)))))
            (achieve `(arm-at ,arm ,location))))))
    (achieve `(emtpy-hand ?obj))
    (info-out (suturo planlib) "Droped, object~%")))