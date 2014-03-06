(in-package :suturo-planning-planlib)

(defvar *inaccuracy-factor* 1.05)
(defvar *put-over-offset* 0.02)

(def-goal (achieve (?obj placed-gently ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (info-out (suturo planlib) "Placing object gently: ~a" ?obj)
  (sleep 1.0)
  (let* (;(pose-stamped (reference ?loc))
         (pose-stamped (cl-tf:make-pose-stamped
                        "/base_footprint" 0.0
                        (cl-transforms:make-3d-vector 0.4 0.4 0.6)
                        (cl-transforms:make-quaternion 0 0 0 1)))
         (frame (cl-tf:frame-id pose-stamped))
         (vector (cl-tf:origin pose-stamped))
         (x (cl-tf:x vector))
         (y (cl-tf:y vector))
         (z (cl-tf:z vector))
         (ad (format t "z: ~a~%" z))
         (object-dimensions (desig-prop-value ?obj 'dimensions))
         (object-max-dimension (/ (max (first object-dimensions)
                                       (second object-dimensions)
                                       (third object-dimensions))
                                  2.0))
         (object-offset (* object-max-dimension *inaccuracy-factor*))
         (ac (format t "object-offset: ~a~%" object-offset))
         (lowest-possible-z )
         (pose (suturo-planning-common:get-last-gripper-pose ?obj))
         (arm (get-holding-arm ?obj))
         (pov-frame (cond
                      ((eq arm 'left-arm) "/l_wrist_roll_link")
                      ((eq arm 'right-arm) "r_wrist_roll_link")
                      (t (cpl:error 'suturo-planning-common::unhandled-body-part))))
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
    (with-retry-counters ((lower-arm-counter 5))
      (with-failure-handling
          ((suturo-planning-common::move-arm-failed (f)
             (declare (ignore f))
             (format t "Failed to lower arm.~%.")
             (setf alternate-z (+ z 0.01))
             (do-retry lower-arm-counter
               (format t "Trying again. This time a little bit higher. z: ~a~%" z)
               (retry))))
        (with-designators
            ((location (location `((frame ,frame)
                                   (coords (,alternate-x
                                            ,alternate-y
                                            ,alternate-z))
                                   (pose ,pose)))))
          (achieve `(arm-at ,arm ,location)))))
           #|
    (with-retry-counters ((place-retry-counter 2))
      (let* ((arm (get-holding-arm ?obj)))
        (with-failure-handling
            ((suturo-planning-common::place-failed (f)
               (declare (ignore f))
               (error-out (suturo planlib) "Failed to place object gently on given location.")
               (sleep 2.0)
               (do-retry place-retry-counter
                 (info-out (suturo planib) "Trying again.")
                 (sleep 1.0)
                 (retry))))
          (with-designators ((open-hand (action `((to open-hand)
                                                  (obj ,?obj)))))
            (perform open-hand)))))
    |#
    (info-out (suturo planlib) "Droped, object")))


;              (perform (make-designator 'action `((to move-arm) (loc ,loc-des) (arm ,arm))))
