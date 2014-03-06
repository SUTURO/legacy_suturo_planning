(in-package :suturo-planning-planlib)

(defvar *inaccuracy-factor* 1.05)

(def-goal (achieve (?obj placed-gently ?loc))
  "Places an object `?obj' on a given location `loc'.
The location has to be reachable without having to move the robot's base."
  (info-out (suturo planlib) "Placing object gently: ~a" ?obj)
  (sleep 1.0)
  (let* (;(pose-stamped (reference ?loc))
         (pose-stamped (cl-tf:make-pose-stamped
                        "/base_link" 0.0
                        (cl-transforms:make-3d-vector 0.5 0.35 0.6)
                        (cl-transforms:make-quaternion 0 0 0 1)))
         (frame (cl-tf:frame-id pose-stamped))
         (vector (cl-tf:origin pose-stamped))
         (x (cl-tf:x vector))
         (y (cl-tf:y vector))
         (z (cl-tf:z vector))
         (object-dimensions (desig-prop-value ?obj 'dimensions))
         (object-max-dimension (/ (max (first object-dimensions)
                                    (second object-dimensions)
                                    (third object-dimensions)) 2.0))
         (alternate-z (+ z (* object-max-dimension *inaccuracy-factor*)))
         (aa (format t "alternate-z: ~a~%" alternate-z))
         (lowest-possible-z )
         (pose (suturo-planning-common:get-last-gripper-pose ?obj))
         (arm (get-holding-arm ?obj))
         (pov-frame (cond
                      ((eq arm 'left-arm) "/l_wrist_roll_link")
                      ((eq arm 'right-arm) "r_wrist_roll_link")
                      (t (cpl:error 'suturo-planning-common::unhandled-body-part))))
         (obj-name (desig-prop-value ?obj 'name))
         (offset-loc (calc-gripper-offset arm obj-name))
         (ab (format t "offset-loc: ~a~%" offset-loc))
         (alternate-loc (make-designator
                         'location `((frame ,frame)
                                     (coords (,(+ x (cl-transforms:x offset-loc))
                                               ,(+ y (cl-transforms:y offset-loc))
                                               ,(+ alternate-z (cl-transforms:z offset-loc))))
                                     (pose ,pose)))))
    (format t "Created variables~%")
    (format t "Moving ~a over given location: ~a~%" arm alternate-loc)
    (achieve `(arm-at ,arm ,alternate-loc))

    #|
    (with-retry-counters ((move-arm-over-loc-retry-counter 2))
      (with-failure-handling
          ((suturo-planning-common::move-arm-failed (f)
             (declare (ignore f))
             (info-out (suturo planlib) "Failed to move ~a over given location." arm)
             (sleep 2.0)
             (do-retry move-arm-over-loc-retry-counter
               (info-out (suturo planib) "Trying again.~%")
               (sleep 1.0)
               (retry))))
        (with-designators ((move-arm (action `((to move-arm)
                                               (loc ,alternate-loc)
                                               (arm ,arm)))))
          (format t "performing ~a~%" move-arm)
          (perform move-arm))))
    |#

    (format t "Lowering arm...~%")
    (with-retry-counters ((lower-arm-counter 5))
      (with-failure-handling
          ((suturo-planning-common::move-arm-failed (f)
             (declare (ignore f))
             (info-out (suturo planlib) "Failed to lower arm.~%.")
             (setf z (+ z 0.01))
             (sleep 2.0)
             (do-retry lower-arm-counter
               (info-out (suturo planib) "Trying again. This time a little bit higher. z: ~a~%" z)
               (sleep 1.0)
               (retry))))
        (with-designators ((move-arm (action `((to move-arm)
                                               (loc ,(make-designator 'location `((frame ,frame)
                                                                                  (coords (,x ,y ,z))
                                                                                  (pose ,pose))))
                                               (arm ,arm)))))
          (perform move-arm))))
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
