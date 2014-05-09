(in-package :suturo-planning-planlib)

(def-goal (achieve (scanned-from ?obj))
  "Moves an object `?obj' in fornt of the webcam and rotates it until a barcode was found and scanned. The `?obj' has to be held by a gripper."
  (format t "Try to scan Barcode from ~a~%" ?obj)
  (let* ((loc-to-reach (make-designator 'location 
                                       `((to reach) (obj ,?obj)))))
    (format t "Moving Robot")
    (achieve `(robot-at ,loc-to-reach))
    (format t "Taking home-pose")
    (achieve `(home-pose))
    (format t "Grasping Objekt")
    (with-retry-counters ((cnt 3))
      (with-failure-handling
          ((suturo-planning-common::grasping-failed (e)
             (declare (ignore e))
             (do-retry cnt
               (retry))))
        (achieve `(object-in-hand ,?obj left-arm sp-manipulation::grasp-action-above 360))))
    (format t "Moving Objekt in front of camera")
    (achieve `(home-pose left-arm-campose))
    (format t "Scanning barcode.")
    (achieve `(scan-barcode ,?obj))))
 
(defparameter *deg* 90) ;degrees rotated each step

(def-goal (achieve (scan-barcode ?obj))
  (with-retry-counters ((scan-object-counter (- (/ 360 *deg*) 1)))
    (with-failure-handling
      ((suturo-planning-common::barcode-scan-failed (e)
        (declare (ignore e))
        (do-retry scan-object-counter
          (achieve `(gripper-rotated left-arm *deg*))
          (retry))))
      (let ((scanned-barcode-object (subseq (first (first (perform (make-designator 'action `((to scan-barcode) (obj ,?obj)))))) 1)))
        (case (first scanned-barcode-object)
          ("nobarcode" (fail 'suturo-planning-common::barcode-scan-failed))
          ("nothingfound" (error-out (plan barcode scan) "Barcode scan failed!"))
          (otherwise (list->designator scanned-barcode-object)))))))

(def-goal (achieve (gripper-rotated ?arm ?degree))
  (let* ((gripper-frame (get-gripper-frame ?arm))
         (gripper-in-base-link (transform gripper-frame "/base_link" :timeout 2))
         (rotated-location 
           (pose->pose-stamped
            (cl-transforms:transform 
             gripper-in-base-link
             (cl-tf:make-pose-stamped gripper-frame 0.0
                                      (cl-tf:make-3d-vector 0 0 0)
                                      (cl-transforms-euler-degree->quaternion :ax ?degree)))
            "/base_link")))
    (with-retry-counters ((arm-at-counter 3))
      (with-failure-handling 
          ((suturo-planning-common::move-arm-failed (e)
             (declare (ignore e))
             (format t "Failed to move arm.~%")
             (do-retry arm-at-counter
               (format t "Retry rotatiing.~%")
               (retry))))
        (achieve `(arm-at left-arm ,rotated-location))))))



#|
(def-goal (achive (object-passed-over ?obj))
  ;holding-arm in right position
    (let* ((holding-arm (get-holding-arm ?obj))(other-arm 'right-arm))
      (if
        (eq holding-arm 'right-arm) (setf other-arm 'left-arm))
      (achive arm-at ... )
  ;grasp with other arm
    (achive `(object-in-hand ?obj other-arm sp-manipulation::grasp-action-grasp nil))
  (achive `(empty-hand ?obj nil)))) ;geht das? taget-on? 
|#

(def-goal (achieve (obj-in-front-of-webcam ?obj))
  (let ((retry-cnt 6)
        (distance-to-cam 0.23)
        (y (first (transform-coords-to-frame (format nil "/~a" (desig-prop-value ?obj 'name))
                                             "/l_wrist_roll_link"
                                             '(0 0 0))))
        (z 0))
    (with-failure-handling
        ((suturo-planning-common::move-arm-failed (e)
           (declare (ignore e))
           (format t "Failed to move arm.~%")
           (unless (eql retry-cnt 0)
             (case retry-cnt
               (0 (setf z (- z 4)))
               (1 (setf z (+ z 2)))
               (2 (setf y (- y 4)))
               (3 (setf y (+ y 2)))
               (4 (setf distance-to-cam (- distance-to-cam 4)))
               (5 (setf distance-to-cam (+ distance-to-cam 2)))
               (6 (format t "First try.~%")))
             (retry))))
      (format t "Trying with x:~a y:~a z:~a.~%" distance-to-cam y z)
      (let* ((wrist-coords-in-front-of-webcam (list distance-to-cam y z))
             (loc-in-front-of-webcam (make-designator 'location `((coords ,wrist-coords-in-front-of-webcam)
                                                                  (pose ,(cl-transforms-euler-degree->quaternion-as-list :az -90))
                                                                  (frame "/webcam")))))
        (setf retry-cnt (- retry-cnt 1))
        (achieve `(arm-at left-arm ,loc-in-front-of-webcam))))))

    
