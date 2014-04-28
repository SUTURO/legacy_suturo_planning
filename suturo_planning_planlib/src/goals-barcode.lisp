(in-package :suturo-planning-planlib)

(def-goal (achieve (scanned-from ?obj))
  "Moves an object `?obj' in fornt of the webcam and rotates it until a barcode was found and scanned. The `?obj' has to be held by a gripper."
  (format t "Try to scan Barcode from ~a~%" ?obj)

  ;reach campose with object in left-arm

  ;(let* ((arm (get-holding-arm ?obj)))
  ;  (if (eq arm 'right-arm)
  ;      (achive `(object-passed-over ?obj)))
  (let* ((obj-name (desig-prop-value ?obj 'name))
         (obj-in-base-link-origin (transform-get-origin obj-name "/base_link" :timeout 2)))
    (achieve `(robot-at ,(cl-tf:make-pose-stamped "/base_link" 0.0 (cl-tf:make-3d-vector 0 (cl-transforms:y obj-in-base-link-origin) 0)
                                                  (cl-tf:make-identity-rotation))))
    (achieve `(object-in-hand ?obj left-arm sp-manipulation::grasp-action-grasp nil))
    (achieve `(home-pose left-arm-campose)))
 
  ;ask perception||knwoledge if barcode was found
  ;rotate obj by 45°
                                     
    (achieve `(gripper-roteted left-arm 45.0)))

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
                                     
    (achieve `(arm-at left-arm ,rotated-location))))



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