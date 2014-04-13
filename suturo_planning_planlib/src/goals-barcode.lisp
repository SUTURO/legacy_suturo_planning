(in-package :suturo-planning-planlib)

(def-goal (achieve (scanned-from ?obj))
  "Moves an object `?obj' in fornt of the webcam and rotates it until a barcode was found and scanned. The `?obj' has to be held by a gripper."
  (format t "Try to scan Barcode from ~a~%" ?obj)

  ;reach campose with object in left-arm

  (let* ((arm (get-holding-arm ?obj)))
    (if (eq arm 'right-arm)
     ;switch arm  )
    (achieve `(home-pose 'left-arm-campose)))
 
  ;ask perception||knwoledge if barcode was found
  ;rotate obj by 45°

  (let* ((pose-stamped (suturo-planning-common:get-last-gripper-pose ?obj)))
    )

)