(in-package :suturo-planning-common)

(defparameter *visualization-advertiser* nil) 

(defun capped (value min max)
  (if (and min (< value min))
      min
      (if (and max (> value max))
          max
          value)))

(defun publish-visualization-marker (pose-stamped &key 
                                                    (type :arrow) 
                                                    (frame "/map")
                                                    min-x max-x
                                                    min-y max-y
                                                    min-z max-z
                                                    (scale-x 0.3)
                                                    (scale-y 0.15)
                                                    (scale-z 0.15)
                                                    (color-a 1)
                                                    (color-r 1)
                                                    (color-g 1)
                                                    (color-b 1)
                                                    (lifetime 50))
  (if (not *visualization-advertiser*)
      (setf *visualization-advertiser* 
            (advertise "/suturo/planning_marker" 
                       "visualization_msgs/Marker")))
  (let ((origin (cl-tf:origin pose-stamped))
        (orientation (cl-tf:orientation pose-stamped)))
    (publish *visualization-advertiser*
             (make-message "visualization_msgs/Marker"
                           (frame_id header) frame
                           (stamp header)  (ros-time)
                           ns "my_ns"
                           id 0
                           type (symbol-code 'visualization_msgs-msg:marker 
                                             type)
                           action 0
                           (x position pose) (capped (cl-tf:x origin)
                                                     min-x max-x)
                           (y position pose) (capped (cl-tf:y origin)
                                                     min-y max-y)
                           (z position pose) (capped (cl-tf:z origin)
                                                     min-z max-z)
                           (x orientation pose) (cl-tf:x orientation)
                           (y orientation pose) (cl-tf:y orientation)
                           (z orientation pose) (cl-tf:z orientation)
                           (w orientation pose) (cl-tf:w orientation)
                           (x scale) scale-x
                           (y scale) scale-y
                           (z scale) scale-z
                           (a color) color-a
                           (r color) color-r
                           (g color) color-g
                           (b color) color-b
                           lifetime lifetime))))

(defun publish-visualization-marker2 (loc)
  (let ((origin (desig-prop-value loc 'coords))
        (orientation (desig-prop-value loc 'pose)))
    (publish (advertise "/visualization_marker" "visualization_msgs/Marker")
             (make-message "visualization_msgs/Marker"
                           (frame_id header) "/map"
                           (stamp header)  (ros-time)
                           ns "my_ns"
                           id 0
                           type 0
                           action 0
                           (x position pose) (first origin)
                           (y position pose) (second origin)
                           (z position pose) (third origin)
                           (x orientation pose) (first orientation)
                           (y orientation pose) (second orientation)
                           (z orientation pose) (third orientation)
                           (w orientation pose) (fourth orientation)
                           (x scale) 0.2
                           (y scale) 0.15
                           (z scale) 0.15
                           (a color) 1
                           (r color) 0
                           (g color) 1
                           (b color) 1
                           lifetime 50))))
