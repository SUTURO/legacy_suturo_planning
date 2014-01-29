(in-package :suturo-planning-pm-utils)

(defvar *transform-listener* nil)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler get-best-arm (obj)
  (cpl-impl:with-retry-counters ((lookup-retry-counter 8))
    (cpl-impl:with-failure-handling
        ((cl-tf:tf-lookup-error (f)
           (declare (ignore f))
           (format t "cl-tf: tf-lookup-error~%")
           (sleep 1)
           (cpl-impl:do-retry lookup-retry-counter
             (cpl-impl:retry))))      
      (let* ((coords (get-coords obj))
             (frame (get-frame obj))
             (pose-stamp-old (cl-tf:make-pose-stamped
                              frame 0.0
                              (cl-transforms:make-3d-vector (nth 0 coords)
                                                            (nth 1 coords)
                                                            (nth 2 coords))
                              (cl-transforms:make-quaternion 0 0 0 1)))
             (pose-stamp-base-link nil))
        (if (not *transform-listener*)
            (setf *transform-listener* 
                  (make-instance 'cl-tf:transform-listener)))
        (setf pose-stamp-base-link 
              (cl-tf:transform-pose *transform-listener* 
                                    :pose pose-stamp-old
                                    :target-frame "/base_link"))
        (if (> (cl-transforms:y (cl-transforms:origin pose-stamp-base-link)) 0)
            'left-arm
            'right-arm)))))

(def-action-handler get-location-over (loc)
  (cpl-impl:with-retry-counters ((lookup-retry-counter 8))
    (cpl-impl:with-failure-handling
        ((cl-tf:tf-lookup-error (f)
           (declare (ignore f))
           (format t "cl-tf: tf-lookup-error~%")
           (sleep 1)
           (cpl-impl:do-retry lookup-retry-counter
             (cpl-impl:retry))))      
      (let* ((coords (desig-prop-value loc 'coords))
             (frame (desig-prop-value loc 'frame))
             (pose-stamp-old (cl-tf:make-pose-stamped
                              frame 0.0
                              (cl-transforms:make-3d-vector (nth 0 coords)
                                                            (nth 1 coords)
                                                            (nth 2 coords))
                              (cl-transforms:make-quaternion 0 0 0 1)))
             (pose-stamp-base-link nil))
        (if (not *transform-listener*)
            (setf *transform-listener* 
                  (make-instance 'cl-tf:transform-listener)))
        (setf pose-stamp-base-link 
              (cl-tf:transform-pose *transform-listener* 
                                    :pose pose-stamp-old
                                    :target-frame "/base_link"))
        (let ((x-coord (- (cl-transforms:x 
                           (cl-transforms:origin pose-stamp-base-link))
                           0.20))
              (y-coord (cl-transforms:y 
                         (cl-transforms:origin pose-stamp-base-link)))
              (z-coord (+ (cl-transforms:y 
                           (cl-transforms:origin pose-stamp-base-link))
                          0.40)))
          (make-designator 'location 
                           (update-designator-properties 
                            `((coords (,x-coord ,y-coord ,z-coord))
                              (frame "/base_link"))
                            (description loc))))))))

(defun get-frame (obj)
  "Returns the coordinates of the object"
  (if obj
      (desig-prop-value (desig-prop-value (current-desig obj) 'at) 'frame)
      nil))

(defun get-coords (obj)
  "Returns the coordinates of the object"
  (if obj
      (desig-prop-value (desig-prop-value (current-desig obj) 'at) 'coords)
      nil))