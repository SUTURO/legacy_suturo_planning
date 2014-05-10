(in-package :suturo-planning-planlib)

(def-goal (achieve (know-unknown-object ?obj))
  "The passed object `?obj' has to be positioned at different angles (90°, 135°, 225°, 270°, 315°, 405°) to the PR2's view axis. In every position the object has to be released from the holding gripper and the PR2 has to scan/take a picture of the object.

Initially the PR2 has to be positioned in front of the object. The object has to be placed on a table/surface with with enough space to manipulate the object's position/pose."
  (format t "Getting to know unknown object.~%")
  (let* ((obj (current-desig ?obj))
         (obj-name (desig-prop-value ?obj 'name))
         (obj-in-base-link-origin (transform-get-origin obj-name "/base_link" :timeout 2))
         (base-link-in-map (transform "/base_link" "/map" :timeout 2))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (obj-on (suturo-planning-pm-knowledge::call-action
                      'suturo-planning-pm-knowledge::get-static-object obj-on-name))
         (arm 'right-arm)
         (gripper-frame (get-gripper-frame arm))
         (rotations '(45.0 90.0 45.0 45.0 90.0)))
    (format t "obj-on-name: ~a~%" obj-on-name)
    (format t "obj-on: ~a~%" obj-on)
    (format t "Moving in front of unknown object.~%")
    (achieve `(robot-at ,(pose->pose-stamped
               (cl-transforms:transform
                base-link-in-map
                (cl-tf:make-pose-stamped "/base_link" 0.0
                                         (cl-tf:make-3d-vector 0 (cl-transforms:y obj-in-base-link-origin) 0)
                                         (cl-tf:make-identity-rotation)))
               "/map")))
    (achieve `(home-pose both-arms))
    (format t "Grasping unknown object.~%")
    (grasp obj arm :intents 1 :update-planning-scene nil)
    (let* ((gripper-origin (transform-get-origin gripper-frame "/base_link" :timeout 2))
           (initial-lifted-location
             (cond
               ((eq arm 'left-arm)
                (cl-tf:make-pose-stamped "/base_link" 0.0
                                         (cl-tf:v+ gripper-origin
                                                   (cl-tf:make-3d-vector 0 0 0.02))
                                         (cl-tf:make-quaternion 0.5000285573285219d0
                                                                0.4990192438614538d0
                                                                -0.5000920419769876d0
                                                                0.500858448728967d0)))
               ((eq arm 'right-arm)
                (cl-tf:make-pose-stamped "/base_link" 0.0
                                         (cl-tf:v+ gripper-origin
                                                   (cl-tf:make-3d-vector 0 0 0.02))
                                         (cl-tf:make-quaternion -0.4979847991023766d0
                                                                0.5021521617962537d0
                                                                0.49771687296865463d0
                                                                0.5021277333792858d0)))
              (t (cpl:error 'suturo-planning-common::unhandled-body-part)))))
      (format t "Taking initial scan pose.~%")
      (format t "Lifting and rotating arm.~%")
      (achieve `(arm-at ,arm ,initial-lifted-location))
      (format t "Lowering arm.~%")
      (let ((x-val 0.02))
        (with-retry-counters ((move-arm-counter 1))
          (with-failure-handling
              ((suturo-planning-common::move-arm-failed (e)
                 (declare (ignore e))
                 (format t "Failed to lower arm.~%")
                 (do-retry move-arm-counter
                   (format t "Trying again. This time a little bit higher.~%")
                   (setf x-val 0.01)
                   (retry))
                 (return)))
            (achieve `(arm-at ,arm
                              ,(pose->pose-stamped
                                (cl-transforms:transform (transform gripper-frame "/base_link" :timeout 2)
                                                         (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                                  (cl-tf:make-3d-vector x-val 0 0)
                                                                                  (cl-tf:make-identity-rotation)))
                                "/base_link"))))))
      (format t "Dropping object.~%")
      (achieve `(empty-hand ,obj ,(make-designator 'object `((name ,obj-on-name)))))
      (perform (make-designator 'action `((to learn-object)
                                          (action learn-object-start)
                                          (name ,obj-name))))
      (learn-object obj arm :intents 2)
      (loop for rotation in rotations
            do (format t "Grasping unknown object.~%")
               (grasp obj arm)
               (let* ((gripper-in-base-link (transform gripper-frame "/base_link" :timeout 2))
                      (lifted-location
                        (pose->pose-stamped
                         (cl-transforms:transform
                          gripper-in-base-link
                          (cl-tf:make-pose-stamped gripper-frame 0.0
                                                   (cl-tf:make-3d-vector -0.02 0 0)
                                                   (cl-transforms-euler-degree->quaternion :ax rotation)))
                         "/base_link")))
                 (format t "Lifiting arm and rotating object in ~a ~a degrees.~%" arm rotation)
                 (achieve `(arm-at ,arm ,lifted-location))
                 (format t "Lowering arm.~%")
                 (let* ((x-val 0.02))
                   (with-retry-counters ((move-arm-counter 1))
                     (with-failure-handling
                         ((suturo-planning-common::move-arm-failed (e)
                            (declare (ignore e))
                            (format t "Failed to lower arm.~%")
                            (do-retry move-arm-counter
                              (format t "Trying again. This time a little bit higher.~%")
                              (setf x-val 0.01)
                              (retry))
                            (return)))
                       (achieve `(arm-at ,arm
                                         ,(pose->pose-stamped
                                           (cl-transforms:transform
                                            gripper-in-base-link
                                            (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                     (cl-tf:make-3d-vector x-val 0 0)
                                                                     (cl-transforms-euler-degree->quaternion :ax rotation)))
                                           "/base_link"))))))
                 (format t "Dropping object.~%")
                 (achieve `(empty-hand ,obj ,(make-designator 'object `((name ,obj-on-name)))))
                 (learn-object obj arm :intents 2)))
      (perform (make-designator 'action `((to learn-object)
                                          (action learn-object-finish)))))))

(defun update-planning-scene (obj-desig)
  (let* ((obj (current-desig obj-desig))
         (obj-name (desig-prop-value obj 'name))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (success nil))
    (loop
      (perform (make-designator 'action
                                `((to update-objects-on)
                                  (name ,obj-on-name))))
      (let ((objects (sp-knowledge::call-action
                      'sp-knowledge::get-graspable-objects
                      sp-planlib::*table-name*)))
        (loop for object in objects do
          (if (equal obj-name (desig-prop-value object 'name))
              (let* ((object-in-base-link-x (transform-get-origin
                                             (desig-prop-value object 'name)
                                             "/base_link" :timeout 2)))
                (if (< (abs object-in-base-link-x) 10)
                    (setf success t)))))))))
  

(defun learn-object (obj-desig arm &key intents)
  (let* ((obj (current-desig obj-desig))
         (obj-name (desig-prop-value obj 'name))
         (gripper-frame (get-gripper-frame arm))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (result nil))
  (loop while (or (not (eq result sp-knowledge::*learn-object-success*))
                  (not intents)
                  (> intents 0))
        do (format t "Learning to know object. Intents left: ~a~%" intents)
           (format t "Moving arm to home pose.~%")
           (achieve `(home-pose ,arm))
           (format t "Moving head to home pose.~%")
           (achieve `(home-pose head))
           (format t "Updating planning scene.~%")
           (perform (make-designator 'action 
                                     `((to update-objects-on)
                                       (name ,obj-on-name))))
           (format t "~%~%### Knowledge is doing its magic now... ###~%~%")
           (setf result (perform (make-designator 'action `((to learn-object)
                                                            (action learn-object-learn)
                                                            (name ,obj-name)))))
           (progn
             (cond
               ((eq result sp-knowledge::*learn-object-success*) t)
               ((eq result sp-knowledge::*learn-object-to-close-to-other-object*)
                (format t "Object too close to other object. Moving object.~%")
                (grasp obj arm)
                (format t "Lifting arm.~%")
                (achieve `(arm-at ,arm
                                  ,(pose->pose-stamped
                                    (cl-transforms:transform (transform gripper-frame "/base_link" :timeout 2)
                                                             (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                                      (cl-tf:make-3d-vector -0.02 0 0)
                                                                                      (cl-tf:make-identity-rotation)))
                                    "/base_link")))
                (format t "Moving arm.~%")
                (let ((obj-in-base-link-origin (transform-get-origin obj-name "/base_link" :timeout 2)))
                  (achieve
                   `(arm-at ,arm
                            ,(cl-tf:make-pose-stamped
                              "/base_link" 0.0
                              (cl-tf:v+
                               obj-in-base-link-origin
                               (cl-tf:make-3d-vector (get-non-blocked-base-link-x-offset obj) 0 0))
                              (cl-tf:make-identity-rotation)))))
                (format t "Lowering arm.~%")
                (let ((x-val 0.02))
                  (with-retry-counters ((move-arm-counter 1))
                    (with-failure-handling
                        ((suturo-planning-common::move-arm-failed (e)
                           (declare (ignore e))
                           (format t "Failed to lower arm.~%")
                           (do-retry move-arm-counter
                             (format t "Trying again. This time a little bit higher.~%")
                             (setf x-val 0.01)
                             (retry))
                           (return)))
                      (achieve `(arm-at ,arm
                                        ,(pose->pose-stamped
                                          (cl-transforms:transform (transform gripper-frame "/base_link" :timeout 2)
                                                                   (cl-tf:make-pose-stamped gripper-frame 0.0
                                                                                            (cl-tf:make-3d-vector x-val 0 0)
                                                                                            (cl-tf:make-identity-rotation)))
                                          "/base_link"))))))
                (format t "Dropping object.~%")
                (achieve `(empty-hand ,obj ,(make-designator 'object `((name ,obj-on-name))))))
               ((eq result sp-knowledge::*learn-object-object-lost*) (format t "Lost object.~%"))
               ((eq result sp-knowledge::*learn-object-failed*) (format t "Learn object failed.~%"))
               (t (cpl:error 'unhandled-value)))
             (if intents
                 (decf intents))))
    (cond
      ((eq result sp-knowledge::*learn-object-success*) t)
      ((eq result sp-knowledge::*learn-object-to-close-to-other-object*) (cpl:error 'object-to-close-to-other-object))
      ((eq result sp-knowledge::*learn-object-object-lost*) (cpl:error 'object-suddenly-lost))
      ((eq result sp-knowledge::*learn-object-failed*) (cpl:error 'learn-object-failed))
      (t (cpl:error 'unhandled-value)))))

(defun get-non-blocked-base-link-x-offset (obj-desig)
  (let* ((obj (current-desig obj-desig))
         (obj-frame (desig-prop-value obj 'name))
         (obj-max-dimension (max (desig-prop-value obj 'dimensions)))
         (obj-on-name (desig-prop-value-concat obj '(at on name)))
         (obj-on (suturo-planning-pm-knowledge::call-action
                  'suturo-planning-pm-knowledge::get-static-object obj-on-name))
         (obj-on-depth (second (desig-prop-value obj-on 'dimensions)))
         (obj-in-obj-on-origin (transform-get-origin obj-frame obj-on-name :timeout 2))
         (space-in-front-of-obj (+ (/ obj-on-depth 2) (cl-transforms:x obj-in-obj-on-origin))))
    (if (> space-in-front-of-obj (/ obj-max-dimension 2))
        (/ obj-max-dimension 2)
        (* (/ obj-max-dimension 2) -1))))

(defun grasp (obj-desig arm &key (intents 3) (update-planning-scene t))
  (let* ((obj (current-desig obj-desig))
         (obj-on-name (desig-prop-value-concat obj '(at on name))))
    (with-retry-counters ((grasp-obj-counter intents))
      (with-failure-handling
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (format t "Failed to grasp object.~%")
             (do-retry grasp-obj-counter
               (format t "Trying again.~%")
               (if update-planning-scene
                   (progn
                     (format t "Moving arm to home pose.~%")
                     (achieve `(home-pose ,arm))
                     (format t "Moving head to home pose.~%")
                     (achieve `(home-pose head))
                     (format t "Updating planning scene.~%")
                     (perform (make-designator 'action 
                                               `((to update-objects-on)
                                                 (name ,obj-on-name))))))
               (retry))))
        (achieve `(object-in-hand ,obj ,arm sp-manipulation::grasp-action-above 10))))))