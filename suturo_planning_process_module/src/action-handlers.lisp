(in-package :suturo-planning-process-module)

(defmacro def-action-handler (name args &body body)
  (alexandria:with-gensyms (action-sym params)
    `(defmethod call-action ((,action-sym (eql ',name)) &rest ,params)
       (destructuring-bind ,args ,params ,@body))))

(def-action-handler perceive ()
  "Returns a list of objects perceived on the table"
  (roslisp:with-ros-node ("perception_client_planning")
    (let ((service "/GetClusters"))
      (if (not (roslisp:wait-for-service service 10))
          (roslisp:ros-warn nil "Timed out waiting for service GetClusters")
          (let ((results (roslisp:call-service
                          service
                          "suturo_perception_msgs/GetClusters"
                          :s "get")))
            (roslisp:with-fields (perceivedobjs) results
;              (loop for i from 0 below (length perceivedobjs)
;                    for obj = (elt perceivedobjs i)
;                    do 
;                       (roslisp:with-fields (c_id c_centroid c_volume) obj
;                         (format t "~a~%" c_centroid)
;                         (format t "~a~%" c_volume)))
              perceivedobjs))))))

(def-action-handler ground (obj-desig)
  "Looks up matching objects from the knowledge base"
  (roslisp:ros-info (suturo-pm ground)
                    "Grounding object.")
  (format t "OBJ-DESIG: ~a" obj-desig))

(def-action-handler move (pose)
  "Foobar"
  (roslisp:ros-info (suturo-pm move)
                    "Reached pose ~a."
                    pose))

(def-action-handler touch (arm obj)
  "Moves the arm to the object"
  (roslisp:with-ros-node ("manipulation_client_planning")
    (let ((service "/suturo_manipulation_move_action_server"))
      (if (not (roslisp:wait-for-service service 10))
          (roslisp:ros-warn nil "Timed out waiting for service suturo_manipulation_move_action_server")
          (let ((results (roslisp:call-service
                          service
                          "suturo_manipulation_msgs/suturo_manipulation_move_action_server"
                          :p obj
                          :arm (if (eql arm 'left) 
                                   "left_arm"
                                   "right_arm"))))
            (roslisp:with-fields (answer) results
              (format t "RESULT ~a" answer))))))
  (roslisp:ros-info (suturo-pm touch)
                    "Touched object"))
