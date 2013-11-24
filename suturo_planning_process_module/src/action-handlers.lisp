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

(def-action-handler ground (objs)
  "Looks up matching objects from the knowledge base"
  (roslisp:with-ros-node ("knowledge_client_planning")
    (let ((query (concatenate 'string "is_edible(" (parse-perceived-objects-to-string objs) ", answer)"))
          (service "/json_prolog/simple_query")
          (ret-service "/json_prolog/next_solution"))
      (format t "~a~%" query)
      (if (not (roslisp:wait-for-service service 10))
          (roslisp:ros-warn nil "Timed out waiting for simple_query")
          (let ((results (roslisp:call-service
                         service
                         "json_prolog/PrologQuery"
                         :mode 0
                         :id ""
                         :query query)))
            (roslisp:with-fields (ok message) results
              (format t "~a~%" message)
              (format t "~a~%" ok)
              (if ok
                (if (not (roslisp:wait-for-service ret-service 10))
                  (roslisp:ros-warn nil "Timed out waiting for next_solution")
                  (let ((res (roslisp:call-service
                             ret-service
                             "json_prolog/PrologNextSolution"
                             :id "")))
                    (roslisp:with-fields (status solution) res
                      (format t "~a~%" solution)
                      (if (= status 3)
                        (filter-objects-by-ids objs (parse-object-ids-from-string solution))
                        #()))))
                #())))))))

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
