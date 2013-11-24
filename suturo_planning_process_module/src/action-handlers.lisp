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
    (let ((query (concatenate 'string "is_edible(" (parse-perceived-objects-to-string objs) ", Answer)"))
          (service "/json_prolog/simple_query")
          (ret-service "/json_prolog/next_solution")
          (fin-service "/json_prolog/finish"))
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
                      (format t "~a~%" status)
                      (format t "~a~%" solution)
                      (format t "~a~%" (parse-object-ids-from-string solution))
                      (if (not (roslisp:wait-for-service fin-service 10))
                        (roslisp:ros-warn nil "Timed out waiting for finish")
                        (progn
                          (format t "foo")
                          (roslisp:call-service
                              fin-service
                              "json_prolog/PrologFinish"
                              :id "")
                          (if (= status 3)
                            (let ((r (filter-objects-by-ids objs (parse-object-ids-from-string solution))))
                              (format t "~a~%" r)
                              r)
                            #())))))))))))))

(def-action-handler move (pose)
  "Foobar"
  (roslisp:ros-info (suturo-pm move)
                    "Reached pose ~a."
                    pose))

(def-action-handler touch (arm obj)
  "Moves the arm to the object"
  (roslisp:with-ros-node ("manipulation_client_planning")
                         (call-touch-action arm obj)))

;; Define helper functions for manipulation action

(defvar *touch-action-client* nil)

(defun init-action-client ()
  (setf *touch-action-client* (actionlib:make-action-client
                                "move_action_server"
                                "suturo_manipulation_msgs/suturo_manipulation_moveAction"))
  (roslisp:ros-info (suturo-pm touch-action-client)
                    "Waiting for action server...")
  (loop until
        (actionlib:wait-for-server *touch-action-client*))
  (roslisp:ros-info (suturo-pm touch-action-client)
                    "Action client created..."))

(defun get-action-client ()
  (when (null *touch-action-client*)
    (init-action-client))
    *touch-action-client*)

(defun make-touch-action-goal (in-arm in-target)
  (actionlib:make-action-goal (get-action-client)
                              arm in-arm
                              obj in-obj))

(defun call-touch-action (&key arm obj)
  (multiple-value-bind (result status)
    (let ((actionlib:*action-server-timeout* 10.0))
      (actionlib:call-goal
        (get-action-client)
        (make-touch-action-goal arm obj)))
    (roslisp:ros-info (suturo-pm touch-action-client)
                      "Action finished. Object hopefully touched.")
    (values result status)))

;same for initial pose 

(defvar *initial-action-client* nil)

(defun init-initial-action-client ()
  (setf *initial-action-client* (actionlib:make-action-client
                                "home_action_server"
                                "suturo_manipulation_msgs/suturo_manipulation_homeAction"))
  (roslisp:ros-info (suturo-pm initial-action-client)
                    "Waiting for action server...")
  (loop until
        (actionlib:wait-for-server *initial-action-client*))
  (roslisp:ros-info (suturo-pm initial-action-client)
                    "Action client created..."))

(defun get-initial-action-client ()
  (when (null *initial-action-client*)
    (init-initial-action-client))
    *initial-action-client*)

(defun make-initial-action-goal (in-arm)
  (actionlib:make-action-goal (get-initial-action-client)
                              arm in-arm))

(defun call-initial-action (&key arm)
  (multiple-value-bind (result status)
    (let ((actionlib:*action-server-timeout* 10.0))
      (actionlib:call-goal
        (get-initial-action-client)
        (make-initial-action-goal arm)))
    (roslisp:ros-info (suturo-pm initial-action-client)
                      "Action finished. Object hopefully touched.")
    (values result status)))
