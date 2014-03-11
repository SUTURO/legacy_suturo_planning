(in-package :suturo-planning-pm-manipulation)

(defvar *current-goal-result-type* nil)
(defvar *current-goal-id* nil)
(defvar *current-goal-failed* nil)

(defun with-lost-in-resultation-workaround
    (client goal timeout throwable server cancel-topic-type result-topic-type status-topic-type
     &key (intents 2)
       (on-timeout-fn #'(lambda () t))
       (on-fail-fn #'(lambda () t))
       (on-success-fn #'(lambda () t)))
  "Calls actionlib:call-goal `client' `goal' :timeout `timeout' :result-timeout `timeout'.
When timeout is reached the function `on-timeout-fn' is called.
If `on-timeout' returns t, the process will be repeated `intents' times.
When call-goal fails or `max-retry-intents' is reached, `on-fail-fn' will be executed
and `trowable' will be thrown.
When call-goal succeeds, `on-success-fn' will be executed."
  (let ((time-begin nil)
        (time-now nil)
        (result-subscriber
          (roslisp:subscribe
           (concatenate 'string server "/result")
           result-topic-type #'(lambda (msg) (get-result msg))))
        (status-subscriber
          (roslisp:subscribe
           (concatenate 'string server "/status")
           status-topic-type #'(lambda (msg) (get-status msg)))))
    (loop while (> intents 0)
          do (format t "calling goal: ~a~%client: ~a~%timeout: ~a~%" goal client timeout)
             (format t "Waiting for server: ~a~%" (actionlib:wait-for-server client))
             (setf time-begin (roslisp:ros-time))
             (setf time-now (roslisp:ros-time))
             (setf *current-goal-id* (actionlib:goal-id (actionlib:send-goal client goal)))
             (format t "Goal created: ~a~%" *current-goal-id*)
             (loop while (and
                          (not (timeout-occured timeout time-begin time-now))
                          (not *current-goal-failed*)
                          (not *current-goal-result-type*))
                   do (sleep 0.1)
                      (setf time-now (roslisp:ros-time)))                      
             (roslisp:unsubscribe result-subscriber)
             (roslisp:unsubscribe status-subscriber)
             (cond
               (*current-goal-failed*
                (progn
                  (format t "Goal failed.~%")
                  (goal-failed on-fail-fn throwable)
                  (return)))
               (*current-goal-result-type*
                (progn
                  (handle-action-answer *current-goal-result-type* throwable)
                  (roslisp:ros-info (suturo-pm-manipulation) "Action finished successfully.")
                  (clear-variables)
                  (funcall on-success-fn)                 
                  (return)))
               ((timeout-occured timeout time-begin time-now)
                (progn
                  (decf intents)
                  (format t "Timeout reached.~%")
                  (format t "Intents left: ~a~%" intents)
                  (roslisp:publish (concatenate 'string server "/cancel")
                                   (roslisp:make-msg cancel-topic-type
                                                     (stamp) (roslisp:ros-time)
                                                     (id) *current-goal-id*))
                  (clear-variables)
                  (if (not (funcall on-timeout-fn))
                      (return))))))
    (if (< intents 1)
        (progn
          (format t "Reached maximum number of retries.~%")
          (goal-failed on-fail-fn throwable)))))

(defun clear-variables ()
  (setf *current-goal-result-type* nil)
  (setf *current-goal-id* nil)
  (setf *current-goal-failed* nil))

(defun goal-failed (on-fail-fn throwable)
  (clear-variables)
  (funcall on-fail-fn)
  (cpl:error throwable))

(defun timeout-occured (timeout start-time now)
  (>= (- now start-time) timeout))
    
(defun get-result (msg)
  (format t "Calling get-result~%")
  (roslisp:with-fields ((goal_id (goal_id status))  (succ (succ result))) msg
    (roslisp:with-fields (id) goal_id
      (roslisp:with-fields (type) succ
        (if (equal id *current-goal-id*)
        (setf *current-goal-result-type* type))))))
    
(defun get-status (msg)
  (roslisp:with-fields (status_list) msg
    (let ((list (roslisp-msg-protocol:ros-message-to-list status_list)))
      ;;(format t "Transformed in list.~%")
      (loop for s across list
            do (roslisp:with-fields ((id (id goal_id)) status) s
                 (if (equal id *current-goal-id*)
                     (progn
                       (format t "Goal: ~a~% status: ~a~%" id status)
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       ;;;;;;;; TODO Wenn Status auf 3 (oder ähnliches gesetzt, ;;;;;;;;
                       ;;;;;;;; dann loop abbrechen und goal nochmal ausführen. ;;;;;;;;
                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                       (cond
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PENDING)) )
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :ACTIVE)) )
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PREEMPTED))
                          (setf *current-goal-failed* t))
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :SUCCEEDED)) )
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :ABORTED))
                          (setf *current-goal-failed* t))
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :REJECTED))
                          (setf *current-goal-failed* t))
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PREEMPTING)) )
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :RECALLING)) )
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :RECALLED))
                          (setf *current-goal-failed* t))
                         ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :LOST))
                          (setf *current-goal-failed* t))))))))))

(defun handle-action-answer (type error-to-throw)
  "Analyses returns from action servers and throws the passed error if necessary."
  (cond
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :SUCCESS))
     (roslisp:ros-info(suturo-planning-pm-manipulation) "SUCCESS!"))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :FAIL))
     (roslisp:ros-info suturo-planning-pm-manipulation "FAIL!")
     (cpl:error error-to-throw))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :NOPLAN))
     (roslisp:ros-info (suturo-planning-pm-manipulation) "No plan found!")
     (cpl:error 'suturo-planning-common::no-plan-found))
    ((eql
      type
      (roslisp-msg-protocol:symbol-code 'suturo_manipulation_msgs-msg:ActionAnswer :UNDEFINED))
     (info-out suturo-planning-pm-manipulation "Unknown error. Blame manipulation!")
     (cpl:error error-to-throw))
    (t
     (roslisp:ros-info suturo-planning-pm-manipulation "Unhandled action answer.")
     (cpl:error 'suturo-planning-common::unhandled-action-answer))))