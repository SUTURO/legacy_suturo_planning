(in-package :suturo-planning-pm-manipulation)

(defvar *lock* (sb-thread:make-mutex))

(defvar *current-goal-result-type* nil)
(defvar *current-goal-id* nil)
(defvar *current-goal-failed* nil)
(defvar *current-goal-subscribers* nil)
(defvar *current-goal-result-callback-failed* nil)
(defvar *versuch* 0)

(defun with-lost-in-resultation-workaround
    (client goal timeout throwable server cancel-topic-type result-topic-type status-topic-type
     &key (intents 2)
       (on-timeout-fn #'(lambda () t))
       (on-fail-fn #'(lambda () t))
       (on-success-fn #'(lambda () t)))
  "Calls (actionlib:send-goal `client' `goal'.
`server' is the action server. It is necessary to subscribe to the
/cancel-topic (`cancel-topic-type' [the type of the /cancel-topic]
has to be defined), the /result-topic (`result-topic-type'
[the type of the /result-topic] has to be defined) and the
/status-topic (`status-topic-type' [the type of the /status-topic]
has to be defined).
When `timeout' is reached, the function `on-timeout-fn' is called.
If `on-timeout-fn' returns t, the action is assumed to have succeeded
and the workaround exits without raising any condition.
If `on-timeout-fn' returns nil, the goal will be sent out again.
When call-goal fails or `max-retry-intents' is reached, `on-fail-fn' will
be executed and `trowable' will be thrown.
When call-goal succeeds, `on-success-fn' will be executed and the
workaround exits without raising any condition"
  (format t "Starting workaround.~%")
  (unsubscribe)
  (clear-variables)
  (let* ((time-begin nil)
         (time-now nil))
    (sb-thread:with-recursive-lock (*lock*)
      (setf *current-goal-subscribers*
            (list
             (roslisp:subscribe
              (concatenate 'string server "/result")
              result-topic-type #'(lambda (msg) (get-result msg)))
             (roslisp:subscribe
              (concatenate 'string server "/status")
              status-topic-type #'(lambda (msg) (get-status msg))))))
    (sleep 0.5)
    (incf intents)
    (loop while (> intents 0)
          do (clear-variables)
             ;;(format t "Calling goal: ~a~%Client: ~a~%timeout: ~a~%" goal client timeout)
             (format t "intents left (including this one): ~a~%" intents)
             (format t "Waiting for server: ~a~%" (actionlib:wait-for-server client))
             (setf time-begin (roslisp:ros-time))
             (setf time-now (roslisp:ros-time))
             (format t "Sending goal.~%")
             (sb-thread:with-recursive-lock (*lock*)
               (setf *current-goal-id* (actionlib:goal-id (actionlib:send-goal client goal)))
               (format t "Goal sent: ~a~%" *current-goal-id*))
             (loop while (sb-thread:with-recursive-lock (*lock*)
                           (and
                            (not (timeout-occured timeout time-begin time-now))
                            (not *current-goal-failed*)
                            (not *current-goal-result-type*)
                            (not *current-goal-result-callback-failed*)))
                   do (format t "looping...~%")
                      (format t "~tcurrent-goal-result-callback-failed: ~a~%"
                          *current-goal-result-callback-failed*)
                      (sleep 1)
                      (setf time-now (roslisp:ros-time)))
             (cond
               ((sb-thread:with-recursive-lock (*lock*)
                  *current-goal-failed*)
                (progn
                  (format t "Goal failed.~%")
                  (goal-failed on-fail-fn throwable)
                  (return)))
               ((sb-thread:with-recursive-lock (*lock*)
                  *current-goal-result-type*)
                (progn
                  (handle-action-answer *current-goal-result-type* throwable)
                  (roslisp:ros-info (suturo-pm-manipulation) "Action finished successfully.")
                  (funcall on-success-fn)
                  (return)))
               ((sb-thread:with-recursive-lock (*lock*)
                  *current-goal-result-callback-failed*)
                (progn
                  (format t "Goal finished, but callback hasn't been called.~%")
                  (format t "~tcurrent-goal-result-callback-failed: ~a~%"
                          *current-goal-result-callback-failed*)
                  (format t "Cancelling goal and calling again.~%")
                  (cancel-goal server cancel-topic-type)
                  (decf intents)))
               ((timeout-occured timeout time-begin time-now)
                (progn
                  (decf intents)
                  (format t "Timeout reached.~%")
                  (format t "Intents left: ~a~%" intents)
                  (cancel-goal server cancel-topic-type)
                  (if (not (funcall on-timeout-fn))
                      (return))))))
    (format t "Cleaning up after workaround.~%")
    (cancel-goal server cancel-topic-type)
    (unsubscribe)
    (clear-variables)
    (if (< intents 1)
        (progn
          (format t "Reached maximum number of retries.~%")
          (goal-failed on-fail-fn throwable))))
  (format t "Leaving workaround.~%"))

(defun cancel-goal (server cancel-topic-type)
  "Cancels the currently running goal."
  (format t "Canceling goal.~%")
  (sb-thread:with-recursive-lock (*lock*)
    (roslisp:publish (concatenate 'string server "/cancel")
                     (roslisp:make-msg cancel-topic-type
                                       (stamp) (roslisp:ros-time)
                                       (id) *current-goal-id*))))

(defun unsubscribe ()
  "Unsubscribes  all subscribed topics in `*current-goal-subscribers*'"
  (format t "Unsubscribing.~%")
  (sb-thread:with-recursive-lock (*lock*)
    (loop for subscriber in *current-goal-subscribers*
          do (roslisp:unsubscribe subscriber))
    (setf *current-goal-subscribers* nil)))

(defun clear-variables ()
  "Clears all variables which are used to monitor the goal's progress."
  (format t "Clearing variables.~%")
  (sb-thread:with-recursive-lock (*lock*)
    (setf *current-goal-id* nil)
    (setf *current-goal-result-type* nil)
    (setf *current-goal-failed* nil)
    (setf *current-goal-result-callback-failed* nil)))

(defun goal-failed (on-fail-fn throwable)
  "Executes the actions to be done when a goal fails."
  (unsubscribe)
  (clear-variables)
  (funcall on-fail-fn)
  (cpl:error throwable))

(defun timeout-occured (timeout start-time now)
  "Checks if a timeout occured."
  (>= (- now start-time) timeout))
    
(defun get-result (msg)
  "Callback method for the /result-topic."
  (sb-thread:with-recursive-lock (*lock*)
    (format t "Calling get-result.~%")
    (unsubscribe)
    (roslisp:with-fields ((goal_id (goal_id status))  (succ (succ result))) msg
      (roslisp:with-fields (id) goal_id
        (roslisp:with-fields (type) succ
          (if (equal id *current-goal-id*)
              (progn
                (format t "id: ~a~% current-id: ~a~%" id *current-goal-id*)
                (format t "~tResult ~a~%" type)
                (setf *current-goal-result-type* type))))))))
    
(defun get-status (msg)
  "Callback method for the /status-topic."
  (format t "callback status.~%")
  (roslisp:with-fields (status_list) msg
    (let ((list (roslisp-msg-protocol:ros-message-to-list status_list)))
      (loop for s across list
            do (roslisp:with-fields ((id (id goal_id)) status) s
                 (sb-thread:with-recursive-lock (*lock*)
                   (format t "id: ~a~% current-id: ~a~%" id *current-goal-id*)
                   (unless (equal id *current-goal-id*)
                     (return)))
                 (format t "Goal: ~a~% status: ~a~%" id status)
                 (cond
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PENDING)) )
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :ACTIVE)) )
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PREEMPTED))
                    (format t "Current goal failed.~%")
                    (setf *current-goal-failed* t))
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :SUCCEEDED))
                    (sleep 1)
                    (format t "Current goal succeeded. Waiting for result.~%")
                    (format t "~tcurrent-goal-result-type: ~a~%" *current-goal-result-type*)
                    (sb-thread:with-recursive-lock (*lock*)
                      (if (and (equal id *current-goal-id*)
                               (not *current-goal-result-type*))
                        (setf *current-goal-result-callback-failed* t))))
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :ABORTED))
                    (format t "Current goal failed.~%")
                    (setf *current-goal-failed* t))
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :REJECTED))
                    (format t "Current goal failed.~%")
                    (setf *current-goal-failed* t))
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :PREEMPTING)) )
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :RECALLING)) )
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :RECALLED))
                    (format t "Current goal failed.~%")
                    (setf *current-goal-failed* t))
                   ((eq status (roslisp-msg-protocol:symbol-code 'actionlib_msgs-msg:GoalStatus :LOST))
                    (format t "Current goal failed.~%")
                    (setf *current-goal-failed* t))))))))

(defun handle-action-answer (type error-to-throw)
  "Analyses returns from action servers and throws the passed error if necessary."
  (format t "Handling result.~%")
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