(in-package :suturo-planning-pm-manipulation)

(defun with-lost-in-resultation-workaround
    (client goal timeout throwable
     &key (max-retry-intents 3) (on-timeout-fn #'(lambda () t)) on-fail-fn)
  "Calls actionlib:call-goal `client' `goal' :timeout `timeout' :result-timeout `timeout'.
When timeout is reached the function `on-timeout-fn' is called.
If `on-timeout' returns t, the process will be repeated `max-retry-intents' times.
When call-goal fails or `max-retry-intents' is reached, `on-fail-fn' will be executed
and `trowable' will be thrown."
  (let ((intents 1)
        (keep-looping t))
    (loop while keep-looping                
          do (block continue
               (multiple-value-bind (result status)
                   (actionlib:call-goal client goal :timeout timeout :result-timeout timeout)
                 (roslisp:ros-info (suturo-planning-pm-manipulation)
                                   "Result from call-goal: ~a~%" result)
                 (if (eq result nil)
                     (if (eq status :TIMEOUT)
                         (progn
                           (format t "Timeout reached.~%")
                           (format t "Number intents: ~a~%" intents)
                           (if (not (funcall on-timeout-fn))
                               (return))
                           (if (< intents max-retry-intents)
                               (progn
                                 (format t "Retrying~%")
                                 (incf intents)
                                 (return-from continue))
                               (progn
                                 (format t "Maximum number of intents reached.~%")
                                 (cpl:fail throwable)
                                 (funcall on-fail-fn)
                                 (return))))
                         (progn
                           (format t "Unhandled condition.~%")
                           (cpl:error 'suturo-planning-common::unhandled-condition)
                           (return)))
                     (roslisp:with-fields (succ) result
                       (if (eq succ nil)
                           (progn
                             (format t "Unknown problem.~%")
                             (funcall on-fail-fn)
                             (cpl:error throwable)
                             (return))
                           (roslisp:with-fields (type) succ
                             (setf keep-looping nil)
                             (handle-action-answer type throwable)
                             (roslisp:ros-info (suturo-pm-manipulation)
                                               "Action finished successfully.")
                             (return))))))))))

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