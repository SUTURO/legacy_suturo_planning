(in-package :suturo-planning-pm-manipulation)

(defvar *man-topic-result* '(nil nil nil))

(defun with-lost-in-resultation-workaround
    (client goal timeout throwable topic topic-type
     &key (max-retry-intents 3)
       (on-timeout-fn #'(lambda () t))
       (on-fail-fn #'(lambda () t))
       (on-success-fn #'(lambda () t)))
  "Calls actionlib:call-goal `client' `goal' :timeout `timeout' :result-timeout `timeout'.
When timeout is reached the function `on-timeout-fn' is called.
If `on-timeout' returns t, the process will be repeated `max-retry-intents' times.
When call-goal fails or `max-retry-intents' is reached, `on-fail-fn' will be executed
and `trowable' will be thrown.
When call-goal succeeds, `on-success-fn' will be executed."
  (let ((intents 1)
        (keep-looping t)
        (time-begin (roslisp:ros-time))
        (time-now (roslisp:ros-time))
        (subscriber (subscribe-man-topic topic topic-type)))
    (loop while keep-looping                
          do (block continue
               (setf time-now (roslisp:ros-time))
               (format t "waiting: ~a~%" (actionlib:wait-for-server client))
               (format t "calling goal: ~a~%client: ~a~%timeout: ~a~%" goal client timeout)
               (actionlib:call-goal client goal :timeout 1 :result-timeout 1)
               (format t "*man-topic-result*: ~a~%time passed: ~a~%"
                       *man-topic-result* (< (- time-now time-begin) timeout))
               (loop while (and (man-topic-result-is-nil)
                                (< (- time-now time-begin) timeout))
                     do (format t "looping.~%   *man-topic-result*: ~a~%   time passed: ~a~%"
                                *man-topic-result* (< (- time-now time-begin) timeout))
                        (sleep 0.5)
                        (setf time-now (roslisp:ros-time)))
               (unsubscribe-man-topic subscriber)
               (let ((result (first *man-topic-result*))
                     (succ (second *man-topic-result*))
                     (type (third *man-topic-result*)))
                 (format t "*man-topic-result*: ~a~%" *man-topic-result*)
                 (if (and
                      (not (or result succ type))
                      (>= (- time-now time-begin) timeout))
                     (progn
                       (format t "Timeout reached.~%")
                       (format t "Number intents: ~a~%" intents)
                       (if (not (funcall on-timeout-fn))
                               (return))
                       (if (< intents max-retry-intents)
                               (progn
                                 (format t "Retrying~%")
                                 (incf intents)
                                 (setf time-begin (roslisp:ros-time))
                                 (return-from continue))
                               (progn
                                 (format t "Maximum number of intents reached.~%")
                                 (cpl:fail throwable)
                                 (funcall on-fail-fn)
                                 (return))))
                     (if (not succ)
                           (progn
                             (format t "Unknown problem.~%")
                             (funcall on-fail-fn)
                             (cpl:error throwable)
                             (return))
                           (progn
                             (setf keep-looping nil)
                             (handle-action-answer type throwable)
                             (roslisp:ros-info (suturo-pm-manipulation)
                                               "Action finished successfully.")
                             (funcall on-success-fn)
                             (return))))))))
  (setf *man-topic-result* '(nil nil nil)))

(defun subscribe-man-topic (topic topic-type)
  (roslisp:subscribe topic topic-type #'(lambda (msg) (get-man-topic-result msg))))

(defun unsubscribe-man-topic (subscriber)
  (roslisp:unsubscribe subscriber))

(defun man-topic-result-is-nil ()
  (and
   (not *man-topic-result*)
   (not (first *man-topic-result*))
   (not (second *man-topic-result*))
   (not (third *man-topic-result*))))


(defun get-man-topic-result (msg)
  (let ((res nil))
    (roslisp:with-fields (result) msg
      (if (not result)
          (setf res (list result nil nil))
          (roslisp:with-fields (succ) result
            (if (not succ)
                (setf res (list result succ nil))
                (roslisp:with-fields (type) succ
                  (setf res (list result succ type)))))))
    (setf *man-topic-result* res)))

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