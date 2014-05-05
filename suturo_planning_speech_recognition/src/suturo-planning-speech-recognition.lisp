(in-package :suturo-planning-speech-recognition)

;; Mutex for the callback
(defparameter *queue-mutex* (sb-thread:make-mutex))

;; Subscriber for the speech-recognition topic
(defparameter *sub* nil)

(defparameter *plan-queue* nil)

(defun start-listening ()
  "Subscribes to the speech-recognition topic and sets the callback."
  (setf *sub* (subscribe "/suturo/SpeechRecognitionCommand" 
                         "suturo_perception_msgs/SpeechRecognitionCommand"
                         #'speech-callback))
  (loop while t
        do (execute-plan (sb-thread:with-recursive-lock (*queue-mutex*)
                           (pop *plan-queue*)))))  
(defun stop-listening ()
  "Unsubscribes from the speech-recognition topic."
  (if (not *sub*)
      (unsubscribe *sub*)))

(defun speech-callback (msg)
  "Processes the message from the speech-recognition topic and 
   executes a plan."
  (with-fields (command object) msg
    (ros-info (speech-recognition)
              "command: ~a~%object: ~a%" command object)
      (alexandria:switch (command :test #'equal)
        ("cleanup" (add-to-queue #'exec::clean-table))
        ("clean" (add-to-queue #'exec::clean-table)))))

(defun add-to-queue (plan)
  (sb-thread:with-recursive-lock (*queue-mutex*)
    (append `(,plan) *plan-queue*)))

(defun execute-plan (plan)
  "Executes the given plan.
   `plan' is a function that starts a plan."
  (info-out (speech-recognition) "Aye! Aye! Captain!")
  (funcall plan))
        


