(in-package :suturo-planning-speech-recognition)

(defparameter cb-mutex (sb-thread:make-mutex))

(defun start-listening ()
  (subscribe "/suturo/SpeechRecognitionCommand" "suturo_perception_msgs/SpeechRecognitionCommand"
             #'speech-callback))

(defun stop-listening (subscriber)
  (unsubscribe subscriber))

(defun speech-callback (msg)
  (sb-thread:with-recursive-lock (cb-mutex)
    (with-fields (command object) msg
      (ros-info (speech-recognition)
                "command: ~a~%object: ~a%" command object)
      (cond
        ((equal command "cleanup")
         (exec::clean-table))
        ((equal command "clean")
         (exec::clean-table))))))
        


