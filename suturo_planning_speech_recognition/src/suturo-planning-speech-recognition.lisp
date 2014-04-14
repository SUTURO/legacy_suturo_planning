(in-package :suturo-planning-speech-recognition)

(defparameter cb-mutex (sb-thread:make-mutex))

(defun start-listening ()
  (subscribe "/suturo/topic" "suturo_perception_msgs/SpeechRecognitionCommand"
             #'speech-callback))

(defun stop-listening (subscriber)
  (unsubscribe subscriber))

(defun speech-callback (msg)
  (sb-thread:with-recursive-lock (cb-mutex)
    (with-fields (command object) msg
      (format t "~a ~a%" command object))))


