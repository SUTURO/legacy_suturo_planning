(in-package :suturo-planning-common)

(defmacro error-out (domain msg &rest arg)
  (let
      ((str (apply #'format nil msg arg)))
    (progn
      (eval `(roslisp:ros-error ,domain ,str))
      (speak str))))

(defmacro info-out (domain msg &rest arg)
  (let
      ((str (apply #'format nil msg arg)))
    (progn
      (eval `(roslisp:ros-info ,domain ,str))
      (speak str))))

(defun speak (msg)
  (speak-with-voice msg "voice_kal_diphone"))

(defun speak-with-voice (msg voice)
  (let ((pub (advertise "robotsound" "sound_play/SoundRequest")))
    (publish-msg pub :sound -3 :command 1 :arg msg :arg2 voice)))
