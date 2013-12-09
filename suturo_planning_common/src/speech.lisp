(in-package :suturo-planning-common)

(defun error-out (msg)
  (seq
    (roslisp:ros-error (suturo planning) msg msg) ; pass it twice to match some lambda list
    (speak msg)))

(defun info-out (msg)
  (seq
    (roslisp:ros-info (suturo planning) msg msg) ; pass it twice to match some lambda list
    (speak msg)))

(defun speak (msg)
  (speak-with-voice msg "voice_kal_diphone"))

(defun speak-with-voice (msg voice)
  (let ((pub (advertise "robotsound" "sound_play/SoundRequest")))
    (publish-msg pub :sound -3 :command 1 :arg msg :arg2 voice)))
