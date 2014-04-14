(in-package :suturo-planning-common)

(defvar *adv* nil)

(defun common-init ()
  (setf *adv* (advertise "/robotsound" "sound_play/SoundRequest")))

(roslisp-utilities:register-ros-init-function common-init)

(defmacro error-out (domain msg &rest arg)
  `(let
      ((str (apply #'format nil ,msg (mapcar #'(lambda (x)
        (if (eq (type-of x) 'OBJECT-DESIGNATOR)
          (first (last (split-string (desig-prop-value x 'name) "#")))
          x)) (list ,@arg)))))
      (roslisp:ros-error ,domain ,msg ,@arg)
      (speak str)))

(defmacro info-out (domain msg &rest arg)
  `(let
      ((str (apply #'format nil ,msg (mapcar #'(lambda (x)
        (if (eq (type-of x) 'OBJECT-DESIGNATOR)
          (first (last (split-string (desig-prop-value x 'name) "#")))
          x)) (list ,@arg)))))
      (roslisp:ros-info ,domain ,msg ,@arg)
      (speak str)))

(defun speak (msg)
  (speak-with-voice msg "voice_kal_diphone"))

(defun speak-with-voice (msg voice)
    (publish-msg *adv* :sound -3 :command 1 :arg msg :arg2 voice))
