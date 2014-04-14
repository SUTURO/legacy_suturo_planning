(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-speech-recognition
  (:nicknames :sp-speech-rec)
  (:use #:roslisp
        ;#:sb-thread
        #:cram-utilities
        #:designators-ros
        #:cram-roslisp-common
        #:cram-designators
        #:cram-plan-knowledge
        #:cpl
        #:cram-plan-failures
        #:cram-plan-library
        #:cram-language-designator-support
        #:cram-process-modules
        #:suturo-planning-common))