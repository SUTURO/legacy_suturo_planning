(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-knowledge
  (:nicknames :suturo-pm-knowledge)
  (:use #:common-lisp
        #:crs
        #:cut
        #:desig
        #:designators-ros
        #:cram-roslisp-common
        #:cram-process-modules
        #:cram-plan-failures
        #:cram-plan-knowledge)
  (:export suturo-planning-pm-knowledge)
  (:desig-properties #:to
                     #:ground
                     #:obj
                     #:update-semantic-map
                     #:get-container-objects
                     #:get-graspable-objects))