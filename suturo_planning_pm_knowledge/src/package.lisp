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
  (:desig-properties #:to #:perceive #:obj #:ground #:update-semantic-map
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:edible))
