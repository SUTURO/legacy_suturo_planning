(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pmd-knowledge
  (:nicknames :suturo-pmd-knowledge)
  (:use #:common-lisp
        #:crs
        #:cut
        #:desig
        #:designators-ros
        #:cram-roslisp-common
        #:cram-process-modules
        #:cram-plan-failures
        #:cram-plan-knowledge)
  (:export suturo-planning-pmd-knowledge)
  (:desig-properties #:to #:perceive #:obj #:ground #:touch
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:edible))
