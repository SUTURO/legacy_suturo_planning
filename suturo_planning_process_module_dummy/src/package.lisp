(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-process-module-dummy
  (:nicknames :suturo-pm-dummy)
  (:use #:common-lisp
        #:crs
        #:cut
        #:desig
        #:designators-ros
        #:cram-roslisp-common
        #:cram-process-modules
        #:cram-plan-failures
        #:cram-plan-knowledge)
  (:import-from alexandria ignore-some-conditions)
  (:export suturo-planning-process-module-dummy)
  (:desig-properties #:to #:perceive #:obj #:ground #:touch
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:edible))
