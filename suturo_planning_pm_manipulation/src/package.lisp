(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-manipulation
  (:nicknames :suturo-pm-manipulation)
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
  (:export suturo-planning-pm-manipulation)
  (:desig-properties #:to #:move #:obj #:move-head #:take-object #:move-arm-over-box
                     #:type #:box #:bowl #:cutlery #:arm #:move-closer-to-other-arm
                     #:color #:red #:white #:blue #:green #:touch #:left #:right
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:edible))
