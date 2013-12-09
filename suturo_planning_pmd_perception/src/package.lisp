(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pmd-perception
  (:nicknames :suturo-pmd-perception)
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
  (:export suturo-planning-pmd-perception)
  (:desig-properties #:to #:perceive #:obj
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:edible))
