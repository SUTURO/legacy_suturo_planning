(in-package :cl-user)

(desig-props:def-desig-package suturo-process-module
  (:nicknames :suturo-pm)
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
  (:export suturo-process-module)
  (:desig-properties #:to #:perceive #:obj #:ground
                     #:type #:box #:bowl #:cutlery
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose))
