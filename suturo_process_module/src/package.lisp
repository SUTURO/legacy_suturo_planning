(in-package :cl-user)

(desig-props:def-desig-package suturo-process-module
  (:nicknames :suturo-pm)
  (:use #:common-lisp
        #:crs
        #:cut
        #:desig
        #:designators-ros
        #:cram-roslisp-common
        #:cram-process-modules)
  (:desig-properties #:to #:perceive #:obj #:ground
                     #:type #:box #:bowl #:cutlery
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose))
