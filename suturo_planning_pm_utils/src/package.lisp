(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-utils
  (:nicknames :sp-pm-utils)
  (:use #:common-lisp
        #:crs
        #:cut
        #:desig
        #:designators-ros
        #:cram-roslisp-common
        #:cram-process-modules
        #:cram-plan-failures
        #:cram-plan-knowledge)
  (:export suturo-planning-pm-utils)
  (:desig-properties #:to
                     #:obj
                     #:loc
                     #:coords
                     #:frame
                     #:at
                     #:left-arm
                     #:right-arm
                     #:get-best-arm
                     #:get-location-over))
