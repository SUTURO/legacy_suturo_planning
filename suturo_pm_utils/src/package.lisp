(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-utils
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
  (:export suturo-planning-pm-utils)
  (:desig-properties #:to
                     #:ground
                     #:obj
                     #:container
                     #:name
                     #:coords
                     #:type
                     #:frame
                     #:at
                     #:edible
                     #:use
                     #:storage-for-food
                     #:storage-for-stuff
                     #:update-semantic-map
                     #:get-container-objects
                     #:get-graspable-objects
                     #:placed-object-in-box))
