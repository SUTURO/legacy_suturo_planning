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
                     #:at
                     #:on
                     #:name
                     #:coords
                     #:frame
                     #:edible
                     #:obj
                     #:use
                     #:grip-force
                     #:storage-for-stuff
                     #:storage-for-food
                     #:container
                     #:update-semantic-map
                     #:clear-maps
                     #:placed-object-in-box
                     #:get-container-objects
                     #:get-graspable-objects))
