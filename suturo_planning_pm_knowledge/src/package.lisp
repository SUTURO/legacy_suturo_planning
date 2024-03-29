(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-knowledge
  (:nicknames :sp-knowledge)
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
                     #:unknown
                     #:props
                     #:use
                     #:grip-force
                     #:storage-for-stuff
                     #:storage-for-food
                     #:container
                     #:update-semantic-map
                     #:clear-maps
                     #:placed-object-in-box
                     #:get-objects-with-properties
                     #:update-objects-on
                     #:get-static-object
                     #:get-container-objects
                     #:get-graspable-objects
                     #:learn-object
                     #:learn-object-start
                     #:learn-object-learn
                     #:learn-object-abort
                     #:learn-object-finish
                     #:action
                     #:scan-barcode))
