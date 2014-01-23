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
  (:desig-properties #:to
                     #:ground
                     #:obj
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
                     #:get-graspable-objects))
