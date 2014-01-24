(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-executive
  (:nicknames :exec)
  (:use #:roslisp 
        #:cram-utilities 
        #:designators-ros
        #:cram-roslisp-common 
        #:cram-designators
        #:cram-plan-knowledge 
        #:cpl
        #:cram-plan-failures
        #:cram-plan-library
        #:suturo-planning-common
        #:suturo-planning-planlib
        #:suturo-planning-pmd-manipulation
        #:suturo-planning-pmd-manipulation)
  (:desig-properties #:to 
                     #:grasp 
                     #:arm 
                     #:obj 
                     #:container
                     #:left-arm
                     #:use
                     #:get-graspable-objects
                     #:update-semantic-map
                     #:get-container-objects))
