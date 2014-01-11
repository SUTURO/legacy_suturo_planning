(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-planlib
  (:use #:roslisp
        #:cram-utilities
        #:designators-ros
        #:cram-roslisp-common
        #:cram-designators
        #:cram-plan-knowledge
        #:cpl
        #:cram-plan-failures
        #:cram-plan-library
        #:suturo-planning-common)
  (:import-from #:cram-reasoning #:<- #:def-fact-group)
  (:export #:achieve
           #:object-in-hand
           #:object-in-box
           #:objects-in-appropriate-boxes
           #:objects-perceived
           #:initial-pose)
  (:desig-properties #:to
                     #:arm 
                     #:obj 
                     #:grasp 
                     #:take-pose
                     #:pose
                     #:move-head
                     #:direction
                     #:open-hand
                     #:move-arm
                     #:loc))
