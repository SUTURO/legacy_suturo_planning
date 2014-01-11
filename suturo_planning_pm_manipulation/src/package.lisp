(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-manipulation
  (:nicknames :suturo-pm-manipulation)
  (:use #:cut
        #:cpl
        #:designators-ros
        #:cram-designators
        #:cram-roslisp-common
        #:cram-process-modules
        #:cram-plan-failures
        #:cram-plan-knowledge
        #:cram-plan-library
        #:cram-utilities
        #:roslisp
        #:suturo-planning-common)
  (:import-from alexandria ignore-some-conditions)
  (:export suturo-planning-pm-manipulation)
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
                     #:loc
                     #:keep-object-in-hand
                     #:gripper-is-closed))
