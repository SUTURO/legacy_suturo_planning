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
           #:home-pose
           #:empty-hand
           #:hand-over)
  (:desig-properties #:to
                     #:arm 
                     #:left-arm
                     #:right-arm
                     #:left-gripper
                     #:right-gripper
                     #:on
                     #:at
                     #:in
                     #:name
                     #:type
                     #:frame
                     #:coords
                     #:obj 
                     #:edible
                     #:grasp 
                     #:take-pose
                     #:pose
                     #:move-head
                     #:direction
                     #:open-hand
                     #:move-arm
                     #:loc
                     #:keep-object-in-hand
                     #:gripper-is-closed
                     #:update-semantic-map
                     #:get-container-objects
                     #:get-graspable-objects
                     #:ground))
