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
           #:hand-over
           #:objects-and-boxes-perceived
           #:get-holding-arm)
  (:desig-properties #:to
                     #:a
                     #:the 
                     #:all
                     #:arm 
                     #:left-arm
                     #:right-arm
                     #:both-arms
                     #:head
                     #:left-gripper
                     #:right-gripper
                     #:all
                     #:body-part
                     #:on
                     #:at
                     #:in
                     #:name
                     #:type
                     #:frame
                     #:coords
                     #:obj 
                     #:container
                     #:use
                     #:edible
                     #:grasp 
                     #:take-pose
                     #:pose
                     #:move-head
                     #:direction
                     #:open-hand
                     #:move-arm
                     #:loc
                     #:initial
                     #:grip-force
                     #:start-monitoring-gripper
                     #:end-monitoring-gripper
                     #:gripper-is-closed
                     #:monitor-gripper
                     #:update-semantic-map
                     #:get-container-objects
                     #:get-graspable-objects
                     #:placed-object-in-box
                     #:get-best-arm
                     #:get-location-over
                     #:storage-for-food
                     #:storage-for-stuff))
