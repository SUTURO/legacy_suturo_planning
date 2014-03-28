(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-planlib
  (:nicknames :sp-planlib)
  (:use #:roslisp
        #:cram-utilities
        #:designators-ros
        #:cram-roslisp-common
        #:cram-designators
        #:cram-plan-knowledge
        #:cpl
        #:cram-plan-failures
        #:cram-plan-library
        #:cram-language-designator-support
        #:cram-process-modules
        #:suturo-planning-common)
  (:import-from #:cram-reasoning #:<- #:def-fact-group)
  (:export init-localize)
  (:desig-properties 
   #:achieve
   #:object-in-hand
   #:object-in-box
   #:objects-in-appropriate-boxes
   #:objects-perceived
   #:home-pose
   #:arm-at
   #:empty-hand
   #:hand-over
   #:objects-and-boxes-perceived
   #:get-holding-arm
   #:in-gripper
   #:robot-at
   #:to
   #:see
   #:reach
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
   #:dimensions
   #:container
   #:use
   #:edible
   #:grasp 
   #:take-pose
   #:pose
   #:props
   #:move-head
   #:move-arm
   #:move-base
   #:direction
   #:open-hand
   #:move-arm
   #:loc
   #:location
   #:initial
   #:grip-force
   #:start-monitoring-gripper
   #:end-monitoring-gripper
   #:gripper-is-closed
   #:monitor-gripper
   #:update-semantic-map
   #:get-container-objects
   #:get-graspable-objects
   #:get-objects-with-properties
   #:update-objects-on
   #:placed-object-in-box
   #:placed-gently
   #:get-best-arm
   #:get-location-over
   #:storage-for-food
   #:storage-for-stuff
   #:target-on))
