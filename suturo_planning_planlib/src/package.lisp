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
   #:gripper-rotated
   #:know-unknown-object
   #:home-pose
   #:drive-pose
   #:arm-at
   #:empty-hand
   #:hand-over
   #:objects-and-boxes-perceived
   #:examine-unknown-objects
   #:get-holding-arm
   #:in-gripper
   #:robot-at
   #:to
   #:see
   #:reach
   #:a
   #:the 
   #:arm 
   #:left-arm
   #:right-arm
   #:both-arms
   #:both-arms-move
   #:head
   #:left-arm-campose
   #:left-gripper
   #:right-gripper
   #:all
   #:body-part
   #:on
   #:in-pose
   #:in-offset
   #:at
   #:in
   #:name
   #:unknown
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
   #:get-static-object
   #:update-objects-on
   #:placed-object-in-box
   #:placed-gently
   #:placed-gently-location
   #:get-best-arm
   #:get-location-over
   #:storage-for-food
   #:storage-for-stuff
   #:target-on
   #:height
   #:target-to-gripper
   #:grasp-action
   #:tolerance
   #:grasping-action-grasp
   #:grasping-action-drop
   #:grasping-action-open
   #:grasping-action-above
   #:action
   #:learn-object
   #:learn-object-start
   #:learn-object-learn
   #:learn-object-abort
   #:learn-object-finish
   #:scanned-from
   #:scan-barcode))
