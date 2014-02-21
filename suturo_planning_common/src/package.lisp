(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-common
  (:use #:roslisp 
        #:cram-utilities 
        #:designators-ros
        #:cram-roslisp-common 
        #:cram-designators
        #:cram-plan-knowledge 
        #:cpl
    	  #:cram-plan-failures
        #:cram-plan-library)
  (:import-from #:cram-reasoning #:<- #:def-fact-group)
  (:export info-out
           error-out
           designator->string
           string->designators
           monitor-not-started
           grasping-failed
           pose-not-reached
           unhandled-body-part
           move-head-failed
           move-arm-failed
           drop-failed
           dropped-object
           location-not-reached
           place-failed
           unhandled-action-answer
           unhandled-condition)
  (:desig-properties 
   #:left
   #:right
   ;; Actions
   #:to  
   #:ground 
   #:update-semantic-map
   #:get-container-objects
   #:get-graspable-objects
   #:take-pose
   #:move-head
   #:move-arm
   #:grasp
   #:open-hand
   #:direction
   ;; Action - pose
   #:pose
   #:initial
   ;; Actions - attributes
   #:arm
   #:left-arm
   #:right-arm
   ;; Objects
   #:obj
   #:volume
   #:table
   #:box
   #:grip-force
   ;; Objects - type
   #:type
   #:container
   ;; Objects - use
   #:use
   #:storage-for-food
   #:storage-for-stuff
   ;; Objects - attributes
   #:at
   #:edible
   #:owner
   #:name
   #:color
   #:dimensions
   ;; Locations
   #:loc
   #:frame
   #:on
   #:in
   #:over
   #:between
   #:coords
   #:left-gripper
   #:right-gripper))
