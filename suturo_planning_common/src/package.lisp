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
           error-out)
  (:desig-properties 
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
   ;; Action - pose
   #:pose
   #:initial
   ;; Actions - attributes
   #:arm
   #:left-arm
   #:right-arm
   ;; Objects
   #:obj
   #:frame
   #:table
   #:box
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
   ;; Locations
   #:loc
   #:on
   #:in
   #:over
   #:between
   #:coords
   #:left-gripper
   #:right-gripper))
