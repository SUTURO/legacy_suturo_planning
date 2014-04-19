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
           json-prolog->designators
           ;; conditions
           monitor-not-started
           ambiguous-description
           grasping-failed
           pose-not-reached
           unhandled-body-part
           move-head-failed
           move-arm-failed
           move-base-failed
           drop-failed
           dropped-object
           location-not-reached
           place-failed
           unhandled-action-answer
           unhandled-condition
           get-holding-gripper
           get-gripper-frame
           get-last-gripper-pose
           no-object-with-that-description
           objs-in-on-failed
           calc-gripper-offset
           object-not-hold-by-any-gripper
           ;; tf
           pose-stamped->pose
           stamped-transform->transform
           pose-stamped->transform
           pose-stamped->stamped-transform
           transform
           transform->pose
           transform-get-pose
           transform-get-pose-stamped
           transform->origin
           transform->orientation
           transform-get-origin
           transform->origin-as-list
           transform->matrix
           transform->quaternion
           transform->quaternion-as-list
           transform-coords-to-frame
           cl-transforms-euler-degree->quaternion
           cl-transforms-euler-degree->quaternion-as-list
           ;;visualization
           publish-visualization-marker
           publish-visualization-marker2
           ;;designators
           desig-prop-value-concat
           pose-stamped->designator)
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
   #:pose
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
   #:in-pose
   #:in-offset
   #:over
   #:between
   #:coords
   #:left-gripper
   #:right-gripper))
