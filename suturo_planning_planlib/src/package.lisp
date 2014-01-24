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
                     #:start-monitoring-gripper
                     #:end-monitoring-gripper
                     #:gripper-is-closed
                     #:monitor-gripper
                     #:update-semantic-map
                     #:get-container-objects
                     #:get-graspable-objects
                     #:placed-object-in-box
                     #:storage-for-food
                     #:storage-for-stuff))
