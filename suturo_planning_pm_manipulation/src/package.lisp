(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-manipulation
 (:nicknames :sp-manipulation)
 (:use #:common-lisp
       #:crs
       #:cut
       #:desig
       #:designators-ros
       #:cram-roslisp-common
       #:cram-process-modules
       #:cram-plan-failures
       #:cram-plan-knowledge
       #:suturo-planning-common)
  (:import-from alexandria ignore-some-conditions)

  (:export suturo-planning-pm-manipulation)
  (:desig-properties #:to
                     #:arm
                     #:in
                     #:on
                     #:both-arms
                     #:head
                     #:obj 
                     #:coords
                     #:grasp 
                     #:take-pose
                     #:initial
                     #:pose
                     #:body-part
                     #:move-head
                     #:direction
                     #:open-hand
                     #:move-arm
                     #:on
                     #:in-pose
                     #:in-offset
                     #:move-base
                     #:pose
                     #:loc
                     #:keep-object-in-hand
                     #:gripper-is-closed
                     #:get-gripper-state
                     #:name
                     #:frame
                     #:location
                     #:coords
                     #:dimensions
                     #:left-arm
                     #:right-arm
                     #:left-gripper
                     #:right-gripper
                     #:type
                     #:grip-force
                     #:grasping-action-grasp
                     #:grasping-action-drop
                     #:grasping-action-open
                     #:grasping-action-above
                     #:target-on
                     #:target-to-gripper
                     #:height
                     #:grasp-action
                     #:tolerance))
