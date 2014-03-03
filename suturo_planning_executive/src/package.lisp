(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-executive
  (:nicknames :exec)
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
        #:suturo-planning-common
        #:suturo-planning-planlib
        #:suturo-planning-pmd-manipulation
        #:suturo-planning-pmd-manipulation
        #:location-costmap)
  (:import-from cram-reasoning #:<- #:def-fact-group)
  (:import-from semantic-map-utils semantic-map-name)
  (:desig-properties #:to 
                     #:grasp 
                     #:arm 
                     #:obj 
                     #:container
                     #:left-arm
                     #:use
                     #:get-graspable-objects
                     #:update-semantic-map
                     #:clear-maps
                     #:get-container-objects
                     #:achieve
                     #:object-in-hand
                     #:object-in-box
                     #:objects-in-appropriate-boxes
                     #:objects-perceived
                     #:home-pose
                     #:empty-hand
                     #:hand-over
                     #:objects-and-boxes-perceived
                     #:get-holding-arm
                     #:in-gripper))
