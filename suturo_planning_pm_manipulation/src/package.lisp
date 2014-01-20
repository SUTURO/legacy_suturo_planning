(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-manipulation
 (:nicknames :suturo-pm-manipulation)
 (:use #:common-lisp
       #:crs
       #:cut
       #:desig
       #:designators-ros
       #:cram-roslisp-common
       #:cram-process-modules
       #:cram-plan-failures
       #:cram-plan-knowledge)
  (:import-from alexandria ignore-some-conditions)
  (:export suturo-planning-pm-manipulation)
  (:desig-properties #:to
                     #:arm
                     #:arms
                     #:head
                     #:obj 
                     #:grasp 
                     #:take-pose
                     #:initial
                     #:pose
                     #:body-part
                     #:move-head
                     #:direction
                     #:open-hand
                     #:move-arm
                     #:loc
                     #:keep-object-in-hand
                     #:gripper-is-closed
                     #:name
                     #:frame
                     #:left-arm
                     #:right-arm
                     #:type
                     #:grip-force))
