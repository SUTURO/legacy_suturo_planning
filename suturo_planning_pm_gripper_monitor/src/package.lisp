(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-pm-gripper-monitor
 (:nicknames :suturo-pm-gripper-monitor)
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
  (:export suturo-planning-pm-gripper-monitor)
  (:desig-properties #:to
                     #:arm
                     #:left-arm
                     #:right-arm
                     #:gripper-is-closed
                     #:start-monitoring-gripper
                     #:end-monitoring-gripper
                     #:monitor-gripper))
