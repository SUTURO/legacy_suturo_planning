(defsystem suturo-planning-pm-gripper-monitor
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (designators-ros
               process-modules
               cram-plan-failures
               cram-plan-knowledge
               cram-projection
               suturo-planning-common
               sensor_msgs-msg
               cl-transforms)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pm-gripper-monitor" :depends-on ("package" "designators" "action-handlers"))))))
