(defsystem suturo-planning-pmd-gripper-monitor
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (designators-ros
               process-modules
               roslisp
               cram-language
               cram-roslisp-common
               cram-reasoning
               cram-projection
               cram-plan-failures
               cram-plan-knowledge
               alexandria
               suturo-planning-common
               actionlib
               sensor_msgs-msg)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pmd-gripper-monitor" :depends-on ("package" "designators" "action-handlers"))))))
