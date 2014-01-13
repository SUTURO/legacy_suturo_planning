(defsystem suturo-planning-pm-manipulation
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
               suturo_manipulation_msgs-msg
               suturo-planning-common
               actionlib
               sensor_msgs-msg)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "gripper-monitor" :depends-on ("package" "designators" "action-handlers"))
             (:file "suturo-planning-pm-manipulation" :depends-on ("package" "designators" "action-handlers" "gripper-monitor"))))))
