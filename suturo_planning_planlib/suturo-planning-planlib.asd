(defsystem suturo-planning-planlib
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               designators-ros
               cram-roslisp-common
               cram-plan-library
               cram-reasoning
               cram-plan-knowledge
               alexandria
      	       cram-plan-failures
               cram-language
               suturo-planning-common
               sensor_msgs-msg)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-planlib" :depends-on ("package"))
     (:file "gripper-monitor" :depends-on ("package" "suturo-planning-planlib"))
     (:file "goals" :depends-on ("package" "suturo-planning-planlib" "gripper-monitor"))))))
