(defsystem suturo-planning-planlib
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (designators-ros
               cram-plan-library
               cram-plan-failures
               cram-plan-knowledge
               cl-tf
               suturo-planning-common
               nav_msgs-msg
               location-costmap)

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-planlib" :depends-on ("package"))
     (:file "achieve-on-in" :depends-on ("package" "suturo-planning-planlib"))
     (:file "goals-knowledge" :depends-on ("package"))
     (:file "goals" :depends-on ("package" "suturo-planning-planlib" "goals-knowledge"))
     (:file "locate" :depends-on ("package" "suturo-planning-planlib"))))))
