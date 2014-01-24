(defsystem suturo-planning-pm-manipulation
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (actionlib
               suturo_manipulation_msgs-msg
               designators-ros
               process-modules
               cram-plan-failures
               cram-plan-knowledge
               cram-projection
               cram-language
               cram-plan-library
               suturo-planning-common)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pm-manipulation" :depends-on ("package" "designators" "action-handlers"))))))
