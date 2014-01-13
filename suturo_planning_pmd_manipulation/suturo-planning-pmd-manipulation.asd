(defsystem suturo-planning-pmd-manipulation
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (designators-ros
               process-modules
               roslisp
               cram-roslisp-common
               cram-reasoning
               cram-projection
               cram-plan-failures
               cram-plan-knowledge
               alexandria
               suturo_manipulation_msgs-msg
               suturo-planning-common
               actionlib)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pmd-manipulation"
              :depends-on ("package" "designators" "action-handlers"))))))
