(defsystem suturo-planning-pmd-manipulation
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (designators-ros
               process-modules
               cram-plan-failures
               cram-plan-knowledge
               suturo-planning-common)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pmd-manipulation"
              :depends-on ("package" "designators" "action-handlers"))))))
