(defsystem suturo-planning-pmd-knowledge
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (designators-ros
               cram-plan-failures
               cram-plan-knowledge
               cram-projection
               cram-plan-library
               process-modules)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pmd-knowledge"
              :depends-on ("package" "designators" "action-handlers"))))))
