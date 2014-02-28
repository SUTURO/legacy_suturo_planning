(defsystem suturo-planning-pm-knowledge
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (cram-json-prolog
               designators-ros
               process-modules
               cram-plan-failures
               cram-plan-knowledge
               cram-projection
               suturo-planning-common)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-planning-pm-knowledge"
              :depends-on ("package" "designators" "action-handlers"))))))
