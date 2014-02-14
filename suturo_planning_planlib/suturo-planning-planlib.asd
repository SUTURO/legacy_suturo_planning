(defsystem suturo-planning-planlib
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (designators-ros
               cram-plan-library
               cram-plan-failures
               cram-plan-knowledge
               cl-tf
               suturo-planning-common)

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-planlib" :depends-on ("package"))
     (:file "goals" :depends-on ("package" "suturo-planning-planlib"))
     (:file "locate" :depends-on ("package" "suturo-planning-planlib"))))))
