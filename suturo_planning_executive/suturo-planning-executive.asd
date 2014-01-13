(defsystem suturo-planning-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               designators-ros
               cram-roslisp-common
               alexandria
               cram-language
               suturo-planning-pm-manipulation
               suturo-planning-pm-knowledge
               suturo-planning-pmd-manipulation
               suturo-planning-pmd-knowledge
               suturo-planning-planlib
               suturo-planning-common)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-executive" :depends-on ("package"))))))
