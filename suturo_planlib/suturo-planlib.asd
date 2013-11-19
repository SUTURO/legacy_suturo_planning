(defsystem suturo-planlib
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
               suturo-process-module)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planlib" :depends-on ("package"))))))
