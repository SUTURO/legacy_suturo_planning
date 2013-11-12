(defsystem suturo-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               designators-ros
               cram-roslisp-common
               alexandria
               cram-language
               suturo-process-module
               suturo-planlib)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-executive" :depends-on ("package"))))))
