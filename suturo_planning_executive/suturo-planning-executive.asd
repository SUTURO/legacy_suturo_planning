(defsystem suturo-planning-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               designators-ros
               cram-roslisp-common
               alexandria
               cram-language
               suturo-planning-process-module
               ;suturo-planning-process-module-dummy
               suturo-planning-planlib)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-executive" :depends-on ("package"))))))
