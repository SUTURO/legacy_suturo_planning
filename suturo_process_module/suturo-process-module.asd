(defsystem suturo-process-module
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (designators-ros
               process-modules
               cram-roslisp-common
               cram-reasoning
               cram-projection
               alexandria)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators"))
             (:file "suturo-process-module"
              :depends-on ("package" "designators" "action-handlers"))))))
