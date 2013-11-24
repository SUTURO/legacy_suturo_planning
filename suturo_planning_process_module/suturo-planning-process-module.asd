(defsystem suturo-planning-process-module
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
               suturo_perception_msgs-srv
               json_prolog-srv
               suturo_perception_msgs-msg
               json_prolog-srv
               cl-ppcre
               suturo_manipulation_msgs-msg
               suturo-planning-common
               actionlib
               cl-ppcre)

  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "util" :depends-on ("package"))
             (:file "designators" :depends-on ("package"))
             (:file "action-handlers" :depends-on ("package" "designators" "util"))
             (:file "suturo-planning-process-module"
              :depends-on ("package" "designators" "action-handlers"))))))
