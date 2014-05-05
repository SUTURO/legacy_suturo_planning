(defsystem suturo-planning-speech-recognition
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
 
  :depends-on (alexandria
               designators-ros
               cram-plan-library
               cram-plan-failures
               cram-plan-knowledge
               cram-language-designator-support
               suturo-planning-common
               suturo-planning-executive
               suturo_perception_msgs-msg
               process-modules)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-speech-recognition" :depends-on ("package"))))))