(defsystem suturo-planning-common
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               designators-ros
               cram-roslisp-common
               cram-plan-library
               cram-reasoning
               cram-plan-knowledge
               cl-json-pl-client
               alexandria
      	       cram-plan-failures
               sound_play-msg
               cram-language
               cl-ppcre)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "speech" :depends-on ("package"))
     (:file "suturo-planning-common" :depends-on ("package" "speech"))))))
