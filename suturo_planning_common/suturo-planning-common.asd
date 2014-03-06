(defsystem suturo-planning-common
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (designators-ros
               cram-plan-library
               cram-plan-failures
               cram-plan-knowledge
               sound_play-msg
               roslisp-utilities
               cl-ppcre
               cl-transforms)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "speech" :depends-on ("package"))
     (:file "designator-parser" :depends-on ("package"))
     (:file "conditions" :depends-on ("package"))
     (:file "suturo-planning-common" :depends-on ("package"))))))
