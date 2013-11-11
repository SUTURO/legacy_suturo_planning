(defsystem suturo-planlib
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               cram-language)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planlib" :depends-on ("package"))))))
