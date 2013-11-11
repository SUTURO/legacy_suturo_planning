(defsystem suturo-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"
  
  :depends-on (roslisp
               cram-language)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-executive" :depends-on ("package"))))))
