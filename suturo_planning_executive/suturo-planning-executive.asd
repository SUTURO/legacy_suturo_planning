(defsystem suturo-planning-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"

  :depends-on (suturo-planning-common
               suturo-planning-planlib
               suturo-planning-pm-manipulation
               suturo-planning-pmd-manipulation
               suturo-planning-pm-gripper-monitor
               suturo-planning-pmd-gripper-monitor
               suturo-planning-pm-knowledge
               suturo-planning-pmd-knowledge)

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "suturo-planning-executive" :depends-on ("package"))))))
