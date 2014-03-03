(defsystem suturo-planning-executive
  :author "SUTURO Planning <suturo-planning@cs.uni-bremen.de>"

  :depends-on (designators-ros
               cram-plan-library
               cram-plan-failures
               cram-plan-knowledge
               roslisp-utilities
               semantic-map-costmap
               suturo-planning-common
               suturo-planning-planlib
               suturo-planning-pm-manipulation
               suturo-planning-pmd-manipulation
               suturo-planning-pm-gripper-monitor
               suturo-planning-pmd-gripper-monitor
               suturo-planning-pm-knowledge
               suturo-planning-pmd-knowledge
               suturo-planning-pm-utils)

  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "costmap-metadata" :depends-on ("package"))
     (:file "sem-map-config" :depends-on ("package"))
     (:file "suturo-planning-executive" :depends-on ("package" "costmap-metadata" "sem-map-config"))))))
