(in-package :cl-user)

(desig-props:def-desig-package suturo-planning-common
  (:use #:roslisp #:cram-utilities #:designators-ros
        #:cram-roslisp-common #:cram-designators
        #:cram-plan-knowledge #:cram-plan-library #:cpl
    	  #:cram-plan-failures)
  (:import-from #:cram-reasoning #:<- #:def-fact-group)
  (:export initial right left all the)
  (:desig-properties #:to #:perceive #:obj #:ground #:touch
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:all #:a #:the #:edible #:left #:right))
