(in-package :cl-user)

(desig-props:def-desig-package suturo-planlib
  (:use #:roslisp #:cram-utilities #:designators-ros
        #:cram-roslisp-common #:cram-designators
        #:cram-plan-knowledge #:cram-plan-library #:cpl
	#:cram-plan-failures)
  (:import-from #:cram-reasoning #:<- #:def-fact-group)
  (:export test1 initial a the all right left)
  (:desig-properties #:to #:perceive #:obj #:ground #:touch
                     #:type #:box #:bowl #:cutlery #:arm
                     #:color #:red #:white #:blue #:green
                     #:move #:at #:loc
                     #:name #:container #:category #:owner #:cornflakes
                     #:pose #:all #:a #:the :#edible :#left :#right))
