(in-package :suturo-planning-planlib)

(defparameter *last-seen* nil)

(def-goal (achieve (all ?obj ?prep ?loc))
  "All objects matching the description of ?obj will be put on ?loc"
    (let ((objs (perceive `(,?obj)))
          (obj nil)
          (first-obj nil))
      (if (not objs)
          (fail 'no-object-with-that-description))
      (with-retry-counters ((new-obj-counter 3))
        (with-failure-handling 
            () ;(simple-plan-failure (f)
               ;(declare (ignore f))
               ;(do-retry new-obj-counter
               ;  (retry))))
          (loop while objs
                do (setf obj (pop objs))
                   (if (not first-obj) (next-solution ?loc))
                   (with-retry-counters ((same-obj-counter 1))
                     (with-failure-handling 
                         () ;(simple-plan-failure (f)
                            ;(declare (ignore f))
                            ;(do-retry same-obj-counter
                            ;  (retry))
                            ;(append `(,obj) objs)))
                       (achieve `(the ,obj ,?prep ,?loc)))))))))

(def-goal (achieve (a ?obj ?prep ?loc))
  "Puts the object described by ?obj on ?loc, fails if there are more
   than one object matching the description of ?obj"
  (let ((objs (perceive `(,?obj)))
        (obj nil))
    (if (not objs)
        (fail 'no-object-with-that-description))
    (setf obj (pop objs))
    (with-failure-handling 
        ((simple-plan-failure (f)
           (declare (ignore f))
           (when objs
             (setf obj (pop objs))
             (retry))))
      (achieve `(the ,obj ,?prep ,?loc)))))
      
(def-goal (achieve (the ?obj on ?loc))
  "Puts one object matching the description of ?obj on ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc)))))
    (with-failure-handling 
        ((place-failed (f)
           (declare (ignore f))
           (retry-with-next-solution ?loc)
           (retry-with-next-solution loc-to-reach)))
      (setf *last-seen* ?obj)
      (achieve `(in-gripper ,?obj))
      (achieve '(home-pose both-arms))
      (achieve `(robot-at ,loc-to-reach))
      (achieve `(,?obj placed-gently ,?loc)))))

(def-goal (achieve (the ?obj in ?loc))
  "Drops a object matching the description of ?obj in ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc))))
                     (remove-object (action `((to placed-object-in-box)
                                              (obj ,?obj)
                                              (container ,?loc)))))
      (with-failure-handling 
          ()
        (setf *last-seen* ?obj)
        (achieve `(in-gripper ,?obj))
        (achieve '(home-pose both-arms))
        (achieve `(robot-at ,loc-to-reach))
        (achieve `(hand-over ,(make-designator 'object
                                               `((at ,?loc)))
                             ,(get-holding-arm (current-desig ?obj))))
        (achieve `(empty-hand ,(current-desig ?obj) ,?loc))
        (perform remove-object)
        (format t "END"))))


