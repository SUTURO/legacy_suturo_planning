(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj ?prep ?loc))
  "All objects matching the description of ?obj will be put on ?loc"
  (let ((obj nil))
    (with-perceived-objects ((objs ?obj))
      (with-retry-counters ((new-obj-counter 3))
        (with-failure-handling 
            ((simple-plan-failure (f)
               (declare (ignore f))
               (do-retry new-obj-counter
                 (retry))))
          (loop while objs
                do (setf obj (pop objs))
                   (with-retry-counters ((same-obj-counter 1))
                     (with-failure-handling 
                         ((simple-plan-failure (f)
                            (declare (ignore f))
                            (do-retry same-obj-counter
                              (retry))
                            (append `(,obj) objs)))
                       (achieve `(the ,obj ,?prep ,?loc))))))))))

(def-goal (achieve (the ?obj ?prep ?loc))
  "Puts the object described by ?obj on ?loc, fails if there are more
   than one object matching the description of ?obj"
  (perceive `(,?obj))
  (if (> (length (get-equal-designators ?obj)) 1)
      (cpl:fail 'ambiguous-description))
  (achieve `(a ,?obj ,?prep ,?loc)))   

(def-goal (achieve (a ?obj on ?loc))
  "Puts one object matching the description of ?obj on ?loc"
  (let ((obj nil))
    (with-perceived-objects ((objs-to-place ?obj))
      (setf obj (pop objs-to-place))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-place
               (setf obj (pop objs-to-place ))
               (retry))))
        (achieve `(in-gripper ,obj))
        (achieve `(robot-at ,(make-designator 'location 
                                              `((to reach) (loc ,?loc)))))
        (achieve `(,?obj placed ,?loc))))))

(def-goal (achieve (a ?obj in ?loc))
  "Drops a object matching the description of ?obj in ?loc"
  (let ((obj nil))
    (with-perceived-objects ((objs-to-put ?obj))
      (setf obj (pop objs-to-put))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-put
               (setf obj (pop objs-to-put))
               (retry))))
        (achieve `(in-gripper ,obj))
        (achieve `(robot-at ,(make-designator 'location 
                                              `((to reach) (loc ,?loc)))))
        (achieve `(hand-over ,(make-designator 'object
                                               '((name "traschcan")))
                             ,(get-holding-arm (current-desig obj))))
        (achieve `(empty-hand ,(current-desig obj)))))))

