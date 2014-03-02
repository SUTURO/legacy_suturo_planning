(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj ?prep ?loc))
  "All objects matching the description of ?obj will be put on ?loc"
  (with-perceived-objects ((objs ?obj))
    (if (not objs)
        (fail 'no-object-with-that-description))
    (let ((obj nil))
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

(def-goal (achieve (a ?obj ?prep ?loc))
  "Puts the object described by ?obj on ?loc, fails if there are more
   than one object matching the description of ?obj"
  (with-perceived-objects ((objs ?obj))
    (if (not objs)
        (fail 'no-object-with-that-description))
    (let ((obj (pop objs)))
      (with-failure-handling 
          ((simple-plan-failure (f)
             (declare (ignore f))
             (when objs
               (setf obj (pop objs))
               (retry))))
        (achieve `(the ,obj ,?prep ,?loc))))))
      

(def-goal (achieve (the ?obj on ?loc))
  "Puts one object matching the description of ?obj on ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc)))))
    (with-failure-handling 
        ((move-base-failed (f)
           (declare (ignore f))
           );(next-different-location-solution loc-to-reach)
         (place-failed (f)
           (declare (ignore f))
           ));(next-different-location-solution ?loc)
      (achieve `(in-gripper ,?obj))
      (achieve `(robot-at ,loc-to-reach))
      (achieve `(,?obj placed ,?loc)))))

(def-goal (achieve (the ?obj in ?loc))
  "Drops a object matching the description of ?obj in ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc)))))
      (with-failure-handling 
          ((move-base-failed (f)
             (declare (ignore f))
             ));(next-different-location-solution loc-to-reach)
        (achieve `(in-gripper ,?obj))
        (achieve `(robot-at ,loc-to-reach))
        (achieve `(hand-over ,(make-designator 'object
                                               `((at ,?loc)))
                             ,(get-holding-arm (current-desig ?obj))))
        (achieve `(empty-hand ,(current-desig ?obj))))))

