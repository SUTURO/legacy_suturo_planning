(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj ?prep ?loc))
  "All objects matching the description of ?obj1 will be put on ?obj2"
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
  "Puts the object described by ?obj1 on ?obj2, fails if there are more
   than one object matching the description of ?obj1"
  (perceive `(,?obj))
  (if (> (length (get-equal-designators ?obj)) 1)
      (cpl:fail 'ambiguous-description))
  (achieve `(a ,?obj ,?prep ,?loc)))   

(def-goal (achieve (a ?obj on ?loc))
  "Puts one object matching the description of ?obj1 on ?obj2"
  (let ((obj nil)
        (loc-to-place (locate ?loc)))
    (with-perceived-objects ((objs-to-place ?obj))
      (setf obj (pop objs-to-place))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-place
               (setf obj (pop objs-to-place ))
               (retry)))
           (place-failed (f)
             (declare (ignore f))
             ;;get new loc-to-place
             ))
        (achieve `(in-gripper ,obj))
        (achieve `(robot-at ,(make-designator 'location 
                                              `((to-execute put-down) 
                                                (at ,loc-to-place)))))
        (perform (make-designator 'action 
                                  `((to put-down) 
                                    (at ,loc-to-place))))))))

(def-goal (achieve (a ?obj in ?loc))
  "Drops a object matching the description of ?obj in ?loc"
  (let ((obj nil)
        (container (locate ?loc)))
    (with-perceived-objects ((objs-to-put ?obj))
      (setf obj (pop objs-to-put))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-put
               (setf obj (pop objs-to-put))
               (retry)))
           (simple-plan-failure (f)  
             (declare (ignore f))
             ;; new loc
             (retry))))
        (achieve `(in-gripper ,obj))
        (achieve `(at-location (locate ,(make-designator 'location 
                                                         `((to-execute drop) 
                                                           (obj ,obj) 
                                                           (in ,container))))))
        (perform (make-designator 'action 
                                  `((to drop) 
                                    (obj ,obj) 
                                    (in ,container)))))))

