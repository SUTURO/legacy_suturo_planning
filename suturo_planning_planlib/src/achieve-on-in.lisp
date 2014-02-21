(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj1 ?prep ?obj2))
  "All objects matching the description of ?obj1 will be put on ?obj2"
  (let ((obj nil))
    (with-perceived-objects ((objs ?obj1))
      (with-retry-counters ((new-obj-counter 3))
        (with-failure-handling 
            ((suturo-planning-common::simple-plan-failure (f)
               (declare (ignore f))
               (do-retry new-obj-counter
                 (retry))))
          (loop while objs
                do (setf obj (pop objs))
                   (with-retry-counters ((same-obj-counter 1))
                     (with-failure-handling 
                         ((suturo-planning-common::simple-plan-failure (f)
                            (declare (ignore f))
                            (do-retry same-obj-counter
                              (retry))
                            (append `(,obj) objs)))
                       (achieve `(the ,obj ,?prep ,?obj2))))))))))

(def-goal (achieve (the ?obj1 ?prep ?obj2))
  "Puts the object described by ?obj1 on ?obj2, fails if there are more
   than one object matching the description of ?obj1"
  (perceive `(,?obj1))
  (if (> (length (get-equal-designators ?obj1)) 1)
      (cpl:fail 'ambiguous-description)) ;missing real condition 
  (achieve `(a ,?obj1 ,?prep ?obj2)))   

(def-goal (achieve (a ?obj1 on ?obj2))
  "Puts one object matching the description of ?obj1 on ?obj2"
  (let ((obj nil)
        (place nil))
    (with-perceived-objects ((objs-to-place ?obj1)
                             (places-to-put ?obj2))
      (setf obj (pop objs-to-place))
      (setf place (pop places-to-put))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-place
               (setf obj (pop objs-to-place ))
               (retry)))
           (simple-plan-failure (f)  ;maybe change condition
             (declare (ignore f))
             (when places-to-put
               (setf place (pop places-to-put))
               (retry))))
        (let ((loc-put-down (locate `(,(make-designator 'location 
                                                        `((to put-down) 
                                                          (obj ,obj) 
                                                          (on ,place)))))))
          (achieve `(in-gripper ,obj))
          (achieve `(robot-at ,(make-designator 'location 
                                                `((to-execute put-down) 
                                                  (at ,loc-put-down)))))
          (perform (make-designator 'action 
                                    `((to put-down) 
                                      (at ,loc-put-down)))))))))
  
(def-goal (achieve (a ?obj1 in ?obj2))
  "Drops a object matching the description of ?obj1 in ?obj2"
  (let ((obj nil)
        (container nil))
    (with-perceived-objects ((objs-to-put ?obj1)
                             (containers ?obj2))
      (setf obj (pop objs-to-put))
      (setf container (pop containers))
      (with-failure-handling 
          ((grasping-failed (f)
             (declare (ignore f))
             (when objs-to-put
               (setf obj (pop objs-to-put))
               (retry)))
           (simple-plan-failure (f)  ;maybe change condition
             (declare (ignore f))
             (when containers
               (setf container (pop containers))
               (retry))))
        (achieve `(in-gripper ,obj))
        (achieve `(at-location (locate ,(make-designator 'location 
                                                         `((to-execute drop) 
                                                           (obj ,obj) 
                                                           (in ,container))))))
        (perform (make-designator 'action 
                                  `((to drop) 
                                    (obj ,obj) 
                                    (in ,container))))))))
