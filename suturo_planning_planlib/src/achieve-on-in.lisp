(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj ?prep ?loc))
  "All objects matching the description of ?obj will be put on ?loc"
  (with-retry-counters ((perceive-again 1))
    (with-failure-handling
        ((objs-in-on-failed (f)
           (declare (ignore f))
           (do-retry perceive-again
             (retry))))
      (let ((objs (perceive `(,?obj)))
            (obj nil)
            (failed-objs nil)
            (first-obj t)
            (nr-of-objs 0))
         (multiple-value-bind (known-objects unknown-objects) 
             (seperate-known-from-unknown-objects objs)
           (setf objs known-objects)
           (with-failure-handling
               ((objs-in-on-failed (f)
                  (declare (ignore f))
                  ;; when not all objects failed
                  (when (< (length failed-objs) nr-of-objs)
                    (setf objs failed-objs)
                    (setf failed-objs nil)
                    (retry))
                  ;; try with unknown object if all known fail
                  (when unknown-objects
                    (setf objs unknown-objects)
                    (setf unknown-objects nil)
                    (retry)))
                (simple-plan-failure (f)
                  (unless (eql 'objs-in-on-failed (type-of f))
                    (push obj failed-objs)
                    (retry))))  
             (setf nr-of-objs (length objs))
             ;; loop through all objects and put them on/in the location
             (loop while objs
                   do (setf obj (pop objs))
                      ;; don't use next-location for the first object
                      (if first-obj
                          (setf first-obj nil)
                          (next-solution ?loc))
                      (achieve `(the ,obj ,?prep ,?loc)))
             ;; throw an error if at least one object failed
             (if (or failed-objs unknown-objects)
                 (fail 'objs-in-on-failed))))))))

(def-goal (achieve (a ?obj ?prep ?loc))
  "Puts one object matching the description of ?obj on ?loc"
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
  "Puts the object described by ?obj on ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc)))))
    (with-failure-handling 
        ((place-failed (f)
           (declare (ignore f))
           (retry-with-next-solution loc-to-reach)))
      (grasp-object ?obj loc-to-reach))
    (with-failure-handling 
        ((place-failed (f)
           (declare (ignore f))
           (retry-with-next-solution ?loc)))
      (achieve `(,?obj placed-gently ,?loc)))))

(def-goal (achieve (the ?obj in ?loc))
  "Drops the object ?obj in the loaction ?loc"
  (with-designators ((loc-to-reach (location `((to reach) (loc ,?loc))))
                     (remove-object (action `((to placed-object-in-box)
                                              (obj ,?obj)
                                              (container ,?loc)))))
    (grasp-object ?obj loc-to-reach)
    (with-failure-handling 
        ((move-arm-failed (f)
           (declare (ignore f))
           (retry-with-next-solution ?loc)))
      (achieve `(hand-over ,(make-designator 'object
                                             `((at ,?loc)))
                           ,(get-holding-arm (current-desig ?obj)))))
    (achieve `(empty-hand ,(current-desig ?obj) ,?loc))
    (perform remove-object)))

(defun grasp-object (obj loc)
  "Handles the whole grasping process."
  (achieve '(home-pose both-arms))
  (achieve `(in-gripper ,obj))
  (achieve `(robot-at ,loc))
  (achieve '(home-pose both-arms)))


