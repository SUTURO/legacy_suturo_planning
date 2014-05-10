(in-package :suturo-planning-planlib)

(def-goal (achieve (all ?obj-prep-locs))
  "Gets a list in the form ((obj1 prep1 loc1) (obj2 prep2 loc2) ...)
   and brings all objects to their specified location. The objects and
   locations have only to be generic descriptions."
  (let ((all-objs (perceive (list (mapcar #'first ?obj-prep-locs)))))
    (multiple-value-bind (known-objs unknown-objs) 
        (seperate-known-from-unknown-objects all-objs)
      ;; clean up known objects
      (loop for obj-prep-loc in ?obj-prep-locs
            do (destructuring-bind (?obj ?prep ?loc) obj-prep-loc
                 (achieve `(all ,(remove-if #'(lambda (obj)
                                                (not (object-matches-description obj ?obj)))
                                            known-objs) 
                                ,?prep ,?loc))))
      ;; scan unknown objects and clean them up
      (loop for unknown-obj in unknown-objs
            do (let ((obj (achieve `(scanned-from ,unknown-obj)))
                     (obj-prep-loc nil))
                 (when obj
                   (setf obj-prep-loc (find-if #'(lambda (x)
                                                   (object-matches-description obj 
                                                                               (first x)))
                                               ?obj-prep-locs))
                   (destructuring-bind (?obj ?prep ?loc) obj-prep-loc
                     (achieve `(the ,?obj ,?prep ,?loc)))))))))

(def-goal (achieve (all ?objs ?prep ?loc))
  "Gets a list of objects and puts them in/on the location."
  (let ((objs ?objs)
        (obj nil)
        (failed-objs nil)
        (first-obj t)
        (nr-of-objs 0))
    (with-failure-handling
        ((objs-in-on-failed (f)
           (declare (ignore f))
           ;; when not all objects failed
           (when (< (length failed-objs) nr-of-objs)
             (setf objs failed-objs)
             (setf failed-objs nil)
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
      (if failed-objs
          (fail 'objs-in-on-failed)))))


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
  (unless (desig-prop-value (current-desig obj) 'in)
    (achieve '(home-pose both-arms))
    (achieve `(in-gripper ,obj))
    (achieve `(robot-at ,loc)))
  (achieve '(home-pose both-arms)))

(defun object-matches-description (obj desc)
  (eql (desig-prop-value obj 'edible)
       (desig-prop-value desc 'edible)))

