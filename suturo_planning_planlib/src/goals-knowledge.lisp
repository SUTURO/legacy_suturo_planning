(in-package :suturo-planning-planlib)

(def-goal (perceive (?obj))
  (let ((objs (perform (make-designator 'action 
                                        `((to get-objects-with-properties) 
                                          (obj ,?obj))))))
    (if (not objs)
      (let* ((loc (desig-prop-value ?obj 'at))
             (name (desig-prop-value loc 'name))
             (on-obj (get-furniture name))
             (coords (get-coords on-obj))
             (frame (desig-prop-value (desig-prop-value on-obj 'at) 'frame)))
        
        (setf (nth 2 coords) (+ (nth 2 coords) 0.6))
        (achieve `(robot-at ,(make-designator 'location 
                                              `((to see) (name ,name)))))
        (perform (make-designator 'action 
                                  `((to move-head) 
                                    (loc ,(make-designator 'location
                                                           `((coords ,coords)
                                                             (frame ,frame)))))))
        (perform (make-designator 'action '((to update-semantic-map))))
        (perform (make-designator 'action 
                                  `((to get-objects-with-properties) 
                                    (obj ,?obj)))))
      objs)))
; get objs from knwoledge
; if not objs
;   get 'on from description -> o
;   get o from knowledge
;   if o
;     drive to coords(o) + some offset
;     update semantic map
;     get objs from knowledge
;   else fail
;     
