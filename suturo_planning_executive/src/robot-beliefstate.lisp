(in-package :suturo-planning-executive)

(defmethod init-ms-belief-state ()
  (crs:prolog `(btr:clear-bullet-world))
  (let* ((urdf-kitchen
           (cl-urdf:parse-urdf
            (roslisp:get-param "kitchen_description")))
         (scene-rot-quaternion (tf:euler->quaternion :az pi))
         (scene-rot `(,(tf:x scene-rot-quaternion)
                      ,(tf:y scene-rot-quaternion)
                      ,(tf:z scene-rot-quaternion)
                      ,(tf:w scene-rot-quaternion)))
         (scene-trans `(-3.45 -4.35 0))
         (bdgs
           (car
            (force-ll
             (crs:prolog
              `(and (btr:clear-bullet-world)
                    (btr:bullet-world ?w)
                    (btr:assert (btr:object
                                 ?w btr:static-plane floor
                                 ((0 0 0) (0 0 0 1))
                                 :normal (0 0 1) :constant 0))
                    (assert (btr:object
                             ?w btr:semantic-map sem-map-kitchen
                             (,scene-trans ,scene-rot)
                             :urdf ,urdf-kitchen))))))))
    (var-value
     '?sem-map
     (lazy-car
      (crs:prolog
       `(btr:%object ?w sem-map-kitchen ?sem-map) bdgs)))))
