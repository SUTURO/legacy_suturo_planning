(in-package :suturo-planning-planlib)

(def-goal (achieve (?obj placed ?loc))
  "Places an object on a given location."
  (info-out (suturo planlib) "Placing object gently.")
  (sleep 1.0)
  (with-retry-counters ((place-retry-counter 2))
    (with-failure-handling
        ((suturo-planning-common::place-failed (f)
           (declare (ignore f))
           (error-out (suturo planlib) "Failed to place object gently on given location.")
           (sleep 2.0)
           (do-retry place-retry-counter
             (info-out (suturo planib) "Trying again.")
             (sleep 1.0)
             (retry))))
      (with-designators ((open-hand (action `((to open-hand)
                                              (obj ,?obj)))))
        (perform open-hand))))
  (info-out (suturo planlib) "Droped, object"))
