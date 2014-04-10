(in-package :suturo-planning-planlib)


(def-goal (achieve (?obj know-unknown-object))
    "The passed object `?obj' has to be positioned at different angles (90°, 135°, 225°, 270°, 315°, 405°) to the PR2's view axis. In every position the object has to be released from the holding gripper and the PR2 has to scan/take a picture of the object.

Initially the PR2 has to be positioned in front of the object. The object has to be placed on a table/surface with with enough space to manipulate the object's position/pose."
  (format t "Getting to know unknown object.~%")
  (let ((arm (get-best-arm ?obj)))
    (with-retry-counters ((grasp-obj-counter 2))
      (with-failure-handling
          ((suturo-planning-common::grasping-failed (f)
             (declare (ignore f))
             (format t "Failed to grasp object.~%")
             (do-retry grasp-obj-counter
               (format t "Trying again.~%")
               (retry))))
        (achieve `(object-in-hand ,?obj ,arm))))))