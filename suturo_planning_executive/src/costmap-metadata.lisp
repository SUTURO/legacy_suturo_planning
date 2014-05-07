(in-package :suturo-planning-executive)

(def-fact-group costmap-metadata (desig-loc)

  (<- (costmap-size 25 25))
  (<- (costmap-origin -12.5 -12.5))
  (<- (costmap-resolution 0.05))
  
  (<- (costmap-padding 0.70)) ;; used to be 0.55
  (<- (costmap-manipulation-padding 0.55)) ;; used to be 0.4
  (<- (costmap-in-reach-distance 1.0))
  (<- (costmap-reach-minimal-distance 0.3)))