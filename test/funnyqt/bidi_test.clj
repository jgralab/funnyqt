(ns funnyqt.bidi-test
  (:use funnyqt.bidi)
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [funnyqt.relational.tg :as rtg]
            [funnyqt.tg :as tg]
            [funnyqt.visualization :as viz]))

(rtg/generate-schema-relations "test/input/greqltestgraph.tg")
(def rm1 (tg/load-graph "test/input/greqltestgraph.tg"))
(def rm2 (tg/create-graph (tg/load-schema "test/input/greqltestgraph.tg")))

(deftransformation route-map2route-map [g1 g2]
  (^:top county2county
         :left [(+County g1 ?c1)
                (+name g1 ?c1 ?n)
                (+tags g1 ?c1 ?t)]
         :right [(+County g2 ?c2)
                 (+name g2 ?c2 ?n)
                 (+tags g1 ?c2 ?t)]
         :where [(locality2locality :?county1 ?c1 :?county2 ?c2)
                 (capital2capital :?county1 ?c1 :?county2 ?c2)])
  (locality2locality
   :left [(+ContainsLocality g1 ?hc1 ?county1 ?loc1)
          (rtg/typeo g1 ?loc1 ?ct)
          (+name g1 ?loc1 ?n)
          (+foundingDate g1 ?loc1 ?fd)
          (+inhabitants g1 ?loc1 ?inh)]
   :right [(+ContainsLocality g2 ?hc2 ?county2 ?loc2)
           (rtg/typeo g1 ?loc2 ?ct)
           (+name g2 ?loc2 ?n)
           (+foundingDate g2 ?loc2 ?fd)
           (+inhabitants g2 ?loc2 ?inh)]
   :where [(crossroad2crossroad :?loc1 ?loc1 :?loc2 ?loc2)])
  (capital2capital
   :left [(+HasCapital g1 ?has-cap1 ?county1 ?cap1)
          (+name g1 ?cap1 ?n)]
   :right [(+HasCapital g2 ?has-cap2 ?county2 ?cap2)
           (+name g2 ?cap2 ?n)])
  (crossroad2crossroad
   :left [(+ContainsCrossroad g1 ?cc1 ?loc1 ?cr1)
          (rtg/typeo g1 ?cr1 ?ct)]
   :right [(+ContainsCrossroad g2 ?cc2 ?loc2 ?cr2)
           (rtg/typeo g2 ?cr2 ?ct)]
   :optional [(maybe-names :?cr1 ?cr1 :?cr2 ?cr2)])
  (maybe-names
   :left [(+name g1 ?cr1 ?n)]
   :right [(+name g2 ?cr2 ?n)])
  (^:top connection2connection
         :when [(relateo :crossroad2crossroad
                         :?cr1 ?start1 :?cr2 ?start2)
                (relateo :crossroad2crossroad
                         :?cr1 ?end1 :?cr2 ?end2)]
         :left [(+Connection g1 ?con1 ?start1 ?end1)
                (rtg/typeo g1 ?con1 ?con-type)]
         :right [(+Connection g2 ?con2 ?start2 ?end2)
                 (rtg/typeo g2 ?con2 ?con-type)])
  (^:top airroute2airroute
         :when [(relateo :locality2locality
                         :?loc1 ?start1 :?loc2 ?start2)
                (relateo :locality2locality
                         :?loc1 ?end1 :?loc2 ?end2)]
         :left [(+AirRoute g1 ?ar1 ?start1 ?end1)]
         :right [(+AirRoute g2 ?ar2 ?start2 ?end2)]))

#_(route-map2route-map rm1 rm2 :right)
#_(viz/print-model rm2 ".gtk")
#_(tg/save-graph rm2 "/home/horn/copy.tg")

;; New matches after rerunning the transformation for a second time.  What's
;; their cause?

;; |                      :?con2 |                     :?con1 |                  :?start1 |                :?start2 |           :?con-type |                     :?end2 |                    :?end1 |
;; |-----------------------------+----------------------------+---------------------------+-------------------------+----------------------+----------------------------+---------------------------|
;; |  +e341: connections.Highway |  +e55: connections.Highway |  v29: junctions.Crossroad | v3: junctions.Crossroad |  connections.Highway |  v132: junctions.Crossroad |  v35: junctions.Crossroad |
;; |  +e342: connections.Highway |  +e54: connections.Highway |  v30: junctions.Crossroad | v3: junctions.Crossroad |  connections.Highway |    v3: junctions.Crossroad |  v29: junctions.Crossroad |
;; |  +e343: connections.Highway |  +e42: connections.Highway |  v27: junctions.Crossroad | v3: junctions.Crossroad |  connections.Highway |  v141: junctions.Crossroad |  v41: junctions.Crossroad |
;; |   +e344: connections.Street |  +e110: connections.Street |      v16: junctions.Plaza |   v120: junctions.Plaza |   connections.Street |      v120: junctions.Plaza |      v17: junctions.Plaza |
;; |   +e345: connections.Street |   +e95: connections.Street |      v18: junctions.Plaza |   v120: junctions.Plaza |   connections.Street |    v3: junctions.Crossroad |  v87: junctions.Crossroad |
;; |  +e346: connections.Highway |  +e37: connections.Highway | v132: junctions.Crossroad | v3: junctions.Crossroad |  connections.Highway | v117: junctions.Roundabout | v14: junctions.Roundabout |
;; | +e347: connections.Footpath | +e16: connections.Footpath | v127: junctions.Crossroad | v3: junctions.Crossroad | connections.Footpath |    v3: junctions.Crossroad | v128: junctions.Crossroad |
;; |   +e348: connections.Street |  +e217: connections.Street | v124: junctions.Crossroad | v3: junctions.Crossroad |   connections.Street |      v120: junctions.Plaza |      v20: junctions.Plaza |
;; | +e349: connections.Footpath |  +e4: connections.Footpath |  v81: junctions.Crossroad | v3: junctions.Crossroad | connections.Footpath |      v120: junctions.Plaza |      v15: junctions.Plaza |

