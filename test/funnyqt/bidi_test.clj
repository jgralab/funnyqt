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
   :right [(+name g2 ?cr2 ?n)]))

#_(route-map2route-map rm1 rm2 :right)
#_(viz/print-model rm2 ".gtk")
#_(tg/save-graph rm2 "/home/horn/copy.tg")


