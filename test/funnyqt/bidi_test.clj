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
         :where [(loc2loc :?county1 ?c1 :?county2 ?c2)])
  (loc2loc
   :left [(+ContainsLocality g1 ?hc1 ?county1 ?c1)
          (+Locality g1 ?c1)
          (rtg/typeo g1 ?c1 ?ct)
          (+name g1 ?c1 ?n)
          (+foundingDate g1 ?c1 ?fd)
          (+inhabitants g1 ?c1 ?inh)]
   :right [(+ContainsLocality g2 ?hc2 ?county2 ?c2)
           (+Locality g2 ?c2)
           (rtg/typeo g1 ?c2 ?ct)
           (+name g2 ?c2 ?n)
           (+foundingDate g2 ?c2 ?fd)
           (+inhabitants g2 ?c2 ?inh)]
   :where [(plaza2plaza :?loc1 ?c1 :?loc2 ?c2)])
  (plaza2plaza
   :left [(+ContainsCrossroad g1 ?cc1 ?loc1 ?plaza1)
          (+Plaza g1 ?plaza1)
          (+name g1 ?plaza1 ?n)]
   :right [(+ContainsCrossroad g2 ?cc2 ?loc2 ?plaza2)
           (+Plaza g2 ?plaza2)
           (+name g2 ?plaza2 ?n)]))

#_(route-map2route-map rm1 rm2 :right)
#_(viz/print-model rm2 ".gtk")

