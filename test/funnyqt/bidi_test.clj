(ns funnyqt.bidi-test
  (:use funnyqt.bidi)
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [funnyqt.relational.tg :as rtg]
            [funnyqt.tg :as tg]
            [funnyqt.visualization :as viz]
            [clojure.test :as test]))

(rtg/generate-schema-relations "test/input/greqltestgraph-with-cr-ids.tg" rm)

(deftransformation route-map2route-map [g1 g2]
  (^:top county2county
         :left [(rm/+County g1 ?c1)
                (rm/+name g1 ?c1 ?n)
                (rm/+tags g1 ?c1 ?t)]
         :right [(rm/+County g2 ?c2)
                 (rm/+name g2 ?c2 ?n)
                 (rm/+tags g1 ?c2 ?t)]
         :where [(locality2locality :?county1 ?c1 :?county2 ?c2)
                 (capital2capital :?county1 ?c1 :?county2 ?c2)])
  (locality2locality
   :left [;;(rm/+ContainsLocality g1 ?cl1 ?county1 ?loc1)
          (rm/+->localities g1 ?county1 ?loc1)
          (rtg/typeo g1 ?loc1 ?ct)
          (rm/+name g1 ?loc1 ?n)
          (rm/+foundingDate g1 ?loc1 ?fd)
          (rm/+inhabitants g1 ?loc1 ?inh)]
   :right [;;(rm/+ContainsLocality g2 ?cl2 ?county2 ?loc2)
           (rm/+->localities g2 ?county2 ?loc2)
           (rtg/typeo g2 ?loc2 ?ct)
           (rm/+name g2 ?loc2 ?n)
           (rm/+foundingDate g2 ?loc2 ?fd)
           (rm/+inhabitants g2 ?loc2 ?inh)]
   :where [(crossroad2crossroad :?loc1 ?loc1 :?loc2 ?loc2)])
  (capital2capital
   :left [;;(rm/+HasCapital g1 ?has-cap1 ?county1 ?cap1)
          (rm/+->capital g1 ?county1 ?cap1)
          (rm/+name g1 ?cap1 ?n)]
   :right [;;(rm/+HasCapital g2 ?has-cap2 ?county2 ?cap2)
           (rm/+->capital g2 ?county2 ?cap2)
           (rm/+name g2 ?cap2 ?n)])
  (crossroad2crossroad
   :left [;;(rm/+ContainsCrossroad g1 ?cc1 ?loc1 ?cr1)
          (rm/+->crossroads g1 ?loc1 ?cr1)
          (rm/+id g1 ?cr1 ?crid)
          (rtg/typeo g1 ?cr1 ?ct)]
   :right [;;(rm/+ContainsCrossroad g2 ?cc2 ?loc2 ?cr2)
           (rm/+->crossroads g2 ?loc2 ?cr2)
           (rm/+id g2 ?cr2 ?crid)
           (rtg/typeo g2 ?cr2 ?ct)]
   :optional [(maybe-names :?cr1 ?cr1 :?cr2 ?cr2)])
  (maybe-names
   :left [(rm/+name g1 ?cr1 ?n)]
   :right [(rm/+name g2 ?cr2 ?n)])
  #_(^:top connection2connection
         :when [(relateo :crossroad2crossroad
                         :?cr1 ?start1 :?cr2 ?start2)
                (relateo :crossroad2crossroad
                         :?cr1 ?end1 :?cr2 ?end2)]
         :left [(rm/+Connection g1 ?con1 ?start1 ?end1)
                (rtg/typeo g1 ?con1 ?con-type)]
         :right [(rm/+Connection g2 ?con2 ?start2 ?end2)
                 (rtg/typeo g2 ?con2 ?con-type)])
  (^:top airroute2airroute
         :when [(relateo :locality2locality
                         :?loc1 ?start1 :?loc2 ?start2)
                (relateo :locality2locality
                         :?loc1 ?end1 :?loc2 ?end2)]
         :left [(rm/+->dstAirport g1 ?start1 ?end1)]
         :right [(rm/+->dstAirport g2 ?start2 ?end2)]))

(test/deftest routemap-left-to-right
  (let [rm1 (tg/load-graph "test/input/greqltestgraph-with-cr-ids.tg")
        rm2 (tg/create-graph (tg/load-schema "test/input/greqltestgraph-with-cr-ids.tg"))]
    (println "Transforming rm1 to rm2.")
    (time (route-map2route-map rm1 rm2 :right))
    (test/is (= 155 (tg/vcount rm1)))
    (test/is (= 355 (tg/ecount rm1)))
    (test/is (= 146 (tg/vcount rm2)))
    (test/is (= 340 (tg/ecount rm2)))
    (viz/print-model rm2 :gtk)))

(test/deftest routemap-sync
  (let [rm1 (tg/load-graph "test/input/greqltestgraph-with-cr-ids.tg")
        rm2 (let [g (tg/create-graph (tg/load-schema "test/input/greqltestgraph-with-cr-ids.tg"))
                  eh (tg/create-vertex! g 'Village)
                  rlp (tg/create-vertex! g 'County)]
              ;; During the :left transformation, the new Village Ebernhahn +
              ;; the new ContainsLocality edge should be propagated back to the
              ;; rm1 graph.
              (tg/set-value! rlp :name "Rheinland-Pfalz")
              (tg/set-value! rlp :tags {"AREA" 19853.36})
              (tg/create-edge! g 'ContainsLocality rlp eh)
              (tg/set-value! eh :name "Ebernhahn")
              (tg/set-value! eh :inhabitants (int 1228))
              g)]
    (println "Transforming rm1 to rm2.")
    (time (route-map2route-map rm1 rm2 :right))
    (test/is (= 155 (tg/vcount rm1)))
    (test/is (= 355 (tg/ecount rm1)))
    (test/is (= 147 (tg/vcount rm2)))
    (test/is (= 341 (tg/ecount rm2)))
    (viz/print-model rm2 :gtk)
    (println "Transforming rm2 to rm1.")
    (time (route-map2route-map rm1 rm2 :left))
    (test/is (= 156 (tg/vcount rm1)))
    (test/is (= 356 (tg/ecount rm1)))
    (test/is (= 147 (tg/vcount rm2)))
    (test/is (= 341 (tg/ecount rm2)))))

#_(do (time (route-map2route-map rm1 rm2 :right))
         ((juxt tg/vcount tg/ecount) rm2))

#_(route-map2route-map rm1 rm2 :right)
#_(viz/print-model rm2 ".gtk")
#_(tg/save-graph rm2 "/home/horn/copy.tg")


