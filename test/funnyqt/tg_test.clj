(ns funnyqt.tg-test
  (:use [flatland.ordered.set])
  (:use [funnyqt.tg])
  (:use funnyqt.generic)
  (:use [clojure.test])
  (:import (de.uni_koblenz.jgralab Graph Vertex Edge GraphIO)))

;;* The test graphs

(defonce rg (load-graph "test/input/greqltestgraph.tg"))
(defonce jg (load-graph "test/input/medium-model.tg"))

;;* Tests

(deftest test-value
  (let [winningen (vertex rg 4)]
    ;; Normal attribute access
    (is (= "Winningen" (value winningen :name)))
    (is (= 2432 (value winningen :inhabitants)))
    (let [fd (value winningen :foundingDate)]
      ;; Access to record components
      (is (= 1 (value fd :day)))
      (is (= "JAN" (str (value fd :month))))
      (is (= 1016 (value fd :year))))))

(deftest test-average-inhabitants
  (let [locs (vseq rg 'localities.Locality)]
    (is (< 0.00000000000000000001 ;; epsilon
           (- 91079.63636363637   ;; the GReQL computed val
              (/ (reduce + (map #(value %1 :inhabitants)
                                locs))))))))

(deftest test-this
  (doseq [v (vseq rg)
          e (iseq v)]
    (is (= v (this e)))))


(deftest test-schema-imports-1
  (let [l  'localities.Locality
        hc 'localities.HasCapital
        ct 'localities.CountyTags]
    (is (= (attributed-element-class rg 'localities.Locality)
           (attributed-element-class rg l)))
    (is (= (attributed-element-class rg 'localities.HasCapital)
           (attributed-element-class rg hc)))
    (is (= (domain rg 'localities.CountyTags)
           (domain rg ct)))))

(deftest test-create-graph-vertex-edge-1
  (let [g ^Graph (new-graph (schema rg) "Test graph 1")
        v1 (create-vertex! g 'localities.City)
        v2 (create-vertex! g 'junctions.Crossroad)
        v3 (create-vertex! g 'localities.City)
        ;; Also test the generic create function...
        v4 (create-element! g 'junctions.Crossroad)
        e1 (create-edge! g 'localities.ContainsCrossroad v1 v2)
        ;; Also test the generic adj setter function...
        e2 (add-adj! v3 :crossroads v4)
        e3 (create-edge! g 'connections.Street v2 v4)]
    (is (== 4 (.getVCount g)) "Wrong vertex count")
    (is (== 3 (.getECount g)) "Wrong edge count")))

(deftest test-add-adj-throws-on-single-valued-role
  (let [g ^Graph (new-graph (schema rg) "Test graph 1")
        v1 (create-vertex! g 'localities.City)
        v2 (create-vertex! g 'junctions.Crossroad)]
    (is (thrown? Exception
                 (add-adj! v2 :locality v1)))
    (is (thrown? Exception
                 (add-adjs! v2 :locality [v1])))))

(deftest test-vcount
  (is (= 155 (vcount rg) (count (vseq rg))))
  (is (= 3 (vcount rg 'localities.City)))
  (is (= 6808 (vcount jg) (count (vseq jg)))))

(deftest test-ecount
  (is (= 355 (ecount rg) (count (eseq rg))))
  ;; Edge has no direct instances
  (is (= 3 (ecount rg 'connections.AirRoute!)))
  (is (= 6801 (ecount jg) (count (eseq jg)))))

(deftest test-query-1
  (let [r (for [l (vseq rg 'localities.Locality)
                :when (re-matches #".*e.*" (value l 'name))]
            [l (value l 'name)])]
    ;; r must have 8 elems
    (is (= 8 (count r)))
    ;; the elems are...
    (is (= (map #(let [v (vertex rg %1)] [v (value v 'name)])
                [1 2 3 4 7 9 10 11])
           r))))

(deftest test-incidences
  (is (= 1 (count (iseq (vertex rg 1)))))
  (is (= 10 (count (iseq (vertex rg 12)))))
  (is (= 9 (count (iseq (vertex rg 12)
                        'localities.ContainsLocality!))))
  (is (= 1 (count (iseq (vertex rg 12)
                        'localities.HasCapital!))))
  (is (= 2 (count (iseq (vertex rg 6)))))
  (is (= 4 (count (iseq (vertex rg 11)))))
  (is (= 0 (count (iseq (vertex rg 6) nil :out)))))

(deftest test-is-instance?
  (let [g     rg
        gc    (attributed-element-class g)
        city  (vertex g 7)
        cityc (attributed-element-class city)
        hw    (edge g 28)
        hwc   (attributed-element-class hw)]
    (is (is-instance? g gc))
    (is (is-instance? city cityc))
    (is (is-instance? hw hwc))

    (let [loc (attributed-element-class g 'localities.Locality)]
      (is (not (is-instance? g loc)))
      (is (is-instance? city loc))
      (is (not (is-instance? hw loc))))))

(deftest test-class-access
  (let [g rg]
    (is (= (attributed-element-class g 'localities.Locality)
           (attributed-element-class g 'Locality)))
    (is (= (attributed-element-class g 'connections.AirRoute)
           (attributed-element-class g 'AirRoute)))))

;;** Traversal Context

(deftest test-vsubgraph-tc
  (let [vcnt (vcount rg)
        ecnt (ecount rg)]
    (testing "vertex induced TraversalContext by set"
      (on-subgraph [rg (vsubgraph rg (set (map #(vertex rg %)
                                               [1 12 7])))]
        (is (== 3 (vcount rg)))
        (is (== 2 (ecount rg)))
        (testing "on-graph 1"
          (on-graph [rg]
            (is (== vcnt (vcount rg)))
            (is (== ecnt (ecount rg)))))
        (testing "vertex/edge on subgraph"
          ;; These are all in
          (is (= [1 7 12] (map id (vseq rg))))
          (is (= [17 22]  (map id (eseq rg)))))))
    (testing "vertex induced TraversalContext by type"
      (on-subgraph [rg (vsubgraph rg 'junctions.Airport)]
        (is (== 3 (vcount rg)))
        (is (== 3 (ecount rg)))
        (testing "on-graph 2"
          (on-graph [rg]
            (is (== vcnt (vcount rg)))
            (is (== ecnt (ecount rg)))))))
    (testing "vertex induced TraversalContext by predicate"
      ;; Subgraph of all Locality vertices with more than 10 inhabitants.
      (let [locality? (type-matcher rg 'localities.Locality)]
        (on-subgraph [rg (vsubgraph rg
                                    #(and (locality? %)
                                          (> (value % :inhabitants) 10)))]
          (is (== 9 (vcount rg)))
          (testing "on-graph 3"
            (on-graph [rg nil]
              (is (== vcnt (vcount rg)))
              (is (== ecnt (ecount rg)))))
          (is (== 0 (ecount rg))))))))


(deftest test-esubgraph-tc
  (let [vcnt (vcount rg)
        ecnt (ecount rg)]
    (testing "edge induced TraversalContext by set"
      (on-subgraph [rg (esubgraph rg (set (map #(edge rg %)
                                               [17 22])))]
        (is (== 3 (vcount rg)))
        (is (== 2 (ecount rg)))
        (testing "on-graph 4"
          (on-graph [rg]
            (is (== vcnt (vcount rg)))
            (is (== ecnt (ecount rg)))))))
    (testing "edge induced TraversalContext by type"
      (on-subgraph [rg (esubgraph rg 'connections.AirRoute)]
        (is (== 3 (vcount rg)))
        (is (== 3 (ecount rg)))
        (testing "on-graph 5"
          (on-graph [rg]
            (is (== vcnt (vcount rg)))
            (is (== ecnt (ecount rg)))))))
    (testing "edge induced TraversalContext by predicate"
      (let [airroute? (type-matcher rg 'connections.AirRoute)]
        (on-subgraph [rg (esubgraph rg #(and (airroute? %)
                                             (== (value (alpha  %) :inhabitants) 0)))]
          (testing "on-graph 5"
            (on-graph [rg]
              (is (== vcnt (vcount rg)))
              (is (== ecnt (ecount rg)))))
          (is (== 2 (vcount rg)))
          (is (== 1 (ecount rg))))))))

(deftest test-subgraph-intersection-tcs
  (on-subgraph [rg (vsubgraph rg (set (map #(vertex rg %)
                                           [1 12 7])))]
    (on-subgraph-intersection [rg (esubgraph rg (set (map #(edge rg %)
                                                          [22 17])))]
      (is (== 3 (vcount rg)))
      (is (== 2 (ecount rg)))
      (on-subgraph-intersection [rg (esubgraph rg #{(edge rg 22)})]
        (is (== 2 (vcount rg)))
        (is (== 1 (ecount rg)))))))

(deftest test-create-element!
  (let [g (new-graph (schema rg))
        county (create-vertex! g 'County :name "Hessen")
        c1 (create-element! g 'City {:name "Wiesbaden"
                                     :county county})]
    (is (= 2 (vcount g)))
    (is (= 1 (ecount g) (ecount g 'ContainsLocality)))))

;;* Tests for the generated functional API

(generate-schema-functions "test/input/greqltestgraph.tg"
                           test.functional.routemap.tg
                           rg)

(deftest test-generated-functional-api
  (let [g (new-graph (schema rg))
        ^Vertex city (rg/create-City! g :name "Ebernhahn")
        ^Vertex cr1  (rg/create-Plaza! g  :name "Rathausplatz")
        ^Vertex cr2  (rg/create-Plaza! g  :name "Schulplatz")
        hcr1  (rg/create-ContainsCrossroad! g city cr1)
        hcr2  (rg/create-ContainsCrossroad! g city cr2)]
    (is (vertex? city))
    (is (has-type? city 'City!))
    (is (= "Ebernhahn" (value city :name) (rg/name city)))

    (is (vertex? cr1))
    (is (and
         (has-type? cr1 'Plaza)
         (rg/isa-Plaza? cr1)
         (rg/isa-Junction? cr1)
         (not (rg/isa-City? cr1))
         (not (rg/isa-Street? cr1))))

    (is (= "Rathausplatz" (value cr1 :name) (rg/name cr1)))

    (is (edge? hcr1))
    (is (has-type? hcr1 'ContainsCrossroad))

    ;; both should return the city vertex
    (is (= city
           (set-value! city :name "Dernbach")
           (rg/set-name! city "Dernbach")))
    (is (= "Dernbach" (rg/name city)))

    (is (= [cr1 cr2]
           (.adjacences city "crossroads")
           (rg/->crossroads city)))

    (is (= city
           (first (.adjacences cr1 "locality"))
           (first (.adjacences cr2 "locality"))
           (rg/->locality cr1)
           (rg/->locality cr2)))

    (rg/->set-crossroads! city [])
    (is (= []
           (.adjacences city "crossroads")
           (funnyqt.generic/adjs city :crossroads)
           (rg/->crossroads city)))

    (rg/->set-crossroads! city [cr2 cr1])
    (is (= [cr2 cr1]
           (.adjacences city "crossroads")
           (funnyqt.generic/adjs city :crossroads)
           (rg/->crossroads city)))

    (rg/->add-crossroads! city cr1)
    (rg/->addall-crossroads! city [cr1 cr1])
    (is (= [cr2 cr1 cr1 cr1 cr1]
           (.adjacences city "crossroads")
           (funnyqt.generic/adjs city :crossroads)
           (rg/->crossroads city)))))
