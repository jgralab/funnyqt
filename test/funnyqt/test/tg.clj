(ns funnyqt.test.tg
  (:use [ordered.set])
  (:use [funnyqt.tg])
  (:use funnyqt.protocols)
  (:use [clojure.test])
  (:import (de.uni_koblenz.jgralab Graph Vertex Edge GraphIO)))

;;* The test graphs

(def rg (memoize #(load-graph "test/input/greqltestgraph.tg")))
(def jg (memoize #(load-graph "test/input/medium-model.tg")))

;;* Tests

(deftest test-value
  (let [winningen (vertex (rg) 4)]
    ;; Normal attribute access
    (is (= "Winningen" (value winningen :name)))
    (is (= 2432 (value winningen :inhabitants)))
    (let [fd (value winningen :foundingDate)]
      ;; Access to record components
      (is (= 1 (value fd :day)))
      (is (= "JAN" (str (value fd :month))))
      (is (= 1016 (value fd :year))))))


(deftest test-schema-imports-1
  (let [l  'localities.Locality
        hc 'localities.HasCapital
        ct 'localities.CountyTags]
    (is (= (attributed-element-class (rg) 'localities.Locality)
	   (attributed-element-class (rg) l)))
    (is (= (attributed-element-class (rg) 'localities.HasCapital)
	   (attributed-element-class (rg) hc)))
    (is (= (domain (rg) 'localities.CountyTags)
           (domain (rg) ct)))))

(deftest test-create-graph-vertex-edge-1
  (let [g ^Graph (create-graph (schema (rg)) "Test graph 1")
        v1 (create-vertex! g 'localities.City)
        v2 (create-vertex! g 'junctions.Crossroad)
        v3 (create-vertex! g 'localities.City)
        v4 (create-vertex! g 'junctions.Crossroad)
        e1 (create-edge! g 'localities.ContainsCrossroad v1 v2)
        e2 (create-edge! g 'localities.ContainsCrossroad v3 v4)
        e3 (create-edge! g 'connections.Street v2 v4)]
    (is (== 4 (.getVCount g)) "Wrong vertex count")
    (is (== 3 (.getECount g)) "Wrong edge count")))


(deftest test-print-read-stuff
  (let [x [1
           (vertex (rg) 17)
           "Foo"
           #{(edge (rg) 18) :kw}
           {:a "A" :b "B"}
           (rg)
           #{1 2 (vertex (rg) 1)}]]
    (is (= x (tg-read-str (tg-pr-str x) (rg))))))

(deftest test-is-instance?
  (let [g     (rg)
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

