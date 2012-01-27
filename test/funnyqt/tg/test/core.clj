(ns funnyqt.tg.test.core
  (:use [ordered.set])
  (:use [funnyqt.tg.core])
  (:use [clojure.test])
  (:import (de.uni_koblenz.jgralab Graph Vertex Edge GraphIO)))

;;* The test graphs

(def rg (memoize #(load-graph "test/greqltestgraph.tg")))
(def jg (memoize #(load-graph "test/medium-model.tg")))
(def gbjg (memoize #(load-graph "test/apache_ant_modified.tg.gz")))
(def mg (memoize #(load-graph "test/foo.tg")))

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
        e1 (create-edge! 'localities.ContainsCrossroad v1 v2)
        e2 (create-edge! 'localities.ContainsCrossroad v3 v4)
        e3 (create-edge! 'connections.Street v2 v4)]
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

