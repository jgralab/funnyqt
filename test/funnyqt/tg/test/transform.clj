(ns funnyqt.tg.test.transform
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query)
  (:use funnyqt.tg.transform)
  (:use funnyqt.tg.test.core)
  (:use funnyqt.generic)
  (:use clojure.test)
  (:import [de.uni_koblenz.jgralab.schema.impl SchemaImpl]))

(deftransformation transformation-1
  "Creates a graph with 4 vertices and 3 edges."
  [g]
  (create-vertices! g 'localities.City (fn [] [1 2]))
  (set-values! g 'NamedElement.name
               (fn []
                 {(resolve-element 1) "Köln"
                  (resolve-element 2) "Frankfurt"}))
  (create-vertices! g 'junctions.Crossroad (fn [] ["a" "b"]))
  (create-edges! g 'localities.ContainsCrossroad
                 (fn []
                   [[1 (resolve-alpha 1) (resolve-omega "a")]
                    [2 (resolve-alpha 2) (resolve-omega "b")]]))
  (create-edges! g 'connections.Street (fn []
                                         [[1 (resolve-alpha "a") (resolve-omega "b")]]))
  g)

(deftest test-trans-1
  (let [g (transformation-1 (create-graph (schema (rg))))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

