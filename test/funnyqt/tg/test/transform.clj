(ns funnyqt.tg.test.transform
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query)
  (:use funnyqt.tg.transform)
  (:use funnyqt.tg.test.core)
  (:use funnyqt.generic)
  (:use clojure.test)
  (:import [de.uni_koblenz.jgralab.schema.impl SchemaImpl]))

(deftransformation transformation-1-instance-only
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

(deftest test-transformation-1-instance-only
  (let [g (transformation-1-instance-only (create-graph (schema (rg))))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

(deftransformation transformation-2
  "Creates 1 VC and one EC."
  [g]
  (create-vertex-class!
   g {:qname 'Person}
   (fn [] [1 2 3 4 5]))

  (create-attribute! g {:qname 'Person.name :domain 'String :default "\"Fritz\""}
                     (fn [] {(resolve-element 1) "Hugo"
                            (resolve-element 2) "Peter"
                            (resolve-element 3) "August"}))
  (create-attribute! g {:qname 'Person.birthday :domain 'String}
                     (fn [] {(resolve-element 3) "1980-11-01"
                            (resolve-element 4) "1970-06-22"
                            (resolve-element 5) "1975-01-01"}))

  (create-vertex-class! g {:qname 'SpecialPerson}
                        (fn [] [:a :b]))
  (create-attribute! g {:qname 'SpecialPerson.xname :domain 'String}
                     (fn [] {(resolve-element :a) "Franz"
                            (resolve-element :b) "Klaus"}))

  (add-sub-classes! g 'Person 'SpecialPerson)

  (create-edge-class!
   g {:qname 'Knows :from 'Person :to 'Person}
   (fn [] (map (fn [[arch a o]]
                [arch (resolve-alpha a) (resolve-omega o)])
              [[1 1 2] [2 2 3] [3 3 4] [4 4 5] [5 5 1]]))))

(deftest test-transformation-2
  (let [g (empty-graph 'test.transformation2.T2Schema 'T2Graph)]
    (transformation-2 g)
    (show-graph g)
    ))
