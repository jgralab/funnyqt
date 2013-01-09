(ns funnyqt.extensional.tg-test
  (:use funnyqt.tg)
  (:require [funnyqt.emf :as emf])
  (:use funnyqt.query.tg)
  (:require [funnyqt.query.emf :as emfq])
  (:use funnyqt.extensional)
  (:use funnyqt.extensional.tg)
  (:use funnyqt.query)
  (:use [funnyqt.tg-test :only [rg]])
  (:use clojure.test))

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
  (let [g (transformation-1-instance-only (create-graph (schema rg)))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

;; The Family2Genealogy transformation from EMF to TG

(defn family
  "Returns the main family of member m."
  [m]
  (or (adj m :familyFather) (adj m :familyMother)
      (adj m :familySon)    (adj m :familyDaughter)))

(defn male?
  "Returns true, iff member m is male."
  [m]
  (or (adj m :familyFather)
      (adj m :familySon)))

(defn parents-of
  "Returns the set of parent members of m."
  [m]
  (emfq/reachables
   m [p-seq
      [p-alt :familySon :familyDaughter]
      [p-alt :father :mother]]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (adj m :familyFather :mother))

(deftransformation families2genealogy [m g]
  (create-vertices! g 'Male
                    (fn []
                      (filter male?
                              (emf/eallobjects m 'Member))))
  (create-vertices! g 'Female
                    (fn []
                      (filter (complement male?)
                              (emf/eallobjects m 'Member))))
  (set-values! g 'Person.fullName
               (fn []
                 (for [mem (emf/eallobjects m 'Member)]
                   [(resolve-element mem)
                    (str (emf/eget mem :firstName) " "
                         (emf/eget (family mem) :lastName))])))
  (create-edges! g 'HasSpouse
                 (fn []
                   (for [mem (filter wife (emf/eallobjects m 'Member))
                         :let [w (wife mem)]]
                     [(family mem) (resolve-alpha mem) (resolve-omega w)])))
  (create-edges! g 'HasChild
                 (fn []
                   (for [child (emf/eallobjects m 'Member)
                         parent (parents-of child)]
                     [[child parent] (resolve-alpha parent) (resolve-omega child)])))
  @*img*)

(deftest test-families2genealogy-extensional
  (let [g (create-graph (load-schema "test/input/genealogy-schema.tg"))
        _ (emf/load-metamodel "test/input/Families.ecore")
        m (emf/load-model "test/input/example.families")]
    #_(clojure.pprint/pprint (families2genealogy m g))
    (families2genealogy m g)
    (is (= 13 (vcount g 'Person)))
    (is (=  7 (vcount g 'Female)))
    (is (=  6 (vcount g 'Male)))
    (is (= 18 (ecount g 'HasChild)))
    (is (=  3 (ecount g 'HasSpouse)))
    (is (= 21 (ecount g 'HasRelative)))
    #_(show-graph g)
    ))
