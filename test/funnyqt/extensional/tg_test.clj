(ns funnyqt.extensional.tg-test
  (:use funnyqt.tg)
  (:require [funnyqt.emf       :as emf]
            [funnyqt.generic   :as g]
            [funnyqt.query     :as q]
            [funnyqt.extensional :as e]
            [funnyqt.extensional.tg :as etg])
  (:use [funnyqt.tg-test :only [rg]])
  (:use clojure.test))

(e/deftransformation transformation-1-instance-only
  "Creates a graph with 4 vertices and 3 edges."
  [g]
  (etg/create-vertices! g 'localities.City (fn [] [1 2]))
  (etg/set-values! g 'NamedElement :name
                   (fn []
                     {(etg/resolve-element 1) "Köln"
                      (etg/resolve-element 2) "Frankfurt"}))
  (etg/create-vertices! g 'junctions.Crossroad (fn [] ["a" "b"]))
  (etg/create-edges! g 'localities.ContainsCrossroad
                     (fn []
                       [[1 (etg/resolve-alpha 1) (etg/resolve-omega "a")]
                        [2 (etg/resolve-alpha 2) (etg/resolve-omega "b")]]))
  (etg/create-edges! g 'connections.Street (fn []
                                             [[1 (etg/resolve-alpha "a") (etg/resolve-omega "b")]]))
  g)

(deftest test-transformation-1-instance-only
  (let [g (transformation-1-instance-only (new-graph (schema rg)))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

;; The Family2Genealogy transformation from EMF to TG

(defn family
  "Returns the main family of member m."
  [m]
  (or (g/adj m :familyFather) (g/adj m :familyMother)
      (g/adj m :familySon)    (g/adj m :familyDaughter)))

(defn male?
  "Returns true, iff member m is male."
  [m]
  (or (g/adj m :familyFather)
      (g/adj m :familySon)))

(defn parents-of
  "Returns the set of parent members of m."
  [m]
  (q/p-seq m
           [q/p-alt :familySon :familyDaughter]
           [q/p-alt :father :mother]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (g/adj m :familyFather :mother))

(e/deftransformation families2genealogy [m g]
  (etg/create-vertices! g 'Male
                        (fn []
                          (filter male?
                                  (emf/eallcontents m 'Member))))
  (etg/create-vertices! g 'Female
                        (fn []
                          (filter (complement male?)
                                  (emf/eallcontents m 'Member))))
  (etg/set-values! g 'Person :fullName
                   (fn []
                     (for [mem (emf/eallcontents m 'Member)]
                       [(etg/resolve-element mem)
                        (str (emf/eget mem :firstName) " "
                             (emf/eget (family mem) :lastName))])))
  (etg/create-edges! g 'HasSpouse
                     (fn []
                       (for [mem (filter wife (emf/eallcontents m 'Member))
                             :let [w (wife mem)]]
                         [(family mem) (etg/resolve-alpha mem) (etg/resolve-omega w)])))
  (etg/create-edges! g 'HasChild
                     (fn []
                       (for [child (emf/eallcontents m 'Member)
                             parent (parents-of child)]
                         [[child parent] (etg/resolve-alpha parent) (etg/resolve-omega child)])))
  @e/*img*)

(emf/load-ecore-resource "test/input/Families.ecore")

(deftest test-families2genealogy-extensional
  (let [g (new-graph (load-schema "test/input/genealogy-schema.tg"))
        m (emf/load-resource "test/input/example.families")]
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
