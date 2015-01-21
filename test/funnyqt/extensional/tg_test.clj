(ns funnyqt.extensional.tg-test
  (:use funnyqt.tg)
  (:require [funnyqt.emf       :as emf]
            [funnyqt.generic   :as g]
            [funnyqt.query     :as q]
            [funnyqt.extensional :as e]
            [funnyqt.extensional.tg :as etg])
  (:use [funnyqt.tg-test :only [rg]])
  (:use clojure.test))

;;# Misc tests

(e/deftransformation transformation-1
  "Creates a graph with 4 vertices and 3 edges."
  [g]
  (etg/create-vertices! g 'localities.City (fn [] [1 2]))
  (etg/set-values! g 'NamedElement :name
                   (fn []
                     {(e/resolve-element 1) "Köln"
                      (e/resolve-element 2) "Frankfurt"}))
  (etg/create-vertices! g 'junctions.Crossroad (fn [] ["a" "b"]))
  (etg/create-edges! g 'localities.ContainsCrossroad
                     (fn []
                       [[1 (e/resolve-source 1) (e/resolve-target "a")]
                        [2 (e/resolve-source 2) (e/resolve-target "b")]]))
  (etg/create-edges! g 'connections.Street
                     (fn []
                       [[1 (e/resolve-source "a") (e/resolve-target "b")]]))
  g)

(deftest test-transformation-1
  (let [g (transformation-1 (new-graph (schema rg)))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

(e/deftransformation transformation-2 [g]
  (etg/create-vertices! g 'localities.City (fn [] [1]))
  ;; This should throw because the archetype 1 is already used.
  (etg/create-vertices! g 'localities.City (fn [] [1])))

(deftest test-transformation-2
  (is (thrown-with-msg?
       Exception #"Bijectivity violation:"
       (transformation-2 (new-graph (schema rg))))))

;;# The Family2Genealogy transformation from EMF to TG


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
                       [(e/resolve-element mem)
                        (str (emf/eget mem :firstName) " "
                             (emf/eget (family mem) :lastName))])))
  (etg/create-edges! g 'HasSpouse
                     (fn []
                       (for [mem (filter wife (emf/eallcontents m 'Member))
                             :let [w (wife mem)]]
                         [(family mem) (e/resolve-source mem) (e/resolve-target w)])))
  (etg/create-edges! g 'HasChild
                     (fn []
                       (for [child (emf/eallcontents m 'Member)
                             parent (parents-of child)]
                         [[child parent] (e/resolve-source parent) (e/resolve-target child)])))
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
