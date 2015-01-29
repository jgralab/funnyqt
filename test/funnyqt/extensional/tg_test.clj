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

(deftest test-transformation-1
  (let [g (new-graph (schema rg))]
    (e/with-trace-mappings
      (etg/create-vertices! g 'localities.City (fn [] [1 2]))
      (etg/set-values! g 'NamedElement :name
                       (fn []
                         {(e/element-image 1) "Köln"
                          (e/element-image 2) "Frankfurt"}))
      (etg/create-vertices! g 'junctions.Crossroad (fn [] ["a" "b"]))
      (etg/create-edges! g 'localities.ContainsCrossroad
                         (fn []
                           [[1 (e/source-image 1) (e/target-image "a")]
                            [2 (e/source-image 2) (e/target-image "b")]]))
      (etg/create-edges! g 'connections.Street
                         (fn []
                           [[1 (e/source-image "a") (e/target-image "b")]])))
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

(deftest test-transformation-2
  (is (thrown-with-msg?
       Exception #"Bijectivity violation:"
       (let [g (new-graph (schema rg))]
         (e/with-trace-mappings
           (etg/create-vertices! g 'localities.City (fn [] [1]))
           ;; This should throw because the archetype 1 is already used.
           (etg/create-vertices! g 'localities.City (fn [] [1])))))))

(deftest test-transformation-3
  (is (thrown-with-msg?
       Exception #"Bijectivity violation:"
       (let [g (new-graph (schema rg))]
         (e/with-trace-mappings
           (etg/create-vertices! g 'City   (fn [] [1 2 3]))
           ;; City and County are both NamedElements, so their archetypes must be
           ;; disjoint.  Thus, the following must fail!
           (etg/create-vertices! g 'County (fn [] [1 2 3])))))))

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

(defn families2genealogy [m g]
  (e/with-trace-mappings
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
                         [(e/element-image mem)
                          (str (emf/eget mem :firstName) " "
                               (emf/eget (family mem) :lastName))])))
    (etg/create-edges! g 'HasSpouse
                       (fn []
                         (for [mem (filter wife (emf/eallcontents m 'Member))
                               :let [w (wife mem)]]
                           [(family mem) (e/source-image mem) (e/target-image w)])))
    (etg/create-edges! g 'HasChild
                       (fn []
                         (for [child (emf/eallcontents m 'Member)
                               parent (parents-of child)]
                           [[child parent] (e/source-image parent) (e/target-image child)])))
    @e/*img*))

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
