(ns funnyqt.extensional.emf-test
  (:require [funnyqt
             [emf :refer :all]
             [extensional :as e]
             [generic :as g]
             [query :as q]]
            [funnyqt.extensional.emf :as eemf])
  (:use clojure.test))

;; The Family2Genealogy transformation from EMF to EMF

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

(defn families2genealogy [m tm]
  (e/with-trace-mappings
    (eemf/create-eobjects! tm 'Male
                           (fn []
                             (filter male? (eallcontents m 'Member))))
    (eemf/create-eobjects! tm 'Female
                           (fn []
                             (filter (complement male?)
                                     (eallcontents m 'Member))))
    (eemf/set-values! tm 'Person :fullName
                      (fn []
                        (for [mem (eallcontents m 'Member)]
                          [(e/element-image mem)
                           (str (eget mem :firstName) " "
                                (eget (family mem) :lastName))])))
    (eemf/set-erefs! tm 'Male :wife
                     (fn []
                       (for [mem (filter wife (eallcontents m 'Member))]
                         [(e/element-image mem) (e/target-image (wife mem))])))
    (eemf/add-erefs! tm 'Person :parents
                     (fn []
                       (for [child (eallcontents m 'Member)
                             :let [parents (parents-of child)]
                             :when (seq parents)]
                         [(e/element-image child) (e/target-images parents)])))))

(load-ecore-resource "test/input/Genealogy.ecore")
(load-ecore-resource "test/input/Families.ecore")

(deftest test-families2genealogy-extensional
  (let [tm (new-resource)
        m (load-resource "test/input/example.families")]
    #_(clojure.pprint/pprint (families2genealogy m tm))
    (families2genealogy m tm)
    (is (= 13 (count (eallcontents tm 'Person))))
    (is (=  7 (count (eallcontents tm 'Female))))
    (is (=  6 (count (eallcontents tm 'Male))))
    (is (= 18 (count (epairs tm :parents :children))))
    (is (=  3 (count (epairs tm :husband :wife))))
    #_(print-model tm ".gtk")))

