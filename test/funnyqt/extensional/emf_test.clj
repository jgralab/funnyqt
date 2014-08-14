(ns funnyqt.extensional.emf-test
  (:use funnyqt.emf)
  (:require [funnyqt.query :as q])
  (:use funnyqt.extensional)
  (:use funnyqt.extensional.emf)
  (:use funnyqt.generic)
  (:use clojure.test))

;; The Family2Genealogy transformation from EMF to EMF

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
  (q/p-seq m
           [q/p-alt :familySon :familyDaughter]
           [q/p-alt :father :mother]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (adj m :familyFather :mother))

(deftransformation families2genealogy [m tm]
  (create-eobjects! tm 'Male
                    (fn []
                      (filter male?
                              (eallcontents m 'Member))))
  (create-eobjects! tm 'Female
                    (fn []
                      (filter (complement male?)
                              (eallcontents m 'Member))))
  (set-values! tm 'Person.fullName
               (fn []
                 (for [mem (eallcontents m 'Member)]
                   [(resolve-eobject mem)
                    (str (eget mem :firstName) " "
                         (eget (family mem) :lastName))])))
  (set-values! tm 'Male.wife
               (fn []
                 (for [mem (filter wife (eallcontents m 'Member))]
                   [(resolve-eobject mem) (resolve-target (wife mem))])))
  (add-values! tm 'Person.parents
               (fn []
                 (for [child (eallcontents m 'Member)
                       :let [parents (parents-of child)]
                       :when parents]
                   [(resolve-eobject child) (resolve-all-targets parents)]))))

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

