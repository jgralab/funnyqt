(ns funnyqt.extensional-test
  (:refer-clojure :exclude [parents])
  (:require [clojure.test :refer :all]
            [funnyqt
             [emf :as emf]
             [extensional :refer :all]
             [generic :refer [adj aval elements enum-constant]]
             [query :as q]
             [tg :as tg]]))

(emf/load-ecore-resource "test/input/Families.ecore")

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

(defn parents
  "Returns the set of parent members of m."
  [m]
  (q/p-seq m
           [q/p-alt :familySon :familyDaughter]
           [q/p-alt :father :mother]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (adj m :familyFather :mother))


(defn families2genealogy
  "Transforms the family model `fs` to the genealogy `g`."
  [fs g]
  (with-trace-mappings
    (create-elements! g 'Male
                      (fn []
                        (filter male? (elements fs 'Member))))
    (create-elements! g 'Female
                      (fn []
                        (remove male? (elements fs 'Member))))
    (set-avals! g 'Person :fullName
                (fn []
                  (for [[m p] (image-map g 'Person)]
                    [p (str (aval m :firstName) " "
                            (aval (family m) :lastName))])))
    (set-avals! g 'Person "ageGroup"
                (fn []
                  (fn [p]
                    (let [mem (element-archetype p)]
                      (enum-constant p (if (< (emf/eget mem :age) 18)
                                         'AgeGroup.CHILD
                                         'AgeGroup.ADULT))))))
    (create-relationships! g 'HasChild
                           (fn []
                             (for [mem (elements fs 'Member)
                                   p   (parents mem)]
                               [[mem p] (source-image mem) (target-image p)])))
    (set-adjs! g 'Male :wife
               (fn []
                 (fn [male]
                   (let [mem (element-archetype male)]
                     (target-image (wife mem))))))))

(deftest test-families2genealogy-2-extensional
  (let [g (tg/new-graph (tg/load-schema "test/input/genealogy-schema.tg"))
        m (emf/load-resource "test/input/example.families")]
    (families2genealogy m g)
    ;; (./print-model g :gtk)
    (is (= 13 (tg/vcount g 'Person)))
    (is (=  7 (tg/vcount g 'Female)))
    (is (=  6 (tg/vcount g 'Male)))
    (is (= 18 (tg/ecount g 'HasChild)))
    (is (=  3 (tg/ecount g 'HasSpouse)))
    (is (= 21 (tg/ecount g 'HasRelative)))))
