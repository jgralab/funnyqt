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
                    (let [m (element-archetype p)]
                      (enum-constant p (if (< (aval m :age) 18)
                                         'AgeGroup.CHILD
                                         'AgeGroup.ADULT))))))
    (create-relationships! g 'HasChild
                           (fn []
                             (for [m (elements fs 'Member)
                                   p (parents m)]
                               [[m p] (source-image m) (target-image p)])))
    (set-adjs! g 'Male :wife
               (fn []
                 (fn [male]
                   (let [m (element-archetype male)]
                     (target-image (wife m))))))
    (create-elements! g 'Address
                      (fn []
                        (for [f (elements fs 'Family)]
                          [(aval f :street) (aval f :town)])))
    (let [address-arch-map (archetype-map g 'Address)]
      (set-avals! g 'Address :street
                  (fn []
                    (fn [addr]
                      (first (address-arch-map addr)))))
      (set-avals! g 'Address :town
                  (fn []
                    (fn [addr]
                      (second (address-arch-map addr))))))
    (create-relationships! g 'LivesAt
                           (fn []
                             (for [m (elements fs 'Member)]
                               [m (source-image m)
                                (let [f (family m)]
                                  (target-image [(aval f :street) (aval f :town)]))])))))

(deftest test-families2genealogy-extensional
  (let [g (tg/new-graph (tg/load-schema "test/input/genealogy-schema.tg"))
        m (emf/load-resource "test/input/example.families")]
    (print "families2genealogy-extensional (EMF -> TG): ")
    (time (families2genealogy m g))
    ;;(./print-model g :gtk)
    (is (=  3 (tg/vcount g 'Address)))
    (is (= 13 (tg/vcount g 'Person)))
    (is (=  7 (tg/vcount g 'Female)))
    (is (=  6 (tg/vcount g 'Male)))
    (is (= 13 (tg/ecount g 'LivesAt)))
    (is (= 18 (tg/ecount g 'HasChild)))
    (is (=  3 (tg/ecount g 'HasSpouse)))
    (is (= 21 (tg/ecount g 'HasRelative)))))
