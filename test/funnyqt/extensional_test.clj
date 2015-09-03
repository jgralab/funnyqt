(ns funnyqt.extensional-test
  (:refer-clojure :exclude [parents])
  (:require [clojure.test :refer :all]
            [funnyqt
             [tg-test :refer [rg]]
             [emf :as emf]
             [extensional :refer :all]
             [generic :refer [adj aval elements enum-constant]]
             [query :as q]
             [tg :as tg]]
            [funnyqt.visualization :as viz]))

;;* Helpers

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

;;* Transformation with relationships

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

;;* Transformation without relationships

(defn families2genealogy-no-relationships [m g]
  (with-trace-mappings
    (create-elements! g 'Male
                      (fn []
                        (filter male? (elements m 'Member))))
    (create-elements! g 'Female
                      (fn []
                        (filter (complement male?)
                                (elements m 'Member))))
    (set-avals! g 'Person :fullName
                (fn []
                  (for [mem (elements m 'Member)]
                    [(element-image mem)
                     (str (aval mem :firstName) " "
                          (aval (family mem) :lastName))])))
    (set-adjs! g 'Male :wife
               (fn []
                 (for [mem (filter wife (elements m 'Member))]
                   [(element-image mem) (target-image (wife mem))])))
    (add-adjs! g 'Person :parents
               (fn []
                 (for [child (elements m 'Member)
                       :let [parents (parents child)]
                       :when (seq parents)]
                   [(element-image child) (target-images parents)])))
    (create-elements! g 'Address
                      (fn []
                        (for [f (elements m 'Family)]
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
    (set-adjs! g 'Person :address
               (fn []
                 (fn [p]
                   (let [f (family (element-archetype p))]
                     (target-image [(aval f :street) (aval f :town)])))))))

;;* The tests

(deftest test-families2genealogy-extensional-tg
  (let [g (tg/new-graph (tg/load-schema "test/input/genealogy-schema.tg"))
        m (emf/load-resource "test/input/example.families")]
    (print "families2genealogy (EMF -> TG): ")
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

(emf/load-ecore-resource "test/input/Genealogy.ecore")
(emf/load-ecore-resource "test/input/Families.ecore")

(deftest test-families2genealogy-extensional-emf
  (let [g (emf/new-resource)
        m (emf/load-resource "test/input/example.families")]
    (print "families2genealogy-no-relationships (EMF -> EMF): ")
    (time (families2genealogy-no-relationships m g))
    (is (= 13 (count (emf/eallcontents g 'Person))))
    (is (=  7 (count (emf/eallcontents g 'Female))))
    (is (=  6 (count (emf/eallcontents g 'Male))))
    (is (=  3 (count (emf/eallcontents g 'Address))))
    (is (= 13 (count (emf/epairs g :person  :address))))
    (is (= 18 (count (emf/epairs g :parents :children))))
    (is (=  3 (count (emf/epairs g :husband :wife))))
    #_(viz/print-model g :gtk)))

(deftest test-transformation-1
  (let [g (tg/new-graph (tg/schema rg))]
    (with-trace-mappings
      (create-elements! g 'localities.City (fn [] [1 2]))
      (set-avals! g 'NamedElement :name
                  (fn []
                    {(element-image 1) "Köln"
                     (element-image 2) "Frankfurt"}))
      (create-elements! g 'junctions.Crossroad (fn [] ["a" "b"]))
      (create-relationships! g 'localities.ContainsCrossroad
                             (fn []
                               [[1 (source-image 1) (target-image "a")]
                                [2 (source-image 2) (target-image "b")]]))
      (create-relationships! g 'connections.Street
                             (fn []
                               [[1 (source-image "a") (target-image "b")]])))
    (is (= 4 (tg/vcount g)))
    (is (= 3 (tg/ecount g)))
    (is (= "Köln"      (aval (tg/vertex g 1) :name)))
    (is (= "Frankfurt" (aval (tg/vertex g 2) :name)))))

(deftest test-transformation-2
  (is (thrown-with-msg?
       Exception #"Bijectivity violation:"
       (let [g (tg/new-graph (tg/schema rg))]
         (with-trace-mappings
           (create-elements! g 'localities.City (fn [] [1]))
           ;; This should throw because the archetype 1 is already used.
           (create-elements! g 'localities.City (fn [] [1])))))))

(deftest test-transformation-3
  (is (thrown-with-msg?
       Exception #"Bijectivity violation:"
       (let [g (tg/new-graph (tg/schema rg))]
         (with-trace-mappings
           (create-elements! g 'City   (fn [] [1 2 3]))
           ;; City and County are both NamedElements, so their archetypes must be
           ;; disjoint.  Thus, the following must fail!
           (create-elements! g 'County (fn [] [1 2 3])))))))

