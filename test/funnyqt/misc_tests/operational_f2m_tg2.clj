(ns funnyqt.misc-tests.operational-f2m-tg2
  (:use funnyqt.operational)
  (:use funnyqt.protocols)
  (:use funnyqt.query)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:use funnyqt.tg)
  (:use funnyqt.query.tg))

(defn male? [m]
  (or (adj m :familyFather)
      (adj m :familySon)))

(defn family [m]
  (or (adj m :familyFather)
      (adj m :familyMother)
      (adj m :familySon)
      (adj m :familyDaughter)))

(defn wife [mm]
  (adj mm :familyFather :mother))

(defn children [m]
  (reachables
   m [p-seq [p-alt :familyFather
                   :familyMother]
      [p-alt :sons :daughters]]))

(def ^:dynamic *target-graph*)

(defmapping family2address
  "Creates an Address for the given Family `f`."
  [f]
  (let [v (create-vertex! *target-graph* 'Address)]
    (set-value! v :street (value f :street))
    (set-value! v :town (value f :town))
    v))

(defmapping member2person
  [m]
  (let [p (create-vertex! *target-graph* (if (male? m) 'Male 'Female))]
    (set-value! p :fullName
                (str (value m :firstName)
                     " "
                     (value (family m) :lastName)))
    (deferred
      (add-adj! p :address (resolve-in family2address (family m)))
      (set-adjs! p :children
                 (resolve-all-in member2person (children m)))
      (when-let [w (wife m)]
        (add-adj! p :wife (resolve-in member2person w))))
    p))

(deftransformation families2genealogy-tg [in]
  (binding [*target-graph* (create-graph
                            (load-schema
                             "test/input/genealogy-schema.tg"))]
    (mapv #(type-case %
             'Member (member2person %)
             'Family (family2address %))
          (vseq in '[Member Family]))
    *target-graph*))

;; Run it!

(deftest test-transformation
  (let [in (load-graph "test/input/familygraph.tg")
        gen (time (families2genealogy-tg in))]
    (is gen)
    (is (== 13 (vcount gen 'Person)))
    (is (==  7 (vcount gen 'Female)))
    (is (==  6 (vcount gen 'Male)))
    (is (==  3 (vcount gen 'Address)))
    (is (== 18 (ecount gen 'HasChild)))
    (is (==  3 (ecount gen 'HasSpouse)))))

