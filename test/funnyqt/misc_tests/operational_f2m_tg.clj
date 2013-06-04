(ns funnyqt.misc-tests.operational-f2m-tg
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

(deftransformation families2genealogy-tg [in out]
  (letmapping [(family2address
                [f]
                (let [v (create-vertex! out 'Address)]
                  (set-value! v :street (value f :street))
                  (set-value! v :town (value f :town))
                  v))

               (member2person
                [m]
                (let [p (create-vertex! out (if (male? m)
                                              'Male 'Female))]
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
                  p))]
    (mapv family2address (vseq in 'Family))
    (mapv member2person (vseq in 'Member))
    out))

;; Run it!

(deftest test-transformation
  (let [in (load-graph "test/input/familygraph.tg")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (create-graph out-schema)
        gen (time (families2genealogy-tg in ng))]
    (is gen)
    (is (== 13 (vcount gen 'Person)))
    (is (==  7 (vcount gen 'Female)))
    (is (==  6 (vcount gen 'Male)))
    (is (==  3 (vcount gen 'Address)))
    (is (== 18 (ecount gen 'HasChild)))
    (is (==  3 (ecount gen 'HasSpouse)))))

