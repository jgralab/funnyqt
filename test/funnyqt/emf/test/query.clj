(ns funnyqt.emf.test.query
  (:refer-clojure :exclude [parents])
  (:use funnyqt.utils)
  (:use funnyqt.generic)
  (:use ordered.set)
  (:use ordered.map)
  (:use funnyqt.emf.core)
  (:use funnyqt.emf.query)
  (:use [funnyqt.emf.test.core :only [family-model]])
  (:use clojure.test))

(deftest test-basic
  (let [fm family-model]
    (are [x y n] (let [ox (into-oset x)]
                   (and (= ox y)
                        (== n (count ox))))
         (erefs fm) (ereachables fm -->>) 16
         (ecrossrefs fm) (ereachables fm -->) 0
         (erefs fm :members) (ereachables fm :members) 13
         (erefs fm :families) (ereachables fm :families) 3
         (erefs fm [:members :families]) (ereachables fm [ep-alt :members :families]) 16
         (econtents fm) (ereachables fm [ep-* -->>]) 17)))

(defn- parents
  [m]
  (ereachables m [ep-seq
                  [ep-alt :familySon :familyDaughter]
                  [ep-alt :father :mother]]))

(defn- aunts-or-uncles
  [m r]
  (let [ps (parents m)]
    (ereachables ps [ep-seq
                     [ep-alt :familySon :familyDaughter]
                     r
                     [ep-restr nil #(not (member? % ps))]])))

(defn- aunts
  [m]
  (aunts-or-uncles m :daughters))

(defn- uncles
  [m]
  (aunts-or-uncles m :sons))

(deftest test-relationships
  (let [diana (the (filter #(= (eget % :firstName) "Diana")
                           (econtents family-model 'Member)))
        ps (parents diana)
        us (uncles diana)
        as (aunts diana)]
    (is (== 2 (count ps)))
    (is (= #{"Debby" "Dennis"}
           (into #{} (map #(eget % :firstName) ps))))
    (is (== 2 (count us)))
    (is (= #{"Stu" "Sven"}
           (into #{} (map #(eget % :firstName) us))))
    (is (== 3 (count as)))
    (is (= #{"Stella" "Carol" "Conzuela"}
           (into #{} (map #(eget % :firstName) as))))))

