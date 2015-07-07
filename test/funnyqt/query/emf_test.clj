(ns funnyqt.query.emf-test
  (:refer-clojure :exclude [parents])
  (:require [clojure.test :refer :all]
            [funnyqt
             [emf :refer :all]
             [emf-test :refer [family-model]]
             [query :as q]
             [utils :as u]]
            [funnyqt.query.emf :refer :all]))

(deftest test-basic-rpes
  (let [fm (q/the (econtents family-model))]
    (are [x y z n] (let [ox (u/oset x)]
                     (and (= ox y z) (== n (count ox))))
         (erefs fm) (--> fm) (q/--> fm) 16
         ;;;;
         (ecrossrefs fm) (---> fm) (q/---> fm) 0
         ;;;;
         (erefs fm :members) (q/p-seq fm :members) (q/--> fm :members) 13
         ;;;;
         (erefs fm :families) (q/p-seq fm :families) (q/<>-- fm :families) 3
         ;;;;
         (erefs fm [:members :families])
         (q/p-alt fm :members :families)
         (q/p-alt fm [--> :members] [q/--> :families])
         16
         ;;;;
         (eallcontents family-model)
         (q/p-* fm -->)
         (q/p-* fm -->)
         17)))

(defn get-member
  [first-name]
  (q/the (filter #(= (eget % :firstName) first-name)
                 (eallcontents family-model 'Member))))

(defn get-family
  [street]
  (q/the (filter #(= (eget % :street) street)
                 (eallcontents family-model 'Family))))

(deftest test--<>
  (let [fm (q/the (econtents family-model))
        diana (get-member "Diana")]
    (is (= #{fm} (--<> diana)))
    (is (= #{}   (--<> fm)))))

(deftest test<---
  (let [fm (q/the (econtents family-model))
        diana (get-member "Diana")
        dennis (get-member "Dennis")]
    (is (= #{(get-family "Smithway 17")} (<--- diana)))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (<--- dennis)))
    ;; Using the opposite ref
    (is (= #{(get-family "Smithway 17")}
           (---> dennis :familyFather)
           (<--- dennis :father)))
    ;; Using search
    (is (= #{(get-family "Smithway 17")}
           (---> dennis :familyFather)
           (<--- dennis :father (econtents fm))))
    (is (= #{(get-family "Smithway 17")}
           (---> dennis :familyFather)
           (<--- dennis :father family-model)))
    (is (= #{(get-family "Smithway 17")}
           (---> dennis :familyFather)
           (<--- dennis :father (eallcontents family-model 'Family))))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (---> dennis [:familyFather :familySon])
           (<--- dennis [:father :sons])))))

(deftest test<--
  (let [fm (q/the (econtents family-model))
        diana (get-member "Diana")
        dennis (get-member "Dennis")]
    (is (= #{fm (get-family "Smithway 17")} (<-- diana)))
    (is (= #{fm} (<-- diana :members)))
    (is (= #{fm
             (get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (<-- dennis)))
    ;; Using the opposite ref
    (is (= #{(get-family "Smithway 17")}
           (--> dennis :familyFather)
           (<-- dennis :father)))
    ;; Using search
    (is (= #{(get-family "Smithway 17")}
           (--> dennis :familyFather)
           (<-- dennis :father (econtents fm))))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (--> dennis [:familyFather :familySon])
           (<-- dennis [:father :sons])))
    (is (= #{fm
             (get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (--> dennis [:model :familyFather :familySon])
           (<-- dennis [:members :father :sons])))))

(defn ^:private parents
  [m]
  (q/p-seq m
           [q/p-alt :familySon :familyDaughter]
           [q/p-alt :father :mother]))

(defn ^:private aunts-or-uncles
  [m r]
  (let [ps (parents m)]
    (q/p-seq ps
             [q/p-alt :familySon :familyDaughter]
             r
             [q/p-restr nil #(not (q/member? % ps))])))

(defn ^:private aunts-or-uncles2
  [m r]
  (let [ps (parents m)]
    (q/p-seq ps
             #(q/p-alt % :familySon :familyDaughter)
             r
             (fn [n] (q/p-restr n nil #(not (q/member? % ps)))))))

(defn ^:private aunts-or-uncles3
  [m r]
  (let [ps (parents m)]
    (q/p-seq ps
             [q/p-alt :familySon :familyDaughter]
             r
             [q/p-restr nil #(not (q/member? % ps))])))

(defn ^:private aunts
  [m]
  (aunts-or-uncles m :daughters))

(defn ^:private uncles
  [m]
  (aunts-or-uncles m :sons))

(deftest test-relationships
  (let [diana (get-member "Diana")
        ps (parents diana)
        us (uncles diana)
        us2 (aunts-or-uncles2 diana :sons)
        us3 (aunts-or-uncles3 diana :sons)
        as (aunts diana)
        as2 (aunts-or-uncles2 diana :daughters)
        as3 (aunts-or-uncles3 diana :daughters)]
    (is (== 2 (count ps)))
    (is (= #{"Debby" "Dennis"}
           (into #{} (map #(eget % :firstName) ps))))
    (is (== 2 (count us) (count us2) (count us3)))
    (is (= #{"Stu" "Sven"}
           (into #{} (map #(eget % :firstName) us))
           (into #{} (map #(eget % :firstName) us2))
           (into #{} (map #(eget % :firstName) us3))))
    (is (== 3 (count as) (count as2) (count as3)))
    (is (= #{"Stella" "Carol" "Conzuela"}
           (into #{} (map #(eget % :firstName) as))
           (into #{} (map #(eget % :firstName) as2))
           (into #{} (map #(eget % :firstName) as3))))))
