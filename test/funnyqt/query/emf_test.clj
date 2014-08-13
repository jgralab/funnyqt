(ns funnyqt.query.emf-test
  (:refer-clojure :exclude [parents])
  (:use funnyqt.emf
        funnyqt.query.emf)
  (:use clojure.test)
  (:require [funnyqt.utils    :as u]
            [funnyqt.generic  :as g]
            [funnyqt.query    :as q]
            [funnyqt.emf-test :refer [family-model]])
  (:import
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.common.util URI EList]
   [org.eclipse.emf.ecore EPackage EObject EModelElement]))

(deftest test-basic-rpes
  (let [fm (q/the (econtents family-model))]
    (are [x y z n] (let [ox (u/oset x)]
                     (and (= ox y z) (== n (count ox))))
         (erefs fm) (q/reachables fm -->) (q/reachables fm q/-->) 16
         ;;;;
         (ecrossrefs fm) (q/reachables fm --->) (q/reachables fm q/--->) 0
         ;;;;
         (erefs fm :members) (q/reachables fm :members) (q/reachables fm [q/--> :members]) 13
         ;;;;
         (erefs fm :families) (q/reachables fm :families) (q/reachables fm [q/<>-- :families]) 3
         ;;;;
         (erefs fm [:members :families])
         (q/reachables fm [q/p-alt :members :families])
         (q/reachables fm [q/p-alt [--> :members] [q/--> :families]])
         16
         ;;;;
         (eallcontents family-model)
         (q/reachables fm [q/p-* -->])
         (q/reachables fm [q/p-* -->])
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
    (is (= #{fm} (q/reachables diana --<>)))
    (is (= #{}   (q/reachables fm --<>)))))

(deftest test<---
  (let [fm (q/the (econtents family-model))
        diana (get-member "Diana")
        dennis (get-member "Dennis")]
    (is (= #{(get-family "Smithway 17")} (q/reachables diana <---)))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (q/reachables dennis <---)))
    ;; Using the opposite ref
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [---> :familyFather])
           (q/reachables dennis [<--- :father])))
    ;; Using search
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [---> :familyFather])
           (q/reachables dennis [<--- :father (econtents fm)])))
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [---> :familyFather])
           (q/reachables dennis [<--- :father family-model])))
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [---> :familyFather])
           (q/reachables dennis [<--- :father (eallcontents family-model 'Family)])))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (q/reachables dennis [---> [:familyFather :familySon]])
           (q/reachables dennis [<--- [:father :sons]])))))

(deftest test<--
  (let [fm (q/the (econtents family-model))
        diana (get-member "Diana")
        dennis (get-member "Dennis")]
    (is (= #{fm (get-family "Smithway 17")} (q/reachables diana <--)))
    (is (= #{fm} (q/reachables diana [<-- :members])))
    (is (= #{fm
             (get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (q/reachables dennis <--)))
    ;; Using the opposite ref
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [--> :familyFather])
           (q/reachables dennis [<-- :father])))
    ;; Using search
    (is (= #{(get-family "Smithway 17")}
           (q/reachables dennis [--> :familyFather])
           (q/reachables dennis [<-- :father (econtents fm)])))
    (is (= #{(get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (q/reachables dennis [--> [:familyFather :familySon]])
           (q/reachables dennis [<-- [:father :sons]])))
    (is (= #{fm
             (get-family "Smithway 17")
             (get-family "Smith Avenue 4")}
           (q/reachables dennis [--> [:model :familyFather :familySon]])
           (q/reachables dennis [<-- [:members :father :sons]])))))

(defn ^:private parents
  [m]
  (q/reachables m [q/p-seq
                   [q/p-alt :familySon :familyDaughter]
                   [q/p-alt :father :mother]]))

(defn ^:private aunts-or-uncles
  [m r]
  (let [ps (parents m)]
    (q/reachables ps [q/p-seq
                      [q/p-alt :familySon :familyDaughter]
                      r
                      [q/p-restr nil #(not (q/member? % ps))]])))

(defn ^:private aunts-or-uncles2
  [m r]
  (let [ps (parents m)]
    (q/p-seq ps
             #(q/p-alt % :familySon :familyDaughter)
             #(q/reachables % r)
             (fn [n] (q/p-restr n nil #(not (q/member? % ps)))))))

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
        as (aunts diana)
        as2 (aunts-or-uncles2 diana :daughters)]
    (is (== 2 (count ps)))
    (is (= #{"Debby" "Dennis"}
           (into #{} (map #(eget % :firstName) ps))))
    (is (== 2 (count us) (count us2)))
    (is (= #{"Stu" "Sven"}
           (into #{} (map #(eget % :firstName) us))
           (into #{} (map #(eget % :firstName) us2))))
    (is (== 3 (count as)))
    (is (= #{"Stella" "Carol" "Conzuela"}
           (into #{} (map #(eget % :firstName) as))
           (into #{} (map #(eget % :firstName) as2))))))
