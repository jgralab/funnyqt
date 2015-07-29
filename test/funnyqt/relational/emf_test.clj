(ns funnyqt.relational.emf-test
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.test :as t]
            [funnyqt
             [emf :as emf]
             [emf-test :refer :all]
             [generic :as g]
             [query :as q]
             [relational :refer :all]]))

(generate-metamodel-relations "test/input/Families.ecore"
                              test.relational.families.emf families +)

(t/deftest test-elemento
  (t/is (= (emf/eallcontents family-model)
           (run* [q]
             (elemento family-model q)))))

(t/deftest test-elemento-with-type
  (t/is (= (emf/eallcontents family-model 'Member)
           (run* [q]
             (elemento family-model q)
             (typeo family-model q 'Member))
           (run* [q]
             (families/+Member family-model q))))
  (t/is (= (emf/eallcontents family-model '!Member)
           (run* [q]
             (typeo family-model q '!Member)
             (elemento family-model q))
           (run* [q]
             (families/+!Member family-model q)))))

(t/deftest test-relationshipo-error
  (t/is (thrown-with-msg? Exception #".*Cannot use relationshipo.*"
                          (doall
                           (run* [q s t]
                             (relationshipo family-model q s t))))))

(t/deftest test-avalo
  (t/is (= (map (fn [e]
                  [e (emf/eget e :firstName)])
                (emf/eallcontents family-model 'Member))
           (run* [q]
             (with-fresh
               (avalo family-model ?elem :firstName ?val)
               (== q [?elem ?val])))
           (run* [q]
             (with-fresh
               (families/+firstName family-model ?elem ?val)
               (== q [?elem ?val]))))))

(t/deftest test-adjo
  (let [fam-carter (q/the #(= "Carter" (emf/eget % :lastName))
                          (emf/eallcontents family-model 'Family))]
    (t/is (= (g/adjs fam-carter :daughters)
             (run* [q]
               (adjo family-model fam-carter :daughters q))
             (run* [q]
               (families/+->daughters family-model fam-carter q))))))
