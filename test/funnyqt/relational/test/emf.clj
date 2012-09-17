(ns funnyqt.relational.test.emf
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.emf
        funnyqt.relational
        [funnyqt.test.emf :only [family-model]])
  (:require [funnyqt.emf :as emf]
            [funnyqt.query :as q]
            [clojure.test :as t]))

(generate-ecore-model-relations "test/input/Families.ecore" families)

(t/deftest test-eobjecto
  (t/is (= (emf/eallobjects family-model)
           (run*-on-model family-model [q]
             (eobjecto q))
           (run*-on-models {families family-model} [q]
             (families/eobjecto q)))))

(t/deftest test-typeo
  (t/is (= (emf/eallobjects family-model 'Member)
           (run*-on-model family-model [q]
             (typeo q 'Member))
           (run*-on-models {families family-model} [q]
             (families/typeo q 'Member)))))

(t/deftest test-valueo
  (t/is (= (map (fn [e]
                  [e (emf/eget e :firstName)])
                (emf/eallobjects family-model 'Member))
           (run*-on-model family-model [q]
             (with-fresh
               (valueo ?elem :firstName ?val)
               (== q [?elem ?val])))
           (run*-on-models {families family-model} [q]
             (with-fresh
               (families/valueo ?elem :firstName ?val)
               (== q [?elem ?val]))))))

(t/deftest test-adjo
  (let [fam-carter (q/the #(= "Carter" (emf/eget % :lastName))
                          (emf/eallobjects family-model 'Family))]
    (t/is (= (q/adjs fam-carter :daughters)
             (run*-on-model family-model [q]
               (adjo fam-carter :daughters q))
             (run*-on-models {families family-model} [q]
               (families/adjo fam-carter :daughters q))))))
