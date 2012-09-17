(ns funnyqt.relational.test.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.tg
        funnyqt.relational
        [funnyqt.test.tg :only [rg]])
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [clojure.test :as t]))

(generate-schema-relations "test/input/greqltestgraph.tg" routemap)

(t/deftest test-vertexo
  (t/is (= (tg/vseq rg)
           (run*-on-model rg [q]
             (vertexo q))
           (run*-on-models {routemap rg} [q]
             (routemap/vertexo q)))))

(t/deftest test-edgeo
  (t/is (= (tg/eseq rg)
           (run*-on-model rg [q]
             (with-fresh
               (edgeo q _ _)))
           (run*-on-models {routemap rg} [q]
             (with-fresh
               (routemap/edgeo q _ _)))))
  (t/is (= (map (fn [e]
                  [e (tg/alpha e) (tg/omega e)])
                (tg/eseq rg))
           (run*-on-model rg [q]
             (with-fresh
               (edgeo ?e ?a ?o)
               (== q [?e ?a ?o])))
           (run*-on-models {routemap rg} [q]
             (with-fresh
               (routemap/edgeo ?e ?a ?o)
               (== q [?e ?a ?o]))))))

(t/deftest test-typeo
  (t/is (= (tg/vseq rg 'Junction)
           (run*-on-model rg [q]
             (typeo q 'Junction))
           (run*-on-models {routemap rg} [q]
             (routemap/typeo q 'Junction))))
  (t/is (= (tg/eseq rg 'Connection)
           (run*-on-model rg [q]
             (typeo q 'Connection))
           (run*-on-models {routemap rg} [q]
             (routemap/typeo q 'Connection)))))

(t/deftest test-valueo
  (t/is (= (map (fn [e]
                  [e (tg/value e :name)])
                (concat (tg/vseq rg '[NamedElement Plaza])
                        (tg/eseq rg 'Street)))
           (run*-on-model rg [q]
             (with-fresh
               (valueo ?elem :name ?val)
               (== q [?elem ?val])))
           (run*-on-models {routemap rg} [q]
             (with-fresh
               (routemap/valueo ?elem :name ?val)
               (== q [?elem ?val]))))))

(t/deftest test-adjo
  (t/is (= (q/adjs (tg/vertex rg 12) :localities)
           (run*-on-model rg [q]
             (adjo (tg/vertex rg 12) :localities q))
           (run*-on-models {routemap rg} [q]
             (routemap/adjo (tg/vertex rg 12) :localities q))))
  (t/is (= (q/adjs (tg/vertex rg 12) :capital)
           (run*-on-model rg [q]
             (adjo (tg/vertex rg 12) :capital q))
           (run*-on-models {routemap rg} [q]
             (routemap/adjo (tg/vertex rg 12) :capital q)))))
