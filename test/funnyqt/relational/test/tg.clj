(ns funnyqt.relational.test.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.tg
        funnyqt.relational
        [funnyqt.test.tg :only [rg]])
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [clojure.test :as test]))

(generate-schema-relations "test/input/greqltestgraph.tg" routemap)

(test/deftest test-vertexo
  (test/is (= (tg/vseq rg)
              (run*-on-model rg [q]
                (vertexo q))
              (run*-on-models {routemap rg} [q]
                (routemap/vertexo q)))))

(test/deftest test-edgeo
  (test/is (= (tg/eseq rg)
              (run*-on-model rg [q]
                (with-fresh
                  (edgeo q _ _)))
              (run*-on-models {routemap rg} [q]
                (with-fresh
                  (routemap/edgeo q _ _)))))
  (test/is (= (map (fn [e]
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

(test/deftest test-typeo
  (test/is (= (tg/vseq rg 'Junction)
              (run*-on-model rg [q]
                (typeo q 'Junction))
              (run*-on-models {routemap rg} [q]
                (routemap/typeo q 'Junction))))
  (test/is (= (tg/eseq rg 'Connection)
              (run*-on-model rg [q]
                (typeo q 'Connection))
              (run*-on-models {routemap rg} [q]
                (routemap/typeo q 'Connection)))))

(test/deftest test-valueo
  (test/is (= (map (fn [e]
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

(test/deftest test-adjo
  (test/is (= (q/adjs (tg/vertex rg 12) :localities)
              (run*-on-model rg [q]
                (adjo (tg/vertex rg 12) :localities q))
              (run*-on-models {routemap rg} [q]
                (routemap/adjo (tg/vertex rg 12) :localities q))))
  (test/is (= (q/adjs (tg/vertex rg 12) :capital)
              (run*-on-model rg [q]
                (adjo (tg/vertex rg 12) :capital q))
              (run*-on-models {routemap rg} [q]
                (routemap/adjo (tg/vertex rg 12) :capital q)))))

