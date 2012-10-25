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
           (run* [q]
             (vertexo rg q)))))

(t/deftest test-edgeo
  (t/is (= (tg/eseq rg)
           (run* [q]
             (with-fresh
               (edgeo rg q _ _)))))
  (t/is (= (map (fn [e]
                  [e (tg/alpha e) (tg/omega e)])
                (tg/eseq rg))
           (run* [q]
             (with-fresh
               (edgeo rg ?e ?a ?o)
               (== q [?e ?a ?o]))))))

(t/deftest test-typeo
  (t/is (= (tg/vseq rg 'Junction)
           (run* [q]
             (typeo rg q 'Junction))
           (run* [q]
             (routemap/+Junction rg q))))
  (t/is (= (tg/eseq rg 'Connection)
           (run* [q]
             (typeo rg q 'Connection))
           (run* [q]
             (with-fresh
               (routemap/+Connection rg q _ _))))))

(t/deftest test-valueo
  (t/is (= (map (fn [e]
                  [e (tg/value e :name)])
                (concat (tg/vseq rg '[NamedElement Plaza])
                        (tg/eseq rg 'Street)))
           (run* [q]
             (with-fresh
               (valueo rg ?elem :name ?val)
               (== q [?elem ?val])))
           (run* [q]
             (with-fresh
               (routemap/+name rg ?elem ?val)
               (== q [?elem ?val]))))))

(t/deftest test-adjo
  (t/is (= (q/adjs (tg/vertex rg 12) :localities)
           (run* [q]
             (adjo rg (tg/vertex rg 12) :localities q))
           (run* [q]
             (routemap/+->localities rg (tg/vertex rg 12) q))))
  (t/is (= (q/adjs (tg/vertex rg 12) :capital)
           (run* [q]
             (adjo rg (tg/vertex rg 12) :capital q))
           (run* [q]
             (routemap/+->capital rg (tg/vertex rg 12) q)))))
