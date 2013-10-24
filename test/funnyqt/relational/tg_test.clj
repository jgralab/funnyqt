(ns funnyqt.relational.tg-test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :only [run* run ==]]
        clojure.test
        funnyqt.relational.tg
        funnyqt.relational
        [funnyqt.tg-test :only [rg]])
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.relational.tmp-elem :as tmp]))

(generate-schema-relations "test/input/greqltestgraph.tg"
                           test.relational.routemap.tg routemap)

;;* Basic tests

(deftest test-vertexo
  (is (= (tg/vseq rg)
         (run* [q]
           (vertexo rg q)))))

(deftest test-edgeo
  (is (= (tg/eseq rg)
         (run* [q]
           (with-fresh
             (edgeo rg q _ _)))))
  (is (= (map (fn [e]
                [e (tg/alpha e) (tg/omega e)])
              (tg/eseq rg))
         (run* [q]
           (with-fresh
             (edgeo rg ?e ?a ?o)
             (== q [?e ?a ?o]))))))

(deftest test-typeo
  (is (= (tg/vseq rg 'Junction)
         (run* [q]
           (typeo rg q 'Junction))
         (run* [q]
           (routemap/+Junction rg q))))
  (is (= (tg/eseq rg 'Connection)
         (run* [q]
           (typeo rg q 'Connection))
         (run* [q]
           (with-fresh
             (routemap/+Connection rg q _ _))))))

(deftest test-valueo
  (is (= (map (fn [e]
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

(deftest test-adjo
  (is (= (q/adjs (tg/vertex rg 12) :localities)
         (run* [q]
           (adjo rg (tg/vertex rg 12) :localities q))
         (run* [q]
           (routemap/+->localities rg (tg/vertex rg 12) q))))
  (is (= (q/adjs (tg/vertex rg 12) :capital)
         (run* [q]
           (adjo rg (tg/vertex rg 12) :capital q))
         (run* [q]
           (routemap/+->capital rg (tg/vertex rg 12) q)))))

