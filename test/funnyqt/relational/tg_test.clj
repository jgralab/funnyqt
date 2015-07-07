(ns funnyqt.relational.tg-test
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :only [run* run ==]]
        clojure.test
        funnyqt.relational
        [funnyqt.tg-test :only [rg]])
  (:require [funnyqt.tg                  :as tg]
            [funnyqt.query               :as q]
            [funnyqt.generic             :as g]
            [funnyqt.relational.tmp-elem :as tmp]))

(generate-metamodel-relations "test/input/greqltestgraph.tg"
                              test.relational.routemap.tg routemap +)

;;* Basic tests

(deftest test-elemento
  (is (= (tg/vseq rg)
         (run* [q]
           (elemento rg q)))))

(deftest test-relationshipo
  (is (= (tg/eseq rg)
         (run* [q]
           (with-fresh
             (relationshipo rg q _ _)))))
  (is (= (map (fn [e]
                [e (g/qname e) (tg/alpha e) (tg/omega e)])
              (tg/eseq rg))
         (run* [q]
           (with-fresh
             (typeo rg ?e ?t)
             (relationshipo rg ?e ?a ?o)
             (== q [?e ?t ?a ?o]))))))

(deftest test-elemento-and-relationshipo-with-type
  (is (= (tg/vseq rg 'Junction)
         (run* [q]
           (typeo rg q 'Junction)
           (elemento rg q))
         (run* [q]
           (routemap/+Junction rg q))))
  (is (= (tg/eseq rg 'Connection)
         (run* [q]
           (with-fresh
             (typeo rg q 'Connection)
             (relationshipo rg q _ _)))
         (run* [q]
           (with-fresh
             (routemap/+Connection rg q _ _))))))

(deftest test-avalo
  (is (= (map (fn [e]
                [e (tg/value e :name)])
              (concat (tg/vseq rg '[NamedElement Plaza])
                      (tg/eseq rg 'Street)))
         (run* [q]
           (with-fresh
             (avalo rg ?elem :name ?val)
             (== q [?elem ?val])))
         (run* [q]
           (with-fresh
             (routemap/+name rg ?elem ?val)
             (== q [?elem ?val]))))))

(deftest test-adjo
  (is (= (g/adjs (tg/vertex rg 12) :localities)
         (run* [q]
           (adjo rg (tg/vertex rg 12) :localities q))
         (run* [q]
           (routemap/+->localities rg (tg/vertex rg 12) q))))
  (is (= (g/adjs (tg/vertex rg 12) :capital)
         (run* [q]
           (adjo rg (tg/vertex rg 12) :capital q))
         (run* [q]
           (routemap/+->capital rg (tg/vertex rg 12) q)))))
