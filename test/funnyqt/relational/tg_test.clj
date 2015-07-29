(ns funnyqt.relational.tg-test
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [run* ==]]
            [clojure.test :refer :all]
            [funnyqt
             [emf :as emf]
             [generic :as g]
             [relational :refer :all]
             [tg :as tg]
             [tg-test :refer :all]
             [model2model-test :as m2m-test]]
            [funnyqt.coevo.tg :as coevo]))

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

;;* Tests with the Genealogy graph

(def g (let [fm (emf/load-resource "test/input/example.families")
             g (tg/new-graph (tg/load-schema "test/input/genealogy-schema.tg"))]
         (m2m-test/families2genealogy fm g)
         (coevo/delete-attribute! g 'Person :ageGroup)
         g))

(deftest test-elemento-familygraph
  (is (= (g/elements g)
         (run* [q]
           (elemento g q)))))

