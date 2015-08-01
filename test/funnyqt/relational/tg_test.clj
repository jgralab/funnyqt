(ns funnyqt.relational.tg-test
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer [run* == conde conda all fresh membero nafc everyg succeed]]
            [clojure.core.logic.fd :as fd]
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

(generate-metamodel-relations "test/input/genealogy-schema.tg"
                              test.relational.genealogy.tg-test gen)

(def g (let [fm (emf/load-resource "test/input/example.families")
             g (tg/new-graph (tg/load-schema "test/input/genealogy-schema.tg"))]
         (m2m-test/families2genealogy fm g)
         (coevo/delete-attribute! g 'Person :ageGroup)
         g))

(deftest test-elemento-familygraph
  (is (= (g/elements g)
         (run* [q]
           (elemento g q)))))

(defn parento
  ([m parent child]
   (gen/->children m parent child))
  ([m parent-c child-c parent child]
   (all
    (parent-c m parent)
    (parento m parent child)
    (child-c m child))))

(defn fathero [m father child]
  (parento m gen/Male alwayso father child))

(defn mothero [m mother child]
  (parento m gen/Female alwayso mother child))

(defn sono [m parent son]
  (parento m alwayso gen/Male parent son))

(defn daughtero [m parent daughter]
  (parento m alwayso gen/Female parent daughter))

;; All fathers with their daughters
;; (run* [p c]
;;   (fathero g p c)
;;   (daughtero g p c))

(defn grandparento
  ([m grandparent grandchild]
   (fresh [parent]
     (parento m grandparent parent)
     (parento m parent grandchild)))
  ([m grandparent-c grandchild-c grandparent grandchild]
   (all
    (grandparent-c m grandparent)
    (grandparento m grandparent grandchild)
    (grandchild-c m grandchild))))

(defn grandfathero [m grandfather grandchild]
  (grandparento m gen/Male alwayso grandfather grandchild))

(defn grandmothero [m grandmother grandchild]
  (grandparento m gen/Female alwayso grandmother grandchild))

(defn grandsono [m grandparent grandson]
  (grandparento m alwayso gen/Male grandparent grandson))

(defn granddaughtero [m grandparent granddaughter]
  (grandparento m alwayso gen/Female grandparent granddaughter))


;; All grandmothers with their grandsons
;; (run* [gp gc]
;;   (grandmothero g gp gc)
;;   (grandsono g gp gc))

(defn ancestoro [m a p]
  (conde
   [(parento m a p)]
   [(fresh [i]
      (parento m a i)
      (ancestoro m i p))]))

;; (run* [a p]
;;   (ancestoro g a p))
