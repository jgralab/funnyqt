(ns funnyqt.relational.test.tg
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :only [run* ==]]
        clojure.test
        funnyqt.relational.tg
        funnyqt.relational
        [funnyqt.test.tg :only [rg]])
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.relational.tmp-elem :as tmp]))

(generate-schema-relations "test/input/greqltestgraph.tg" routemap)

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

;;* Tmp elements

(deftest test-tmp-vertexo
  (binding [tmp/*make-tmp-elements* true]
    ;; In the fresh case, vertexo should return a TmpElement with kind :vertex
    ;; as last answer.
    (let [v (last (run* [q] (vertexo rg q)))]
      (is (tmp/tmp-vertex? v)))
    ;; If it's a TmpElement, it should set the kind to :vertex
    (let [v (last (run* [q]
                    (== q (tmp/make-tmp-element rg))
                    (vertexo rg q)))]
      (is (tmp/tmp-vertex? v)))
    ;; If it's a TmpElement with kind :vertex, it should simply succeed.
    (is (seq (run* [q] (vertexo rg (tmp/make-tmp-vertex rg)))))))

(deftest test-tmp-typeo
  (binding [tmp/*make-tmp-elements* true]
    ;; In the fresh case, it should add one TmpElement with preset :kind and
    ;; :type.
    (let [v (last (run* [q] (typeo rg q 'localities.City)))]
      (is (tmp/tmp-vertex? v))
      (is (= (tmp/get-type v) 'localities.City)))
    ;; ditto if an edge class is given
    (let [v (last (run* [q] (typeo rg q 'HasCapital)))]
      (is (tmp/tmp-edge? v))
      (is (= (tmp/get-type v) 'localities.HasCapital)))
    ;; For ground TmpElements it should set :kind and :type
    (let [v (last (run* [q]
                    (== q (tmp/make-tmp-element rg))
                    (typeo rg q 'City)))]
      (is (tmp/tmp-vertex? v))
      (is (= (tmp/get-type v) 'localities.City)))
    ;; ditto for an edge class
    (let [v (last (run* [q]
                    (== q (tmp/make-tmp-element rg))
                    (typeo rg q 'HasCapital)))]
      (is (tmp/tmp-edge? v))
      (is (= (tmp/get-type v) 'localities.HasCapital)))
    ;; TODO: test it simply succeeds on correct TmpElements
    ))

