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
    (let [r (run* [q] (vertexo rg q))]
      (is (seq r))
      (is (tmp/tmp-vertex? (last r))))
    ;; If it's a TmpElement, it should set the kind to :vertex
    (let [r (run* [q]
              (== q (tmp/make-tmp-element rg))
              (vertexo rg q))]
      (is (seq r))
      (is (tmp/tmp-vertex? (last r))))
    ;; If it's a TmpElement with kind :vertex, it should simply succeed.
    (is (seq (run* [q] (vertexo rg (tmp/make-tmp-vertex rg)))))))

(deftest test-tmp-typeo
  (binding [tmp/*make-tmp-elements* true]
    ;; In the fresh case, it should add one TmpElement with preset :kind and
    ;; :type.
    (let [r (run* [q] (typeo rg q 'localities.City))]
      (is (seq r))
      (let [v (last r)]
        (is (tmp/tmp-vertex? v))
        (is (= (tmp/get-type v)
               (tg/attributed-element-class rg 'localities.City)))))
    ;; ditto if an edge class is given
    (let [r (run* [q] (typeo rg q 'HasCapital))]
      (is (seq r))
      (let [v (last r)]
        (is (tmp/tmp-edge? v))
        (is (= (tmp/get-type v)
               (tg/attributed-element-class rg 'localities.HasCapital)))))
    ;; For ground TmpElements it should set :kind and :type
    (let [r (run* [q]
              (== q (tmp/make-tmp-element rg))
              (typeo rg q 'City))]
      (is (seq r))
      (let [v (last r)]
        (is (tmp/tmp-vertex? v))
        (is (= (tmp/get-type v)
               (tg/attributed-element-class rg 'localities.City)))))
    ;; ditto for an edge class
    (let [r (run* [q]
              (== q (tmp/make-tmp-element rg))
              (typeo rg q 'HasCapital))]
      (is (seq r))
      (let [v (last r)]
        (is (tmp/tmp-edge? v))
        (is (= (tmp/get-type v)
               (tg/attributed-element-class rg 'localities.HasCapital)))))
    ;; It simply succeeds on correct TmpElements
    (let [te (tmp/make-tmp-element rg 'City)]
      (is (seq (run* [q]
                 (typeo rg te 'City)
                 (vertexo rg te)))))
    ;; check if type resetting to more special types works
    (let [te (tmp/make-tmp-element rg 'Locality)
          r (run* [q]
              (== q te)
              (typeo rg q 'City)
              (vertexo rg q))]
      (is (= (count r) 1))
      (is (tmp/tmp-vertex? (first r)))
      (is (= (tmp/get-type (first r))
             (tg/attributed-element-class rg 'City))))))

(deftest test-tmp-edgeo
  (binding [tmp/*make-tmp-elements* true]
    ;; When q, alpha, and omega are fresh, it should deliver all existing edges
    ;; (with their respective alphas and omegas) plus one tmp edge.
    (let [r (run* [q]
              (with-fresh
                (edgeo rg q _ _)))]
      (is (= (count r) (inc (tg/ecount rg))))
      (is (tmp/tmp-edge? (last r))))
    ;; When alpha or omega is ground, it should deliver all incident existing
    ;; edges plus one tmp edge.  v122 has 3 outgoing edges.
    (let [r (run* [q]
              (with-fresh
                (edgeo rg q (tg/vertex rg 122) _)))]
      (is (= (count r)
             (inc (count (tg/iseq (tg/vertex rg 122) nil :out)))
             4))
      (is (tmp/tmp-edge? (last r))))
    ;; v122 has 2 incoming edges.
    (let [r (run* [q]
              (with-fresh
                (edgeo rg q _ (tg/vertex rg 122))))]
      (is (= (count r)
             (inc (count (tg/iseq (tg/vertex rg 122) nil :in)))
             3))
      (is (tmp/tmp-edge? (last r))))
    ;; If the given edge is a ground tmp edge, the given alpha and omega should
    ;; be set (if they are ground).
    ;; There are 3 outgoing edges at v122.
    (let [e (tmp/make-tmp-edge rg)
          a (tg/vertex rg 122)
          r (run* [q]
              (with-fresh
                (edgeo rg q a _)))]
      (is (= (count r)
             (inc (count (tg/iseq (tg/vertex rg 122) nil :out)))
             4))
      (is (tmp/tmp-edge? (last r)))
      ;; The alpha & omega should be set
      (is (= (tmp/get-alpha (last r)) (tg/vertex rg 122)))
      ;; The omega should is a new tmp vertex
      (is (tmp/tmp-vertex? (tmp/get-omega (last r)))))
    ;; There is one single edge from v122 to v123.
    (let [e (tmp/make-tmp-edge rg)
          a (tg/vertex rg 122)
          o (tg/vertex rg 123)
          r (run* [q]
              (edgeo rg q a o))]
      (is (= (count r) 2))
      (is (tmp/tmp-edge? (last r)))
      ;; The alpha and omega should be set
      (is (= (tmp/get-alpha (last r)) (tg/vertex rg 122)))
      (is (= (tmp/get-omega (last r)) (tg/vertex rg 123))))))

(deftest test-tmp-valueo
  (binding [tmp/*make-tmp-elements* true]
    ;; With (valueo g elem attr val), attr and val must be ground already when
    ;; *make-tmp-elements* is true.

    ;; This should deliver all NamedElement & Plaza vertices + 1 TmpElement.
    (let [r (run* [q]
              (with-fresh
                (vertexo rg q)
                (valueo rg q :name _)))]
      (is (= (count r) (inc (count (tg/vseq rg '[NamedElement Plaza]))))))
    ;; This should deliver the City Mainz + one tmp vertex with name attr set
    ;; to "Mainz".
    (let [r (run* [q]
              (with-fresh
                (vertexo rg q)
                (valueo rg q :name "Mainz")))]
      (is (= (count r) 2))
      (is (tmp/tmp-vertex? (last r)))
      (is (= (:name (tmp/get-attrs (last r))) "Mainz"))) ))

#_(binding [tmp/*make-tmp-elements* true]
  (let [r (run* [q]
            (with-fresh
              (routemap/+City rg ?city)
              (routemap/+name rg ?city "Koblenz")
              ;;(routemap/+ContainsCrossroad rg ?cc ?city ?cr)
              (typeo rg ?cc 'ContainsCrossroad)
              (edgeo rg ?cc ?city ?cr)
              (routemap/+Crossroad rg ?cr)
              (== q [?city ?cc ?cr])))]
    (tmp/as-map (second (last r)))))
