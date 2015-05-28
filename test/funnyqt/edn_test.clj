(ns funnyqt.edn-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [funnyqt.utils :as u]
            [funnyqt.edn  :as edn]
            [funnyqt.tg   :as tg]
            [funnyqt.emf  :as emf]))

(def test-graph (tg/load-graph "test/input/familygraph.tg"))

(when (try (do (emf/eclassifier 'FamilyModel)
               false)
           (catch Exception _
             true))
  (emf/load-ecore-resource "test/input/Families.ecore"))

(def test-resource-set (doto (emf/new-resource-set)
                         (emf/new-resource "test/input/Families.ecore")
                         (emf/new-resource "test/input/example.families")))

(def test-resource (emf/get-resource test-resource-set
                                     "test/input/example.families" true))

(defn edn-roundtrip [obj & models]
  (let [val (edn/read-string (edn/pr-str obj) models)
        f (java.io.File/createTempFile "test" "edn")]
    (edn/spit obj f)
    (is (= obj (edn/slurp f models)))
    val))

(deftest test-standard-edn-types
  ;; nil
  (is (nil? (edn-roundtrip nil)))
  ;; booleans
  (is (true? (edn-roundtrip true)))
  (is (false? (edn-roundtrip false)))
  ;; characters (& vectors)
  (let [chars [\a \@ \€ \newline \space \tab \uFFFF]
        result (edn-roundtrip chars)]
    (is (= chars result))
    (is (vector? result)))
  ;; symbols (& vectors)
  (let [syms '[foo bar$ some/sym <&=?%>]
        result (edn-roundtrip syms)]
    (is (= syms result))
    (is (vector? result)))
  ;; keywords (& vectors)
  (let [kws [:foo :bar% :some/kw :<&=?%>]
        result (edn-roundtrip kws)]
    (is (= kws result))
    (is (vector? result)))
  ;; numbers (& lists)
  (let [nums (list 0 -18 2.817 Math/PI -2.291e19)
        result (edn-roundtrip nums)]
    (is (= nums result))
    (is (list? result)))
  ;; strings (& maps)
  (let [strings {"foo" "\n", "String \"with\" quotes" "\nyes\n"}
        result (edn-roundtrip strings)]
    (is (= strings result))
    (is (map? result)))
  ;; sets
  (let [sets #{#{3 2 1 0} #{1 -19 -0.5 :foo [0 0 0]}
               ;; TODO: Comment in as soon as
               ;; http://dev.clojure.org/jira/browse/CLJ-1739 is fixed.
               #_(java.util.HashSet. [1 2 3])
               #_(u/oset [2 2 2])}
        result (edn-roundtrip sets)]
    (is (= sets result))
    (is (set? result))
    (is (every? set? result)))
  ;; vectors (& collections implementing RandomAccess)
  (let [vs [[1 2 3] (java.util.ArrayList. [3 2 1]) (java.util.Vector. [0 1 1 "foo"])]
        result (edn-roundtrip vs)]
    (is (= vs result))
    (is (vector? result))
    (is (every? vector? result)))
  ;; lists
  (let [ls (list (list 1 2 3) (java.util.LinkedList. [3 2 1]) (range 10))
        result (edn-roundtrip ls)]
    (is (= ls result))
    (is (list? result))
    (is (every? list? result))))

(deftest test-edn-tg
  (let [g test-graph
        vs (tg/vseq g)
        es (tg/eseq g)
        rg (edn-roundtrip g test-graph)
        rvs (edn-roundtrip vs test-graph)
        res (edn-roundtrip es test-graph)]
    (is (= g rg))
    (is (= vs rvs))
    (is (= es res))))

(deftest test-edn-emf
  (let [r test-resource
        rr1 (edn-roundtrip r test-resource)
        ;; We can also retrieve a resourse out of a resource set
        rr2 (edn-roundtrip r test-resource-set)

        rs test-resource-set
        rrs (edn-roundtrip rs test-resource-set)

        eos-r (emf/eallcontents r)
        eos-rs (emf/eallcontents rs)
        reos-r (edn-roundtrip eos-r test-resource)
        reos-rs (edn-roundtrip eos-rs test-resource-set)]
    (is (= r rr1 rr2))
    (is (= rs rrs))
    (is (= eos-r reos-r))
    (is (= eos-rs reos-rs))))
