(ns funnyqt.polyfns-test
  (:require [clojure.test :refer :all]
            [funnyqt
             [generic :as p]
             [polyfns :refer :all]
             [tg :as tg]
             [tg-test :refer :all]
             [utils :as u]]))

;;* Tests

;;** Tests with the route-graph

(declare-polyfn aec-name-no-default [elem])

(declare-polyfn aec-name-no-default-no-dispatch-table
                {:no-dispatch-table true}
                [elem])

(declare-polyfn aec-name-with-default [elem]
                "--undefined--")

(defpolyfn aec-name-no-default junctions.Junction [elem]
  "Junction")
(defpolyfn aec-name-no-default-no-dispatch-table junctions.Junction [elem]
  "Junction")
(defpolyfn aec-name-with-default junctions.Junction [elem]
  "Junction")

(defpolyfn aec-name-no-default localities.Locality [elem]
  "Locality")
(defpolyfn aec-name-no-default-no-dispatch-table localities.Locality [elem]
  "Locality")
(defpolyfn aec-name-with-default localities.Locality [elem]
  "Locality")

(defpolyfn aec-name-no-default localities.City [elem]
  "City")
(defpolyfn aec-name-no-default-no-dispatch-table localities.City [elem]
  "City")
(defpolyfn aec-name-with-default localities.City [elem]
  "City")

(defpolyfn aec-name-no-default connections.Connection [elem]
  "Connection")
(defpolyfn aec-name-no-default-no-dispatch-table connections.Connection [elem]
  "Connection")
(defpolyfn aec-name-with-default connections.Connection [elem]
  "Connection")

(try
  (aec-name-no-default (tg/first-vertex rg))
  (catch Exception e
    (if (re-matches #"Multiple aec-name-no-default polyfn impls for type junctions\.Airport\." (.getMessage e))
      (println "Tie in polyfn impls successfully detected.")
      (u/errorf "Tie in polyfn impls for aec-name-no-default not detected!"))))

(try
  (aec-name-no-default-no-dispatch-table (first (tg/vseq rg 'Airport)))
  (catch Exception e
    (if (re-matches #"Multiple aec-name-no-default-no-dispatch-table polyfn impls for type junctions\.Airport\." (.getMessage e))
      (println "Tie in polyfn impls successfully detected.")
      (u/errorf "Tie in polyfn impls for aec-name-no-default-no-dispatch-table not detected!"))))

(try
  (aec-name-with-default (tg/first-vertex rg))
  (catch Exception e
    (if (re-matches #"Multiple aec-name-with-default polyfn impls for type junctions\.Airport\." (.getMessage e))
      (println "Tie in polyfn impls successfully detected.")
      (u/errorf "Tie in polyfn impls for aec-name-with-default not detected!"))))

(defpolyfn aec-name-no-default junctions.Airport [e]
  "Airport")
(defpolyfn aec-name-no-default-no-dispatch-table junctions.Airport [e]
  "Airport")
(defpolyfn aec-name-with-default junctions.Airport [e]
  "Airport")

(defpolyfn aec-name-no-default (localities.ContainsCrossroad
                                localities.ContainsLocality
                                localities.HasCapital)
  [e]
  "NoConnEdge")
(defpolyfn aec-name-no-default-no-dispatch-table
  (localities.ContainsCrossroad
   localities.ContainsLocality
   localities.HasCapital)
  [e]
  "NoConnEdge")
(defpolyfn aec-name-with-default (localities.ContainsCrossroad
                                  localities.ContainsLocality
                                  localities.HasCapital)
  [e]
  "NoConnEdge")

(deftest test-polyfns-tg
  (doseq [x (tg/vseq rg '[:and Junction !Airport])]
    (is (= "Junction" (aec-name-no-default x)))
    (is (= "Junction" (aec-name-no-default-no-dispatch-table x)))
    (is (= "Junction" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'Airport)]
    (is (= "Airport" (aec-name-no-default x)))
    (is (= "Airport" (aec-name-no-default-no-dispatch-table x)))
    (is (= "Airport" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg '[:and Locality !City !Airport])]
    (is (= "Locality" (aec-name-no-default x)))
    (is (= "Locality" (aec-name-no-default-no-dispatch-table x)))
    (is (= "Locality" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'City)]
    (is (= "City" (aec-name-no-default x)))
    (is (= "City" (aec-name-no-default-no-dispatch-table x)))
    (is (= "City" (aec-name-with-default x))))

  (doseq [x (tg/eseq rg 'Connection)]
    (is (= "Connection" (aec-name-no-default x)))
    (is (= "Connection" (aec-name-no-default-no-dispatch-table x)))
    (is (= "Connection" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'County)]
    (is (thrown-with-msg? Exception
                          #"No aec-name-no-default polyfn implementation defined"
                          (aec-name-no-default x)))
    (is (thrown-with-msg? Exception
                          #"No aec-name-no-default-no-dispatch-table polyfn implementation defined"
                          (aec-name-no-default-no-dispatch-table x)))
    (is (= "--undefined--" (aec-name-with-default x))))

  (doseq [conn (tg/eseq rg '!Connection)]
    (is (= "NoConnEdge" (aec-name-no-default conn)))
    (is (= "NoConnEdge" (aec-name-no-default-no-dispatch-table conn)))
    (is (= "NoConnEdge" (aec-name-with-default conn)))))

;;** Tests with the PolyfnTestSchema

(declare-polyfn ^:no-dispatch-table foo [el])
(defpolyfn foo A [el] :a)

(declare-polyfn ^:no-dispatch-table bar [el] :default)
(defpolyfn bar A [el] :a)
(defpolyfn bar B [el] :b)

(deftest test-with-polyfntestgraph
  (let [g (tg/new-graph (tg/load-schema "test/input/polyfntestschema.tg"))
        a (tg/create-vertex! g 'A)
        b (tg/create-vertex! g 'B)
        c (tg/create-vertex! g 'C)
        d (tg/create-vertex! g 'D)
        e (tg/create-vertex! g 'E)
        f (tg/create-vertex! g 'F)]
    ;; foo
    (is (= :a (foo a)))
    (is (= :a (foo b)))
    (is (= :a (foo c)))
    (is (= :a (foo d)))
    (is (thrown-with-msg? Exception #"No foo polyfn implementation defined for type E"
                          (foo e)))
    (is (= :a (foo f)))

    ;; bar
    (is (= :a (bar a)))
    (is (= :b (bar b)))
    (is (= :a (bar c)))
    (is (thrown-with-msg? Exception #"Multiple bar polyfn impls for type D."
                          (bar d)))
    (is (= :default (bar e)))
    (is (thrown-with-msg? Exception #"Multiple bar polyfn impls for type F."
                          (bar f)))))
