(ns funnyqt.polyfns-test
  (:require [funnyqt.tg        :as tg])
  (:require [funnyqt.generic :as p])
  (:require [funnyqt.polyfns   :refer :all])
  (:require [funnyqt.utils     :as u])
  (:use [funnyqt.tg-test       :only [rg]])
  (:use clojure.test))

(declare-polyfn aec-name-no-default [elem])

(declare-polyfn aec-name-with-default [elem]
                "--undefined--")

(defpolyfn aec-name-no-default 'junctions.Junction [elem]
  "Junction")
(defpolyfn aec-name-with-default 'junctions.Junction [elem]
  "Junction")

(defpolyfn aec-name-no-default 'localities.Locality [elem]
  "Locality")
(defpolyfn aec-name-with-default 'localities.Locality [elem]
  "Locality")

(defpolyfn aec-name-no-default 'localities.City [elem]
  "City")
(defpolyfn aec-name-with-default 'localities.City [elem]
  "City")

(defpolyfn aec-name-no-default 'connections.Connection [elem]
  "Connection")
(defpolyfn aec-name-with-default 'connections.Connection [elem]
  "Connection")

(try
  (aec-name-no-default (tg/first-vertex rg))
  (catch Exception e
    (if (re-matches #"2 polyfns are applicable.*" (.getMessage e))
      (println "Tie in polyfn impls successfully detected.")
      (u/errorf "Tie in polyfn impls for aec-name-no-default not detected!"))))

(try
  (aec-name-with-default (tg/first-vertex rg))
  (catch Exception e
    (if (re-matches #"2 polyfns are applicable.*" (.getMessage e))
      (println "Tie in polyfn impls successfully detected.")
      (u/errorf "Tie in polyfn impls for aec-name-with-default not detected!"))))

(defpolyfn aec-name-no-default 'junctions.Airport [e]
  "Airport")
(defpolyfn aec-name-with-default 'junctions.Airport [e]
  "Airport")

(deftest test-polyfns-tg
  (doseq [x (tg/vseq rg '[:and Junction !Airport])]
    (is (= "Junction" (aec-name-no-default x)))
    (is (= "Junction" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'Airport)]
    (is (= "Airport" (aec-name-no-default x)))
    (is (= "Airport" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg '[:and Locality !City !Airport])]
    (is (= "Locality" (aec-name-no-default x)))
    (is (= "Locality" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'City)]
    (is (= "City" (aec-name-no-default x)))
    (is (= "City" (aec-name-with-default x))))

  (doseq [x (tg/eseq rg 'Connection)]
    (is (= "Connection" (aec-name-no-default x)))
    (is (= "Connection" (aec-name-with-default x))))

  (doseq [x (tg/vseq rg 'County)]
    (is (thrown-with-msg? Exception
                          #"No polyfn implementation defined"
                          (aec-name-no-default x)))
    (is (= "--undefined--" (aec-name-with-default x)))))

