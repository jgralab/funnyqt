(ns funnyqt.test.generic
  (:use [ordered.set])
  (:use [funnyqt.generic])
  (:use [clojure.test]))

(deftest test-forall?
  (is (forall? even? []))
  (is (forall? even? [2 4 10 100 666]))
  (is (forall? even? (take 50 (iterate (fn [x] (+ 2 x)) 0))))
  (is (not (forall? even? [0 2 4 5 6]))))

(deftest test-exists?
  (is (exists? even? [1 -7 3 5 6 -19 -4]))
  (is (not (exists? even? []))))

(deftest test-exists1?
  (is (exists1? #(== 1 %) (range -100 100)))
  (is (not (exists1? even? [1 -7 3 5 6 -19 -4])))
  (is (not (exists1? even? [])))
  (is (exists1? even? [2]))
  (is (exists1? even? [3 2 1])))
