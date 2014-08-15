(ns funnyqt.query-test
  (:use [flatland.ordered.set])
  (:use [funnyqt.query])
  (:use [clojure.test])
  (:require [clojure.core.reducers :as r]
            [funnyqt.utils :as u]
            [funnyqt.emf :as emf]
            [funnyqt.tg  :as tg]))

(deftest test-forall?
  (is (forall? even? []))
  (is (forall? even? [2 4 10 100 666]))
  (is (forall? even? (take 50 (iterate (fn [x] (+ 2 x)) 0))))
  (is (not (forall? even? [0 2 4 5 6]))))

(deftest test-exists?
  (is (exists? even? [1 -7 3 5 6 -19 -4]))
  (is (not (exists? even? [])))
  ;; Test that we're not realizing lazy seqs too much
  (is (exists? even? (range))))

(deftest test-exist-n?
  (is (exist-n? 1 #(== 1 %) (range -100 100)))
  (is (not (exist-n? 1 even? [1 -7 3 5 6 -19 -4])))
  (is (not (exist-n? 1 even? [])))
  (is (exist-n? 3 even? [1 2 3 4 5 6]))
  (is (not (exist-n? 3 even? [1 2 3 4 5 6 8])))
  (is (not (exist-n? 3 even? [1 2 3 4 5])))
  (is (exist-n? 1 even? [2]))
  (is (exist-n? 1 even? [3 2 1]))
  ;; Test that we're not realizing lazy seqs too much
  (is (not (exist-n? 100 even? (range)))))

(deftest test-member?
  (is (member? 1 #{0 1 2}))
  (is (member? 1 [0 1 2]))
  (is (member? nil [nil 1 2]))
  (is (member? nil [0 nil 1 2]))
  (is (member? nil [0 nil]))
  (is (not (member? nil [0 1]))))

(deftest test-the
  (is (== 1 (the [1])))
  (is (== 1 (the #{1})))
  (is (= [1 2] (the {1 2})))
  (is (thrown-with-msg? Exception #"zero" (the [])))
  (is (thrown-with-msg? Exception #"more than" (the [1 2]))))

(deftest test-pred-seq
  (is (= [[nil 1] [1 2] [2 3] [3 4]]
         (pred-seq [1 2 3 4]))))

(deftest test-succ-seq
  (is (= [[1 2] [2 3] [3 4] [4 nil]]
         (succ-seq [1 2 3 4]))))

(deftest test-xor*-and-xor-fn
  (are [expected in] (= expected
                        (apply xor* in)
                        ((apply xor-fn (map constantly in))))
       false []
       true  [true]
       false [false]
       true  [true false]
       true  [false true]
       false [true true]
       false [false false]
       true  [true false false false false]
       true  [false false false false true]
       false [true false false false false true]
       true  [true false false false true true]))

(deftest test-xor-fn
  (are [expected in] (= expected ((apply xor-fn in) 15))
       ;; One pred matches
       true  [#(== 13 %) #(== 15 %) #(== 17 %)]
       ;; No pred matches
       false [#(== 13 %) #(== 151 %) #(== 17 %)]
       ;; 2 preds match
       false [#(== 13 %) #(== 15 %) #(== 17 %) #(== 0 (mod % 5))]
       ;; 3 preds match
       true  [#(< % 20) #(== 13 %) #(== 15 %) #(== 17 %) #(== 0 (mod % 5))]
       ;; No pred given
       false []))

(deftest test-logicals
  ;; AND
  (are [expected in] (= expected (apply and* in) (reduce #(and %1 %2) true in))
       3     [1 2 3]
       true  []
       false [1 false]
       nil   [nil 1 2 3 4])
  (is (= (and) (and*)))
  (is (= (and 1 2 3) (and* 1 2 3)))
  (is (= (and 1 2 nil 3) (and* 1 2 nil 3)))
  ;; NAND
  (are [expected in] (= expected (apply nand* in) (not (reduce #(and %1 %2) true in)))
       false [1 2 3]
       false []
       true  [1 false]
       true  [nil 1 2 3 4])
  ;; OR
  (are [expected in] (= expected (apply or* in) (reduce #(or %1 %2) nil in))
       1     [1 2 3]
       nil   []
       1     [1 false]
       1     [nil 1 2 3 4]
       false [nil nil false])
  (is (= (or) (or*)))
  (is (= (or 1 2 3) (or* 1 2 3)))
  (is (= (or nil false nil) (or* nil false nil)))
  (is (= (or nil 1 false 2 nil) (or* nil 1 false 2 nil)))
  ;; NOR
  (are [expected in] (= expected (apply nor* in) (not (reduce #(or %1 %2) nil in)))
       false [1 2 3]
       true  []
       false [1 false]
       false [nil 1 2 3 4]
       true  [nil nil false])
  ;; XOR
  (are [expected in] (= expected (apply xor* in) (reduce #(xor %1 %2) false in))
       3     [1 2 3]
       false []
       1     [1 false]
       false [nil 1 2 3 4]
       false [nil nil false])
  (is (= (xor) (xor*)))
  (is (= (xor 1 2 3) (xor* 1 2 3))))

(deftest test-and-fn-nand-fn
  (are [expected in] (= expected
                        ((apply and-fn in) 15)
                        (not ((apply nand-fn in) 15)))
       ;; One pred matches
       false [#(== 13 %) #(== 15 %) #(== 17 %)]
       ;; No pred matches
       false [#(== 13 %) #(== 151 %) #(== 17 %)]
       ;; Two but not all match
       false [#(== 13 %) #(== 15 %) #(== 17 %) #(== 0 (mod % 5))]
       ;; No pred given ==> all given match
       true  []
       ;; All match
       true  [#(== 15 %)]
       true  [#(== 15 %) #(== 0 (mod % 5)) #(== 0 (mod % 3))]))

(deftest test-or-fn-nor-fn
  (are [expected in] (= expected
                        ((apply or-fn in) 15)
                        (not ((apply nor-fn in) 15)))
       ;; One pred matches
       true  [#(== 13 %) #(== 15 %) #(== 17 %)]
       ;; No pred matches
       false [#(== 13 %) #(== 151 %) #(== 17 %)]
       ;; Two but not all match
       true  [#(== 13 %) #(== 15 %) #(== 17 %) #(== 0 (mod % 5))]
       ;; No pred given
       false []
       ;; All match
       true  [#(== 15 %)]
       true  [#(== 15 %) #(== 0 (mod % 5)) #(== 0 (mod % 3))]))

(deftest test-seq-comparator
  (let [sorted [[-1 "zap" 19]
                [-1 "zap" 0]
                [0 "bar" 0]
                [0 "foo" 1]
                [0 "foo" 1]
                [0 "zap" 17]
                [1 "foo" 1]
                [1 "foo" 0]
                [2 "bar" 2]
                [2 "bar" 1]]]
    (dotimes [_ 30]
      (is (= sorted
             (sort
              ;; 1. ascending, 2. lexicographical, 3. descending
              (seq-comparator - compare #(- %2 %1))
              (shuffle sorted)))))))
