(ns funnyqt.query-test
  (:use [ordered.set])
  (:use [funnyqt.query])
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

(deftest test-xor-and-xor-fn
  (are [expected in] (= expected
                        (apply xor in)
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
       false [true false false false false true]))

(deftest test-xor-fn
  (are [expected in] (= expected ((apply xor-fn in) 15))
       ;; One pred matches
       true  [#(== 13 %) #(== 15 %) #(== 17 %)]
       ;; No pred matches
       false [#(== 13 %) #(== 151 %) #(== 17 %)]
       ;; More than one matches
       false [#(== 13 %) #(== 15 %) #(== 17 %) #(== 0 (mod % 5))]
       ;; No pred given
       false []))

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

(deftest test-seq-compare
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
              (seq-compare - compare #(- %2 %1))
              (shuffle sorted)))))))

