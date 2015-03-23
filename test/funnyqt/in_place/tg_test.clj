(ns ^{:pattern-expansion-context :tg}
  funnyqt.in-place.tg-test
  (:require [funnyqt.generic :as g]
            [funnyqt.query   :as q]
            [funnyqt.tg      :as tg])
  (:use funnyqt.tg)
  (:use funnyqt.in-place)
  (:use funnyqt.tg-test)
  (:use clojure.test))


;;* BinTree eval

(defrule replace-binaryop
  "Replaces a binary operation with constant args with
  a constant of the result."
  [g] [b<BinaryOp> -<HasArg>-> a1<Const>
       b -<HasArg>-> a2<Const>
       :when (not= a1 a2)]
  (let [c (create-vertex! g 'Const)]
    (set-value! c :value (eval-exp b))
    (relink! b c nil :in))
  (g/delete! [b a1 a2]))

(deftest test-replace-binops
  (let [tree (bin-tree)]
    (is (== 4 ((iterated-rule replace-binaryop) tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (q/the (vseq tree)) :value)))))

(deftest test-replace-binops2
  (let [tree (bin-tree)]
    (is (== 4 ((iterated-rule
                ;; Also try with an anonymous rule
                (rule [g]
                      [b<BinaryOp> -<HasArg>-> a1<Const>
                       b -<HasArg>-> a2<Const>
                       :when (not= a1 a2)]
                      (let [c (create-vertex! g 'Const)]
                        (set-value! c :value (eval-exp b))
                        (relink! b c nil :in))
                      (g/delete! [b a1 a2])))
               tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (q/the (vseq tree)) :value)))))

(deftest test-replace-binops3
  (let [tree (bin-tree)]
    (is (== 4 ((iterated-rule
                ;; Also try with an anonymous rule with a label and more than
                ;; one sig.
                (rule foo
                      ([g x] [x --> y]
                       (throw (RuntimeException. "Must not have happened.")))
                      ([g]
                       [b<BinaryOp> -<HasArg>-> a1<Const>
                        b -<HasArg>-> a2<Const>
                        :when (not= a1 a2)]
                       (let [c (create-vertex! g 'Const)]
                         (set-value! c :value (eval-exp b))
                         (relink! b c nil :in))
                       (g/delete! [b a1 a2]))))
               tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (q/the (vseq tree)) :value)))))

(deftest test-replace-binops4
  (let [tree (bin-tree)]
    (letrule [(repl-bin-op
               [g] [b<BinaryOp> -<HasArg>-> a1<Const>
                    b -<HasArg>-> a2<Const>
                    :when (not= a1 a2)]
               (let [c (create-vertex! g 'Const)]
                 (set-value! c :value (eval-exp b))
                 (relink! b c nil :in))
               (g/delete! [b a1 a2]))]
      (is (== 4 ((iterated-rule repl-bin-op) tree)))
      (is (== 1 (vcount tree)))
      (is (== 0 (ecount tree)))
      (is (== 1.65 (value (q/the (vseq tree)) :value))))))

(deftest test-replace-binops5
  (let [tree (bin-tree)]
    (letrule [(repl-bin-op
               ([g x] [x --> y]
                (throw (RuntimeException. "Must not have happened.")))
               ([g] [b<BinaryOp> -<HasArg>-> a1<Const>
                     b -<HasArg>-> a2<Const>
                     :when (not= a1 a2)]
                (let [c (create-vertex! g 'Const)]
                  (set-value! c :value (eval-exp b))
                  (relink! b c nil :in))
                (g/delete! [b a1 a2])))]
      (is (== 4 ((iterated-rule repl-bin-op) tree)))
      (is (== 1 (vcount tree)))
      (is (== 0 (ecount tree)))
      (is (== 1.65 (value (q/the (vseq tree)) :value))))))

(defn ^:private counter-graph []
  (let [s (load-schema "test/input/counter-schema.tg")
        g (new-graph s)]
    (dotimes [i 5]
      (let [h (tg/create-vertex! g 'Digit {:val (inc i)})]
        (when-not (zero? i)
          (create-edge! g 'HasNext (tg/vertex g i) h))))
    (create-edge! g 'HasNext (tg/vertex g 5) (tg/vertex g 1))
    (let [c (tg/create-vertex! g 'Counter)]
      (create-edge! g 'HasPrimaryDigit c (tg/vertex g 12))
      (create-edge! g 'HasSecondaryDigit c (tg/vertex g 12)))
    g))

(defn max-digit-val [g]
  (reduce max (map #(tg/value % :val))))

(defrule tick-forward [g]
  [c<Counter> -sec<:secondary>-> <> -<HasNext>-> n
   :alternative [[:when (not= (tg/value n :val) (max-digit-val g))]
                 [c -prim<:primary>-> <> -<HasNext>-> n2]]]
  (tg/set-omega! sec n)
  (when prim
    (tg/set-omega! prim n2)))

(defn test-explore-state-space []
  (let [g (counter-graph)]
    (explore-state-space
     g
     #(g/equal-models? %1 %2 false)
     [tick-forward]
     []
     [])))
