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

(defn ^:private counter-graph [digits]
  (let [s (load-schema "test/input/counter-schema.tg")
        g (new-graph s)]
    (dotimes [i digits]
      (let [h (tg/create-vertex! g 'Digit {:val i})]
        (when-not (zero? i)
          (create-edge! g 'HasNext (tg/prev-vertex h) h))))
    (create-edge! g 'HasNext (tg/last-vertex g) (tg/first-vertex g))
    (let [c (tg/create-vertex! g 'Counter)]
      (create-edge! g 'HasPrimaryDigit c (tg/vertex g 1))
      (create-edge! g 'HasSecondaryDigit c (tg/vertex g 1)))
    g))

(defrule tick-forward [g]
  [c<Counter> -sec<:secondary>-> <> -<HasNext>-> n
   :alternative [[:when (not (zero? (tg/value n :val)))]
                 [:when (zero? (tg/value n :val))
                  c -prim<:primary>-> <> -<HasNext>-> n2]]]
  (when prim
    (tg/set-omega! prim n2))
  (tg/set-omega! sec n))

(defrule tick-backward [g]
  [c<Counter> -sec<:secondary>-> cur<> <-<HasNext>- p
   :alternative [[:when (not (zero? (tg/value cur :val)))]
                 [:when (zero? (tg/value cur :val))
                  c -prim<:primary>-> <> <-<HasNext>- p2]]]
  (when prim
    (tg/set-omega! prim p2))
  (tg/set-omega! sec p))

(deftest test-create-state-space-1
  (let [g (counter-graph 3)
        [ssg s2m _] (create-state-space g
                                        #(g/equal-models? %1 %2 false)
                                        [tick-forward tick-backward])]
    (is (= 9 (vcount ssg 'State)))
    (is (= 18 (ecount ssg 'Transition)))))

(defrule reset-counter [g]
  [c<Counter> -<:secondary>-> d1
   c<Counter>  -<:primary>->  d2
   :when (or (not (zero? (tg/value d1 :val)))
             (not (zero? (tg/value d2 :val))))]
  (let [digit-zero (q/the #(zero? (tg/value % :val))
                          (tg/vseq g 'Digit))]
    (g/set-adj! c :secondary digit-zero)
    (g/set-adj! c :primary digit-zero)))

(deftest test-create-state-space-2
  (let [g (counter-graph 3)
        [ssg s2m _] (create-state-space g
                                        #(g/equal-models? %1 %2 false)
                                        [tick-forward tick-backward reset-counter])]
    (is (= 9 (vcount ssg 'State)))
    ;; 26, not 27, because reset-counter doesn't match in the 0:0 case.
    (is (= 26 (ecount ssg 'Transition)))))

(defn test-explore-state-space []
  (let [g (counter-graph 3)]
    (explore-state-space
     g
     #(g/equal-models? %1 %2 false)
     [tick-forward tick-backward reset-counter]
     {:state-preds []
      :transition-preds {}
      :state-space-preds []})))
