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
    (is (== 4 (apply-repeatedly replace-binaryop tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (q/the (vseq tree)) :value)))))

(deftest test-replace-binops2
  (let [tree (bin-tree)]
    (is (== 4 (apply-repeatedly
               ;; Also try with an anonymous rule
               (rule [g]
                     [b<BinaryOp> -<HasArg>-> a1<Const>
                      b -<HasArg>-> a2<Const>
                      :when (not= a1 a2)]
                     (let [c (create-vertex! g 'Const)]
                       (set-value! c :value (eval-exp b))
                       (relink! b c nil :in))
                     (g/delete! [b a1 a2]))
               tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (q/the (vseq tree)) :value)))))

(deftest test-replace-binops3
  (let [tree (bin-tree)]
    (is (== 4 (apply-repeatedly
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
                      (g/delete! [b a1 a2])))
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
      (is (== 4 (apply-repeatedly repl-bin-op tree)))
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
      (is (== 4 (apply-repeatedly repl-bin-op tree)))
      (is (== 1 (vcount tree)))
      (is (== 0 (ecount tree)))
      (is (== 1.65 (value (q/the (vseq tree)) :value))))))

(defn ^:private clock-graph []
  (let [s (load-schema "test/input/clock-schema.tg")
        g (new-graph s)]
    (dotimes [i 12]
      (let [h (tg/create-vertex! g 'Hour {:hour (inc i)})]
        (when-not (zero? i)
          (create-edge! g 'NextHour (tg/vertex g i) h))))
    (create-edge! g 'NextHour (tg/vertex g 12) (tg/vertex g 1))
    (let [c (tg/create-vertex! g 'Clock)]
      (create-edge! g 'CurrentHour c (tg/vertex g 12)))
    g))

(defrule tick-forward [g]
  [c<Clock> -ch<CurrentHour>-> <> -<NextHour>-> n]
  (tg/set-omega! ch n))

(defrule tick-backward [g]
  [c<Clock> -ch<CurrentHour>-> <> <-<NextHour>- n]
  (tg/set-omega! ch n))

(defrule reset-clock [g]
  [c<Clock> -ch<CurrentHour>-> h
   :when (not= 12 (value h :hour))]
  (set-omega! ch (vertex g 12)))

(deftest test-state-space
  (let [g (clock-graph)
        [ssg s2g] (create-state-space
                   g [tick-forward tick-backward reset-clock]
                   g)]
    ;;(./print-model ssg :gtk)
    (is (= 12 (vcount ssg)))
    ;; From every Hour we can tick forward and backward, and from every state
    ;; except for the first, we can reset to 12 o'clock.
    (is (= 35 (ecount ssg)))))
