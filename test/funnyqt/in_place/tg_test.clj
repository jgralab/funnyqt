(ns ^{:pattern-expansion-context :tg}
  funnyqt.in-place.tg-test
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.tg)
  (:use funnyqt.in-place)
  (:use funnyqt.query.tg)
  (:use funnyqt.tg-test)
  (:use [funnyqt.query.tg-test :only (eval-exp)])
  (:use clojure.test))


;;* BinTree eval

(defn bin-tree
  []
  (let [g (create-graph
           (load-schema "test/input/binop-tree-schema.tg" :standard)
           "ExampleBinaryGraphFunML" :standard)
        v1 (create-vertex! g 'Div)
        v2 (create-vertex! g 'Add)
        v3 (create-vertex! g 'Sub)
        v4 (create-vertex! g 'Mul)
        v5 (doto (create-vertex! g 'Const) (set-value! :value 3.0))
        v6 (doto (create-vertex! g 'Const) (set-value! :value 42.0))
        v7 (doto (create-vertex! g 'Const) (set-value! :value 2.0))
        v8 (doto (create-vertex! g 'Const) (set-value! :value 7.0))
        v9 (doto (create-vertex! g 'Const) (set-value! :value 9.0))]
    (doseq [[a o] [[v1 v2] [v1 v3] [v2 v4] [v2 v5] [v3 v6] [v3 v7]
                   [v4 v8] [v4 v9]]]
      (create-edge! g 'HasArg a o))
    g))

(defrule replace-binaryop
  "Replaces a binary operation with constant args with
  a constant of the result."
  [g] [b<BinaryOp> -<HasArg>-> a1<Const>
       b -<HasArg>-> a2<Const>
       :when (not= a1 a2)]
  (let [c (create-vertex! g 'Const)]
    (set-value! c :value (eval-exp b))
    (relink! b c nil :in))
  (delete! [b a1 a2]))

(deftest test-replace-binops
  (let [tree (bin-tree)]
    (is (== 4 (iteratively replace-binaryop tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (the (vseq tree)) :value)))))

(deftest test-replace-binops2
  (let [tree (bin-tree)]
    (is (== 4 (iteratively
               ;; Also try with an anonymous rule
               (rule [g]
                     [b<BinaryOp> -<HasArg>-> a1<Const>
                      b -<HasArg>-> a2<Const>
                      :when (not= a1 a2)]
                     (let [c (create-vertex! g 'Const)]
                       (set-value! c :value (eval-exp b))
                       (relink! b c nil :in))
                     (delete! [b a1 a2]))
               tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (the (vseq tree)) :value)))))

(deftest test-replace-binops3
  (let [tree (bin-tree)]
    (is (== 4 (iteratively
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
                          (delete! [b a1 a2])))
               tree)))
    (is (== 1 (vcount tree)))
    (is (== 0 (ecount tree)))
    (is (== 1.65 (value (the (vseq tree)) :value)))))

(deftest test-replace-binops4
  (let [tree (bin-tree)]
    (letrule [(repl-bin-op
               [g] [b<BinaryOp> -<HasArg>-> a1<Const>
                    b -<HasArg>-> a2<Const>
                    :when (not= a1 a2)]
               (let [c (create-vertex! g 'Const)]
                 (set-value! c :value (eval-exp b))
                 (relink! b c nil :in))
               (delete! [b a1 a2]))]
             (is (== 4 (iteratively repl-bin-op tree)))
             (is (== 1 (vcount tree)))
             (is (== 0 (ecount tree)))
             (is (== 1.65 (value (the (vseq tree)) :value))))))

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
                  (delete! [b a1 a2])))]
      (is (== 4 (iteratively repl-bin-op tree)))
      (is (== 1 (vcount tree)))
      (is (== 0 (ecount tree)))
      (is (== 1.65 (value (the (vseq tree)) :value))))))


