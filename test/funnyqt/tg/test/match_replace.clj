(ns funnyqt.tg.test.match-replace
  (:use funnyqt.generic)
  (:use funnyqt.generic-protocols)
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.match-replace)
  (:use funnyqt.tg.query)
  (:use funnyqt.tg.test.core)
  (:use [funnyqt.tg.test.query :only (eval-exp)])
  (:use clojure.test))


;;* Pattern match tests

(deftest with-match-1
  (with-match [a (vseq (rg) 'localities.Village)
               :when (> (value a :inhabitants) 252)
               e (iseq a nil :in)
               :let [b (that e)]
               :when (type-of? b 'localities.County)]
    (is (= (vertex (rg) 2) a))
    (is (= (vertex (rg) 12) b))
    (is (= (edge (rg) 23) (normal-edge e)))))

(deftest with-match-2
  (with-match [hc (eseq (rg) 'localities.HasCapital)
               :let [b (alpha hc)
                     a (omega hc)]
               :when (type-of? a 'localities.City)
               :when (type-of? b 'localities.County)
               cl (iseq b 'localities.ContainsLocality :out)
               :when (= (omega cl) a)]
    (is (= (vertex (rg) 6)  a))
    (is (= (vertex (rg) 12) b))
    (is (= (edge (rg) 218)  hc))
    (is (= (edge (rg) 18)   cl))))

(deftest with-match-3
  (is (not (with-match [hc (eseq (rg) 'localities.HasCapital)
                        :let [b (alpha hc)
                              a (omega hc)]
                        :when (type-of? a 'localities.City)
                        :when (type-of? b 'localities.County)
                        cl (iseq b 'localities.ContainsLocality :out)
                        :when (= (omega cl) a)
                        ;; that's impossible, so body must not be executed
                        :when (= a b hc cl)]
             (println a b hc cl)
             (is false "I must not be executed!")))))

;;* BinTree eval

(defn bin-tree
  []
  (let [g (create-graph
           (load-schema "test/binop-tree-schema.tg" :standard)
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
      (create-edge! 'HasArg a o))
    g))

(defrule replace-binaryop
  "Replaces a binary operation with constant args with
  a constant of the result."
  [g] [b     (vseq g 'BinaryOp)
       :let  [[a1 a2] (vec (--> b 'HasArg))]
       :when (type-of? a1 'Const)
       :when (type-of? a2 'Const)]
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
