(ns funnyqt.in-place.test.tg
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.tg)
  (:use funnyqt.in-place)
  (:use funnyqt.query.tg)
  (:use funnyqt.test.tg)
  (:use [funnyqt.query.test.tg :only (eval-exp)])
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
  [g] [b     (vseq g 'BinaryOp)
       :let  [[a1 a2] (vec (--> b 'HasArg))]
       :when (has-type? a1 'Const)
       :when (has-type? a2 'Const)]
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
