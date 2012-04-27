(ns funnyqt.test.constant-folding-tg
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.tg)
  (:use funnyqt.in-place)
  (:use funnyqt.query.tg)
  (:use [funnyqt.utils :only [error]])
  (:use clojure.test))


(defn binary-eval
  "Returs the result of evaluating b with its Const args c1 and c2."
  [b c1 c2]
  (let [v1 (value c1 :value)
        v2 (value c2 :value)]
    (cond
     (type-of? b 'Add) (+ v1 v2)
     (type-of? b 'Sub) (- v1 v2)
     (type-of? b 'Mul) (* v1 v2)
     (type-of? b 'Div) (quot v1 v2)
     :default (error (format "Unknown binary op: %s" b)))))

(defrule pull-up-consts
  "Pulls up constants to make replace-binary apply."
  [g] [b (vseq g '[Add Mul])
       e1 (filter #(type-of? (that %) 'Const)
                  (iseq b 'Dataflow :out))
       e2 (filter #(= (class %) (class b))
                  (iseq b 'Dataflow :out))
       :let [b2 (omega e2)]
       e3 (filter #(type-of? (that %) '[:and !Block !Const])
                  (iseq b2 'Dataflow :out))]
  ;; Switch edges & set position
  (let [e1p (value e1 :position)
        e3p (value e3 :position)]
    (set-value! (set-alpha! e1 b2) :position e3p)
    (set-value! (set-alpha! e3 b)  :position e1p)))

(defrule replace-not
  "Replates not(1) by 0."
  [g start-block] [n (vseq g 'Not)
                   e (iseq n 'Const :out)
                   :let [c (that e)]]
  (let [new-const (create-vertex! g 'Const)]
    (set-value! new-const
                :value (Integer/valueOf ^Integer (.intValue ^Long (bit-not (value c :value)))))
    (set-value! (create-edge! g 'Dataflow new-const start-block)
                :position (Integer/valueOf -1))
    (relink! n new-const nil :in)
    (delete! n)
    (when-not (first (iseq c nil :in))
      (delete! c))
    true))

(defrule replace-binary
  "Find and replace a Binary connected to two Consts with a Const representing
  the evaluation result."
  [g start-block] [b (vseq g 'Binary)
                   e1 (filter #(type-of? (that %) 'Const)
                              (iseq b 'Dataflow :out))
                   e2 (filter #(type-of? (that %) 'Const)
                              (iseq b 'Dataflow :out))
                   :when (= (value e1 :position) 0)
                   :when (= (value e2 :position) 1)
                   :let [c1 (that e1)
                         c2 (that e2)]]
  (let [new-const (create-vertex! g 'Const)]
    (set-value! new-const :value (Integer/valueOf ^Integer (.intValue ^Long (binary-eval b c1 c2))))
    (set-value! (create-edge! g 'Dataflow new-const start-block)
                :position (Integer/valueOf -1))
    (relink! b new-const nil :in)
    (delete! b)
    ;; Delete unused constants
    (when-not (first (iseq c1 nil :in))
      (delete! c1))
    (when-not (first (iseq c2 nil :in))
      (delete! c2))
    true))

(defn fold-constants
  [g]
  (let [start-block (the (vseq g 'StartBlock))]
    ;; Pull up constants
    (iteratively pull-up-consts g)

    ;; Evaluate Not vertices with a const
    (iteratively replace-not g start-block)

    ;; Evaluate binaries with 2 constants
    (iteratively replace-binary g start-block)))

(def firm1 (memoize #(load-graph "test/input/firm_small_46.tg" :standard)))
(def firm2 (memoize #(load-graph "test/input/firm_medium_1248803056.tg" :standard)))

(deftest constant-folding-1
  (let [g (firm1)]
    (println "constant-folding-1")
    (time (fold-constants g))
    (is (= 16 (+ (vcount g) (ecount g))))
    (is (=  7 (vcount g)))
    (is (=  1 (vcount g 'Const)))
    (is (= 46 (value (the (vseq g 'Const)) :value)))))

(deftest constant-folding-2
  (let [g (firm2)]
    (println "constant-folding-2")
    (time (fold-constants g))
    (is (= 16 (+ (vcount g) (ecount g))))
    (is (=  7 (vcount g)))
    (is (=  1 (vcount g 'Const)))
    (is (= 1248803056 (value (the (vseq g 'Const)) :value)))))

