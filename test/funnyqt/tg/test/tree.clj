(ns funnyqt.tg.test.tree
  (:use funnyqt.tg.core)
  (:use funnyqt.match-replace)
  (:use funnyqt.tg.query)
  (:use [funnyqt.utils :only [timing]])
  (:use clojure.test)
  (:import [de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration]))


(defn mintree-init
  "Returns a mintree graph with one Tree vertex."
  []
  (let [g (create-graph (load-schema "test/input/mintree-schema.tg")
                        "MinTree")]
    (create-vertex! g 'Tree)
    g))

(defrule snoc
  "Creates a new Tree at the rightmost child.
  snoc = cons reversed.  So we do like the tree was a singly linked list and
  always add at the end, which is of course a bad idea.
  Forces a complete iteration."
  [g] [t (vseq g)
       :when (not (seq (iseq t 'HasRight :out)))]
  (create-edge! g 'HasRight t (create-vertex! g 'Tree)))

(defrule snoc-recursively
  "Recursive variant of snoc."
  ([g n] [t (vseq g)
          :when (pos? n)
          :when (not (seq (iseq t 'HasRight :out)))]
     (snoc-recursively g n t))
  ([g n t]
     (when (pos? n)
       (let [nt (create-vertex! g 'Tree)]
         (create-edge! g 'HasRight t nt)
         (recur g (dec n) nt)))))

(deftest test-snocs
  (dotimes [i 3]
    (let [g1 (mintree-init)
          g2 (mintree-init)
          n 1000]
      (timing "%s. SNOC SEQ: %Tmilli" (ntimes n snoc g1) (inc i))
      (timing "%s. SNOC REC: %Tmilli" (snoc-recursively g2 n) (inc i))

      (is (== (inc n) (vcount g1) (vcount g2)))
      (is (== n (ecount g1) (ecount g2))))))
