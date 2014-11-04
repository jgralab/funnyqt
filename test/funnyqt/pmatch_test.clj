(ns funnyqt.pmatch-test
  (:use funnyqt.pmatch)
  (:require [funnyqt.tg        :as tg]
            [funnyqt.emf       :as emf]
            [funnyqt.generic   :as g]
            [funnyqt.query     :as q])
  (:use funnyqt.query)
  (:use clojure.test))

;;# Tests on PMatchTest model/graph

(def pmt-graph (let [g (tg/new-graph (tg/load-schema "test/input/PMatchTestSchema.tg"))
                     b1 (tg/create-vertex! g 'B {:i 1})
                     c1 (tg/create-vertex! g 'C {:i 1})
                     c2 (tg/create-vertex! g 'C {:i 2})
                     a1 (tg/create-vertex! g 'A {:i 1})
                     b2 (tg/create-vertex! g 'B {:i 2})
                     d1 (tg/create-vertex! g 'D {:j 1})
                     d2 (tg/create-vertex! g 'D {:j 2})]
                 (doto g
                   (tg/create-edge! 'A2A b1 c1)
                   (tg/create-edge! 'A2A b1 c2)
                   (tg/create-edge! 'A2A c1 c1)
                   (tg/create-edge! 'A2A c1 a1)
                   (tg/create-edge! 'A2A a1 b2)
                   (tg/create-edge! 'A2A c2 a1)
                   (tg/create-edge! 'A2A c2 b2)
                   (tg/create-edge! 'A2D c1 d1)
                   (tg/create-edge! 'A2D a1 d1)
                   (tg/create-edge! 'A2D c2 d2))))

(emf/load-ecore-resource "test/input/PMatchTestMetamodel.ecore")
(def pmt-model (let [m (emf/new-resource)
                     b1 (emf/ecreate! m 'B {:i 1})
                     c1 (emf/ecreate! m 'C {:i 1})
                     c2 (emf/ecreate! m 'C {:i 2})
                     a1 (emf/ecreate! m 'A {:i 1})
                     b2 (emf/ecreate! m 'B {:i 2})
                     d1 (emf/ecreate! m 'D {:j 1})
                     d2 (emf/ecreate! m 'D {:j 2})]
                 (g/add-adjs! b1 :t [c1 c2])
                 (g/add-adjs! c1 :t [c1 a1])
                 (g/add-adjs! a1 :t [b2])
                 (g/add-adjs! c2 :t [a1 b2])
                 (g/add-adjs! c1 :d [d1])
                 (g/add-adjs! a1 :d [d1])
                 (g/add-adjs! c2 :d [d2])
                 m))

(defn pmt-el [model cls val]
  (q/the #(= (g/aval % (if (= 'D cls) :j :i)) val)
         (g/elements model (symbol (str (name cls) "!")))))

(defn pmt-matches [model & matches]
  (for [m matches]
    (cond
     (and (vector? m)
          (= 2 (count m))
          (symbol? (first m))
          (integer? (second m)))  (apply pmt-el model m)
     (map? m)     (zipmap (keys m)
                          (mapcat (partial pmt-matches model) (vals m)))
     (coll? m)    (into (empty m) (let [r (mapcat (partial pmt-matches model) m)]
                                    (if (vector? m)
                                      r
                                      (reverse r))))
     :else m)))

(defn pmt-matches-fn [& matches]
  (fn [model]
    (apply pmt-matches model matches)))

(defn pmt-assert [p-gen p-emf p-tg & [match-count matches-fn]]
  (let [r-gen-emf (p-gen pmt-model)
        r-gen-tg  (p-gen pmt-graph)
        r-emf     (p-emf pmt-model)
        r-tg      (when p-tg (p-tg pmt-graph))
        match-count (or match-count (count r-gen-emf))]
    (is (= match-count (count r-gen-emf) (count r-gen-tg) (count r-emf)))
    (when p-tg
      (is (= match-count (count r-tg))))
    (if matches-fn
      (do
        (is (= (set (matches-fn pmt-model)) (set r-emf) (set r-gen-emf)))
        (is (= (set (matches-fn pmt-graph)) (set r-gen-tg)))
        (when p-tg
          (is (= (set (matches-fn pmt-graph)) (set r-tg)))))
      (do
        (is (= (set r-gen-emf) (set r-emf)))
        (when p-tg
          (is (= (set r-gen-tg) (set r-tg))))))))

;; NOTE: We can't always test the :tg expansion here, because with :emf and
;; :generic [a --> b] means "a references b somehow" whereas the meaning for
;; :tg is "there is an edge starting at a and ending at b", i.e., there the
;; edge direction is considered, thus there are fewer matches.

(deftest test-pmt
  (testing "Testing pattern [a<A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A>])
                5
                (pmt-matches-fn {:a ['B 1]} {:a ['C 1]} {:a ['C 2]} {:a ['A 1]} {:a ['B 2]})))
  (testing "Testing pattern [x<!A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [x<!A>])
                (pattern {:pattern-expansion-context :emf}     [m] [x<!A>])
                (pattern {:pattern-expansion-context :tg}      [m] [x<!A>])
                2
                (pmt-matches-fn {:x ['D 1]} {:x ['D 2]})))
  (testing "Testing pattern [a<A!>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A!>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A!>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A!>])
                1
                (pmt-matches-fn {:a ['A 1]})))
  (testing "Testing pattern [a<!A!>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<!A!>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<!A!>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<!A!>])
                6
                (pmt-matches-fn {:a ['B 1]} {:a ['C 1]} {:a ['C 2]}
                                {:a ['B 2]} {:a ['D 1]} {:a ['D 2]})))
  (testing "Testing pattern [x]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [x])
                (pattern {:pattern-expansion-context :emf}     [m] [x])
                (pattern {:pattern-expansion-context :tg}      [m] [x])
                7
                (pmt-matches-fn {:x ['B 1]} {:x ['C 1]} {:x ['C 2]} {:x ['A 1]} {:x ['B 2]}
                                {:x ['D 1]} {:x ['D 2]})))
  (testing "Testing pattern [x<>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [x<>])
                (pattern {:pattern-expansion-context :emf}     [m] [x<>])
                (pattern {:pattern-expansion-context :tg}      [m] [x<>])
                7
                (pmt-matches-fn {:x ['B 1]} {:x ['C 1]} {:x ['C 2]} {:x ['A 1]} {:x ['B 2]}
                                {:x ['D 1]} {:x ['D 2]})))
  (testing "Testing pattern [c<C> -<:t>-> a<A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> -<:t>-> a<A>])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> -<:t>-> a<A>])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> -<:t>-> a<A>])
                4
                (pmt-matches-fn {:c ['C 1], :a ['C 1]}
                                {:c ['C 1], :a ['A 1]}
                                {:c ['C 2], :a ['A 1]}
                                {:c ['C 2], :a ['B 2]})))
  (testing "Testing pattern [c<C> -<:t>-> a<A> :isomorphic]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> -<:t>-> a<A> :isomorphic])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> -<:t>-> a<A> :isomorphic])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> -<:t>-> a<A> :isomorphic])
                3
                (pmt-matches-fn {:c ['C 1], :a ['A 1]}
                                {:c ['C 2], :a ['A 1]}
                                {:c ['C 2], :a ['B 2]})))
  (testing "Testing pattern [c<C> --> a<A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> --> a<A>])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> --> a<A>])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> --> a<A>])
                7
                (pmt-matches-fn {:c ['C 1], :a ['C 1]}
                                {:c ['C 1], :a ['A 1]}
                                {:c ['C 1], :a ['B 1]}
                                {:c ['C 1], :a ['C 1]}
                                {:c ['C 2], :a ['B 2]}
                                {:c ['C 2], :a ['B 1]}
                                {:c ['C 2], :a ['A 1]})))
  (testing "Testing pattern [c<C> --> a<A> :isomorphic]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> --> a<A> :isomorphic])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> --> a<A> :isomorphic])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> --> a<A> :isomorphic])
                5
                (pmt-matches-fn {:c ['C 1], :a ['A 1]}
                                {:c ['C 1], :a ['B 1]}
                                {:c ['C 2], :a ['B 2]}
                                {:c ['C 2], :a ['B 1]}
                                {:c ['C 2], :a ['A 1]})))
  (testing "Testing pattern [c<C> -<>-> a<A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> -<>-> a<A>])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> -<>-> a<A>])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> -<>-> a<A>])
                7
                (pmt-matches-fn {:c ['C 1], :a ['C 1]}
                                {:c ['C 1], :a ['A 1]}
                                {:c ['C 1], :a ['B 1]}
                                {:c ['C 1], :a ['C 1]}
                                {:c ['C 2], :a ['B 2]}
                                {:c ['C 2], :a ['B 1]}
                                {:c ['C 2], :a ['A 1]})))
  (testing "Testing pattern with non-model-args (1)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1)))
                1
                (pmt-matches-fn {:b ['B 2], :a ['A 1]})))
  (testing "Testing pattern with non-model-args (2)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1)))
                0
                (pmt-matches-fn)))
  (testing "Testing pattern with non-model-args (3)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1) (pmt-el m 'B 2)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1) (pmt-el m 'B 2)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'A 1) (pmt-el m 'B 2)))
                1
                (pmt-matches-fn {:b ['B 2], :a ['A 1]})))
  (testing "Testing pattern with non-model-args (4)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1) (pmt-el m 'B 2)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1) (pmt-el m 'B 2)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a b]
                    [a<A!> -<:t>-> b<B>])
                   m (pmt-el m 'C 1) (pmt-el m 'B 2)))
                0
                (pmt-matches-fn)))
  (testing "Testing pattern with non-model-args (5)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a]
                    [a<A> --> a])
                   m (pmt-el m 'C 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a]
                    [a<A> --> a])
                   m (pmt-el m 'C 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a]
                    [a<A> --> a])
                   m (pmt-el m 'C 1)))
                2
                (pmt-matches-fn {:a ['C 1]} {:a ['C 1]})))
  (testing "Testing pattern with non-model-args (6)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'A 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'A 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'A 1)))
                1
                (pmt-matches-fn {:b ['B 2], :a ['A 1]})))
  (testing "Testing pattern with non-model-args (7)"
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'B 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'B 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m a]
                    [b<B> -<:s>-> a<A!>])
                   m (pmt-el m 'B 1)))
                0
                (pmt-matches-fn)))
  (testing "Testing pattern [b<B> -<:t>-> <C> -<:t>-> <> -<:t>-> a<A>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [b<B> -<:t>-> <C> -<:t>-> <> -<:t>-> a<A>])
                (pattern {:pattern-expansion-context :emf}     [m] [b<B> -<:t>-> <C> -<:t>-> <> -<:t>-> a<A>])
                (pattern {:pattern-expansion-context :tg}      [m] [b<B> -<:t>-> <C> -<:t>-> <> -<:t>-> a<A>])
                4
                (pmt-matches-fn {:b ['B 1], :a ['C 1]}
                                {:b ['B 1], :a ['A 1]}
                                {:b ['B 1], :a ['B 2]}
                                {:b ['B 1], :a ['B 2]})))
  (testing "Testing pattern [a<A> -!<:d>-> <>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A> -!<:d>-> <>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A> -!<:d>-> <>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A> -!<:d>-> <>])
                2
                (pmt-matches-fn {:a ['B 1]} {:a ['B 2]})))
  (testing "Testing pattern [a<A> -!<:t>-> <C>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A> -!<:t>-> <C>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A> -!<:t>-> <C>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A> -!<:t>-> <C>])
                3
                (pmt-matches-fn {:a ['A 1]} {:a ['B 2]} {:a ['C 2]})))
  (testing "Testing pattern [a1<A> -!-> a2<A!>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a1<A> -!-> a2<A!>])
                (pattern {:pattern-expansion-context :emf}     [m] [a1<A> -!-> a2<A!>])
                (pattern {:pattern-expansion-context :tg}      [m] [a1<A> -!-> a2<A!>])
                2
                (pmt-matches-fn {:a1 ['A 1] :a2 ['A 1]}
                                {:a1 ['B 1] :a2 ['A 1]})))
  (testing "Testing pattern [a<A> --> a]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A> --> a])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A> --> a])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A> --> a])
                2
                (pmt-matches-fn {:a ['C 1]}
                                {:a ['C 1]})))
  (testing "Testing pattern [c<C> --> a<A>
                             :when (= 1 (g/aval a :i))
                             :when (= 1 (g/aval c :i))]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> --> a<A>
                                                                    :when (= 1 (g/aval a :i))
                                                                    :when (= 1 (g/aval c :i))])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> --> a<A>
                                                                    :when (= 1 (g/aval a :i))
                                                                    :when (= 1 (g/aval c :i))])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> --> a<A>
                                                                    :when (= 1 (g/aval a :i))
                                                                    :when (= 1 (g/aval c :i))])
                4
                (pmt-matches-fn {:c ['C 1], :a ['C 1]}
                                {:c ['C 1], :a ['A 1]}
                                {:c ['C 1], :a ['B 1]}
                                {:c ['C 1], :a ['C 1]})))
  (testing "Testing pattern [a<A> -<:t>-> c1<C>
                             a    -<:t>-> c2<C>
                             :when (not= c1 c2)
                             a    -!<:s>-> <>]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [a<A> -<:t>-> c1<C>
                                                                    a    -<:t>-> c2<C>
                                                                    :when (not= c1 c2)
                                                                    a    -!<:s>-> <>])
                (pattern {:pattern-expansion-context :emf}     [m] [a<A> -<:t>-> c1<C>
                                                                    a    -<:t>-> c2<C>
                                                                    :when (not= c1 c2)
                                                                    a    -!<:s>-> <>])
                (pattern {:pattern-expansion-context :tg}      [m] [a<A> -<:t>-> c1<C>
                                                                    a    -<:t>-> c2<C>
                                                                    :when (not= c1 c2)
                                                                    a    -!<:s>-> <>])
                2
                (pmt-matches-fn {:a ['B 1], :c1 ['C 1], :c2 ['C 2]}
                                {:a ['B 1], :c1 ['C 2], :c2 ['C 1]})))
  (testing "Testing pattern [c<C> --> d<D>
                             :let [i (g/aval c :i)
                                   j (g/aval d :j)]]"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> --> d<D>
                                                                    :let [i (g/aval c :i)
                                                                          j (g/aval d :j)]])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> --> d<D>
                                                                    :let [i (g/aval c :i)
                                                                          j (g/aval d :j)]])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> --> d<D>
                                                                    :let [i (g/aval c :i)
                                                                          j (g/aval d :j)]])
                2
                (pmt-matches-fn {:c ['C 1] :d ['D 1] :i 1 :j 1}
                                {:c ['C 2] :d ['D 2] :i 2 :j 2})))
  (testing "Testing iterated bindings with :for (1)"
    (letpattern [(a-having-d-generic
                  {:pattern-expansion-context :generic}
                  ([m]
                     [a<A> -<:d>-> d<D>])
                  ([m a d]
                     [a<A> -<:d>-> d<D>]))
                 (a-having-d-emf
                  {:pattern-expansion-context :emf}
                  ([m]
                     [a<A> -<:d>-> d<D>])
                  ([m a d]
                     [a<A> -<:d>-> d<D>]))
                 (a-having-d-tg
                  {:pattern-expansion-context :tg}
                  ([m]
                     [a<A> -<:d>-> d<D>])
                  ([m a d]
                     [a<A> -<:d>-> d<D>]))
                 (a-with-a-having-d-generic
                  {:pattern-expansion-context :generic}
                  [m]
                  [:for [{a1 :a, d :d} (a-having-d-generic m)]
                   a1 -<:t>-> a2<A>
                   :when (seq (a-having-d-generic m a2 d))])
                 (a-with-a-having-d-emf
                  {:pattern-expansion-context :emf}
                  [m]
                  [:for [{a1 :a, d :d} (a-having-d-emf m)]
                   a1 -<:t>-> a2<A>
                   :when (seq (a-having-d-emf m a2 d))])
                 (a-with-a-having-d-tg
                  {:pattern-expansion-context :tg}
                  [m]
                  [:for [{a1 :a, d :d} (a-having-d-tg m)]
                   a1 -<:t>-> a2<A>
                   :when (seq (a-having-d-tg m a2 d))])]
      (pmt-assert a-with-a-having-d-generic
                  a-with-a-having-d-emf
                  a-with-a-having-d-tg
                  2
                  (pmt-matches-fn {:a1 ['C 1], :a2 ['A 1], :d ['D 1]}
                                  {:a1 ['C 1], :a2 ['C 1], :d ['D 1]}))))
  (testing "Testing iterated bindings with :for (2)"
    (letpattern [(c-with-d-generic {:pattern-expansion-context :generic}
                                   [m]
                                   [c<C> -<:d>-> d<D>
                                    :for [follower (p-+ c :t)]])
                 (c-with-d-emf {:pattern-expansion-context :emf}
                               [m]
                               [c<C> -<:d>-> d<D>
                                :for [follower (p-+ c :t)]])
                 (c-with-d-tg {:pattern-expansion-context :tg}
                              [m]
                              [c<C> -<:d>-> d<D>
                               :for [follower (p-+ c :t)]])]
      (pmt-assert c-with-d-generic
                  c-with-d-emf
                  c-with-d-tg
                  5
                  (pmt-matches-fn {:c ['C 1] :d ['D 1] :follower ['C 1]}
                                  {:c ['C 1] :d ['D 1] :follower ['A 1]}
                                  {:c ['C 1] :d ['D 1] :follower ['B 2]}
                                  {:c ['C 2] :d ['D 2] :follower ['A 1]}
                                  {:c ['C 2] :d ['D 2] :follower ['B 2]}))))
  (testing "Testing pattern composition with :extends"
    (letpattern [(a-A [m] [a<A>])
                 (a-having-d-generic
                  {:pattern-expansion-context :generic}
                  ;; Each include may be a symbol, a list of only a symbol, a
                  ;; list (symbol num), a list (symbol & renames), or a list
                  ;; (symbol num & renames).
                  ([m]
                     [:extends [(a-A 0)]
                      a -<:d>-> d<D>])
                  ([m a d]
                     [:extends [a-A]
                      a -<:d>-> d<D>]))
                 (a-having-d-emf
                  {:pattern-expansion-context :emf}
                  ([m]
                     [:extends [a-A]
                      a -<:d>-> d<D>])
                  ([m a d]
                     [:extends [(a-A)]
                      a -<:d>-> d<D>]))
                 ;; Use edge types for TG since this will navigate pattern
                 ;; edges backwards.  This ensures a larger test coverage.
                 (a-having-d-tg
                  {:pattern-expansion-context :tg}
                  ([m]
                     [:extends [a-A]
                      a -<:d>-> d<D>])
                  ([m a d]
                     [:extends [(a-A)]
                      a -<:d>-> d<D>]))
                 (a-with-a-having-d-generic
                  {:pattern-expansion-context :generic}
                  [m]
                  ;; The patten number is optional.  With a-having-d-* both
                  ;; versions 0 and 1 are identical, so it doesn't matter what
                  ;; to chose.
                  [:extends [(a-having-d-generic :a a1)
                             (a-having-d-generic 1 :a a2)]
                   a1 -<:t>-> a2])
                 (a-with-a-having-d-emf
                  {:pattern-expansion-context :emf}
                  [m]
                  [:extends [(a-having-d-emf :a a1)
                             (a-having-d-emf 1 :a a2)]
                   a1 -<:t>-> a2])
                 ;; Use edge types for TG since this will navigate pattern
                 ;; edges backwards.  This ensures a larger test coverage.
                 (a-with-a-having-d-tg
                  {:pattern-expansion-context :tg}
                  [m]
                  [:extends [(a-having-d-tg 0 :a a1)
                             (a-having-d-tg :a a2)]
                   a1 -<:t>-> a2])]
      (pmt-assert a-with-a-having-d-generic
                  a-with-a-having-d-emf
                  a-with-a-having-d-tg
                  2
                  (pmt-matches-fn {:a1 ['C 1], :a2 ['A 1], :d ['D 1]}
                                  {:a1 ['C 1], :a2 ['C 1], :d ['D 1]}))))
  (testing "Testing patterns with arguments."
    (pmt-assert (fn [m]
                  ((pattern
                    {:pattern-expansion-context :generic}
                    [m cur]
                    [cur<A> -<:t>-> next<A>])
                   m (pmt-el m 'B 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :emf}
                    [m cur]
                    [cur<A> -<:t>-> next<A>])
                   m (pmt-el m 'B 1)))
                (fn [m]
                  ((pattern
                    {:pattern-expansion-context :tg}
                    [m cur]
                    [cur<A> -<:t>-> next<A>])
                   m (pmt-el m 'B 1)))
                2
                (pmt-matches-fn {:cur ['B 1] :next ['C 1]}
                                {:cur ['B 1] :next ['C 2]})))
  (testing "Testing recursive patterns."
    (letpattern [(successors-generic
                  {:pattern-expansion-context :generic}
                  [m cur known]
                  [:when (not (known cur))
                   cur<A> -<:t>-> next<A>
                   :let [nnexts (successors-generic m next (conj known cur))]])
                 (successors-emf
                  {:pattern-expansion-context :emf}
                  [m cur known]
                  [:when (not (known cur))
                   cur<A> -<:t>-> next<A>
                   :let [nnexts (successors-emf m next (conj known cur))]])
                 (successors-tg
                  {:pattern-expansion-context :tg}
                  [m cur known]
                  [:when (not (known cur))
                   cur<A> -<:t>-> next<A>
                   :let [nnexts (successors-tg m next (conj known cur))]])]
      (pmt-assert (fn [m]
                    (successors-generic m (pmt-el m 'B 1) #{}))
                  (fn [m]
                    (successors-emf m (pmt-el m 'B 1) #{}))
                  (fn [m]
                    (successors-tg m (pmt-el m 'B 1) #{}))
                  2
                  (pmt-matches-fn
                   '{:cur [B 1],
                     :next [C 1],
                     :nnexts ({:cur [C 1],
                               :next [C 1],
                               :nnexts ()}
                              {:cur [C 1],
                               :next [A 1],
                               :nnexts ({:cur [A 1],
                                         :next [B 2],
                                         :nnexts ()})})}
                   '{:cur [B 1],
                     :next [C 2],
                     :nnexts ({:cur [C 2],
                               :next [A 1],
                               :nnexts ({:cur [A 1],
                                         :next [B 2],
                                         :nnexts ()})}
                              {:cur [C 2],
                               :next [B 2],
                               :nnexts ()})}))))
  (testing "Testing :negative patterns. (1)"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> a<A>
                                         b -<:t>-> c2<C> -<:t>-> a
                                         :isomorphic]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> a<A>
                                         b -<:t>-> c2<C> -<:t>-> a
                                         :isomorphic]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> a<A>
                                         b -<:t>-> c2<C> -<:t>-> a
                                         :isomorphic]])
                1
                (pmt-matches-fn {:b ['B 2]})))
  (testing "Testing :negative patterns. (2)"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> <A>
                                         -<:s>-> c2<C> -<:s>-> b
                                         :isomorphic]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> <A>
                                         -<:s>-> c2<C> -<:s>-> b
                                         :isomorphic]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [b<B>
                              :negative [b -<:t>-> c1<C> -<:t>-> <A>
                                         -<:s>-> c2<C> -<:s>-> b
                                         :isomorphic]])
                1
                (pmt-matches-fn {:b ['B 2]})))
  (testing "Testing :negative patterns. (3, global NAC)"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [a<A>
                              :negative [c1<C> -<:d>-> d1<D>
                                         c1 -<:t>-> a1<A>
                                         a1 -<:d>-> d1]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [a<A>
                              :negative [c1<C> -<:d>-> d1<D>
                                         c1 -<:t>-> a1<A>
                                         a1 -<:d>-> d1]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [a<A>
                              :negative [c1<C> -<:d>-> d1<D>
                                         c1 -<:t>-> a1<A>
                                         a1 -<:d>-> d1]])
                0
                (pmt-matches-fn)))
  (testing "Testing :nested patterns. (1)"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [c<C>
                              :nested [ds [c -<:d>-> d :as d]
                                       ss [c -<:s>-> s :as s]
                                       ts [c -<:t>-> t :as t]]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [c<C>
                              :nested [ds [c -<:d>-> d :as d]
                                       ss [c -<:s>-> s :as s]
                                       ts [c -<:t>-> t :as t]]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [c<C>
                              :nested [ds [c -<:d>-> d :as d]
                                       ss [c -<:s>-> s :as s]
                                       ts [c -<:t>-> t :as t]]])
                2
                (pmt-matches-fn
                 {:c ['C 1], :ds (list ['D 1]), :ss (list ['B 1] ['C 1]), :ts (list ['C 1] ['A 1])}
                 {:c ['C 2], :ds (list ['D 2]), :ss (list ['B 1]), :ts (list ['A 1] ['B 2])})))
  (testing "Testing :nested patterns. (2)"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [a<A> -<:d>-> d<D>
                              :nested [f1 [a -<:t>-> a1
                                           :nested [f2 [a1 -<:t>-> a2 :as a2]]]]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [a<A> -<:d>-> d<D>
                              :nested [f1 [a -<:t>-> a1
                                           :nested [f2 [a1 -<:t>-> a2 :as a2]]]]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [a<A> -<:d>-> d<D>
                              :nested [f1 [a -<:t>-> a1
                                           :nested [f2 [a1 -<:t>-> a2 :as a2]]]]])
                3
                (pmt-matches-fn
                 {:a ['C 1] :d ['D 1] :f1 (list {:a1 ['C 1] :f2 (list ['C 1] ['A 1])}
                                                {:a1 ['A 1] :f2 (list ['B 2])})}
                 {:a ['C 2] :d ['D 2] :f1 (list {:a1 ['A 1] :f2 (list ['B 2])}
                                                {:a1 ['B 2] :f2 ()})}
                 {:a ['A 1] :d ['D 1] :f1 (list {:a1 ['B 2] :f2 ()})})))
  (testing "Testing :as clause"
    (pmt-assert (pattern {:pattern-expansion-context :generic}
                         [m] [c<C> --> d<D>
                              :let [i (g/aval c :i)
                                    j (g/aval d :j)]
                              :as [c d i j]])
                (pattern {:pattern-expansion-context :emf}
                         [m] [c<C> --> d<D>
                              :let [i (g/aval c :i)
                                    j (g/aval d :j)]
                              :as [c d i j]])
                (pattern {:pattern-expansion-context :tg}
                         [m] [c<C> --> d<D>
                              :let [i (g/aval c :i)
                                    j (g/aval d :j)]
                              :as [c d i j]])
                2
                (pmt-matches-fn (list ['C 1] ['D 1] 1 1)
                                (list ['C 2] ['D 2] 2 2))))
  (testing "Testing :distinct clause"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [c<C> --> a<A> :distinct])
                (pattern {:pattern-expansion-context :emf}     [m] [c<C> --> a<A> :distinct])
                (pattern {:pattern-expansion-context :tg}      [m] [c<C> --> a<A> :distinct])
                6
                (pmt-matches-fn {:c ['C 1], :a ['C 1]}
                                {:c ['C 1], :a ['A 1]}
                                {:c ['C 1], :a ['B 1]}
                                {:c ['C 2], :a ['B 2]}
                                {:c ['C 2], :a ['B 1]}
                                {:c ['C 2], :a ['A 1]})))
  (testing "Testing :distinct with :as"
    (pmt-assert (pattern {:pattern-expansion-context :generic} [m] [n1 --> n2
                                                                    :when (not= n1 n2)
                                                                    :as #{n1 n2} :distinct])
                (pattern {:pattern-expansion-context :emf}     [m] [n1 --> n2
                                                                    :when (not= n1 n2)
                                                                    :as #{n1 n2} :distinct])
                (pattern {:pattern-expansion-context :tg}      [m] [n1 --> n2
                                                                    :when (not= n1 n2)
                                                                    :as #{n1 n2} :distinct])
                9
                (pmt-matches-fn #{['B 1] ['C 1]}
                                #{['C 1] ['A 1]}
                                #{['C 1] ['D 1]}
                                #{['A 1] ['B 2]}
                                #{['A 1] ['D 1]}
                                #{['B 1] ['C 2]}
                                #{['C 2] ['A 1]}
                                #{['C 2] ['B 2]}
                                #{['C 2] ['D 2]})))
  (testing "Testing example from impl chapter."
    (pmt-assert (pattern
                 {:pattern-expansion-context :generic}
                 [m]
                 [a1<A> -<:t>-> a2<A>
                  a1 -<:d>-> d
                  a2 -!<:d>-> <>
                  :nested [a3s [a2 -<:t>-> a3 :as a3]]])
                (pattern
                 {:pattern-expansion-context :emf}
                 [m]
                 [a1<A> -<:t>-> a2<A>
                  a1 -<:d>-> d
                  a2 -!<:d>-> <>
                  :nested [a3s [a2 -<:t>-> a3 :as a3]]])
                (pattern
                 {:pattern-expansion-context :tg}
                 [m]
                 [a1<A> -<:t>-> a2<A>
                  a1 -<:d>-> d
                  a2 -!<:d>-> <>
                  :nested [a3s [a2 -<:t>-> a3 :as a3]]])
                2
                (pmt-matches-fn {:a1 ['A 1] :a2 ['B 2] :d ['D 1] :a3s ()}
                                {:a1 ['C 2] :a2 ['B 2] :d ['D 2] :a3s ()}))))


;;# TG

(def fg (tg/load-graph "test/input/familygraph.tg"))

(defpattern families-with-fathers-simple-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> -hf<HasFather>-> m<Member>])

(defpattern families-with-fathers-simple-roles-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> -hf<:father>-> m<Member>])

(deftest test-families-with-fathers-simple-tg
  (is (= 3 (count (families-with-fathers-simple-tg fg))))
  (is (= 3 (count (families-with-fathers-simple-roles-tg fg))))
  (is (= (families-with-fathers-simple-tg fg)
         (families-with-fathers-simple-roles-tg fg))))

(defpattern families-with-fathers-tg
  {:pattern-expansion-context :tg}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(defpattern families-with-fathers-forall-tg
  {:pattern-expansion-context :tg
   :forall true}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(defpattern families-with-fathers-forall-distinct-tg
  {:pattern-expansion-context :tg
   :forall true
   :distinct true}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-tg
  (is (= 3 (count (families-with-fathers-tg fg))))
  (is (= 3 (count (families-with-fathers-forall-tg fg))))
  (is (= 3 (count (families-with-fathers-forall-distinct-tg fg))))
  (is (= 3 (count (families-with-fathers-tg fg (constantly true)))))
  (is (= 2 (count (families-with-fathers-tg fg #(= "Smith" (tg/value % :lastName)))))))

(defpattern same-family-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-tg
  (is (= 62
         (* 2 (count (same-family-distinct-tg fg)))
         (count (same-family-tg fg)))))

(defpattern given-fam-with-all-members-tg
  {:pattern-expansion-context :tg}
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-tg
  (let [fsmith (tg/vertex fg 12)
        r (given-fam-with-all-members-tg fg fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (:fam %)) r))))

(defpattern long-anon-pattern-tg
  {:pattern-expansion-context :tg}
  [g fam]
  [fam --> <Member> <-<_>- <> -<HasSon>-> <> <-<_>- x<Family>
   :when (not= fam x)])

(deftest test-long-anon-pattern-tg
  (let [fsmith (tg/vertex fg 12)
        r (long-anon-pattern-tg fg fsmith)]
    (is (= 3 (count r)))
    (is (= r [{:fam fsmith, :x (tg/vertex fg 1)}
              {:fam fsmith, :x (tg/vertex fg 1)}
              {:fam fsmith, :x (tg/vertex fg 1)}]))))

(deftest test-letpattern-tg
  (letpattern [(families-with-fathers-simple [g]
                                             [f<Family> -hf<HasFather>-> m<Member>])
               (families-with-fathers
                ([g]
                   [f<Family> -hf<HasFather>-> m<Member>])
                ([g famconst]
                   [f<Family> -hf<HasFather>-> m<Member>
                    :when (famconst f)
                    :as [f hf m]]))]
    {:pattern-expansion-context :tg}
    (is (= 3 (count (families-with-fathers-simple fg))))
    (is (= 3 (count (families-with-fathers fg))))
    (is (= 3 (count (families-with-fathers fg (constantly true)))))
    (is (= 2 (count (families-with-fathers fg #(= "Smith" (tg/value % :lastName))))))))

(deftest test-pattern-tg
  (let [families-with-fathers-simple (pattern {:pattern-expansion-context :tg} [g]
                                              [f<Family> -hf<HasFather>-> m<Member>])
        families-with-fathers (pattern {:pattern-expansion-context :tg}
                                       ([g]
                                          [f<Family> -hf<HasFather>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -hf<HasFather>-> m<Member>
                                           :when (famconst f)
                                           :as [f hf m]]))]
    (is (= 3 (count (families-with-fathers-simple fg))))
    (is (= 3 (count (families-with-fathers fg))))
    (is (= 3 (count (families-with-fathers fg (constantly true)))))
    (is (= 2 (count (families-with-fathers fg #(= "Smith" (tg/value % :lastName))))))))

(deftest test-eager-pattern-tg
  (let [lazy-pattern1 (pattern {:pattern-expansion-context :tg} [g]
                               [f<Family> -hf<HasFather>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :tg, :eager true} [g]
                                [f<Family> -hf<HasFather>-> m<Member>])
        lazy-pattern2 (pattern {:pattern-expansion-context :tg} [g]
                               [m1<Member> <-<_>- <> --> m2<Member>
                                :when (distinct? m1 m2)
                                :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:pattern-expansion-context :tg, :eager true} [g]
                                [m1<Member> <-<_>- <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fg) (eager-pattern1 fg)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fg)) (set (eager-pattern2 fg))))))

;;# EMF

(emf/load-ecore-resource "test/input/Families.ecore")
(def fm (emf/load-resource "test/input/example.families"))

(defpattern families-with-fathers-simple-emf
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> -<:father>-> m<Member>])

(deftest test-families-with-fathers-simple-emf
  (is (= 3 (count (families-with-fathers-simple-emf fm)))))

(defpattern families-with-fathers-emf
  {:pattern-expansion-context :emf}
  ([g]
     [f<Family> -<:father>-> m<Member>])
  ([g famconst]
     [f<Family> -<:father>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-emf
  (is (= 3 (count (families-with-fathers-emf fm))))
  (is (= 3 (count (families-with-fathers-emf fm (constantly true)))))
  (is (= 2 (count (families-with-fathers-emf fm #(= "Smith" (emf/eget % :lastName)))))))

(defpattern same-family-emf
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-emf
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-emf
  (is (= 62
         (* 2 (count (same-family-distinct-emf fm)))
         (count (same-family-emf fm)))))

(defpattern given-fam-with-all-members-emf
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-emf
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        r (given-fam-with-all-members-emf fm fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (:fam %)) r))))

(defpattern long-anon-pattern-emf
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> <Member> --> <Family> -<:sons>-> <> --> x<Family>
   :when (not= fam x)
   :as [(emf/eget x :lastName) (emf/eget x :street)]])

(deftest test-long-anon-pattern-emf
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        ofsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                          (= "Smith Avenue 4" (emf/eget f :street))))
                             (emf/eallcontents fm 'Family)))
        r (long-anon-pattern-emf fm fsmith)]
    (is (= 3 (count r)))
    (is (= r [["Smith" "Smith Avenue 4"] ["Smith" "Smith Avenue 4"] ["Smith" "Smith Avenue 4"]]))))

(deftest test-letpattern-emf
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -<:father>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -<:father>-> m<Member>])
                 ([g famconst]
                    [f<Family> -<:father>-> m<Member>
                     :when (famconst f)
                     :as [f m]]))]
    {:pattern-expansion-context :emf}
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-pattern-emf
  (let [families-with-fathers-simple (pattern {:pattern-expansion-context :emf}
                                              [g]
                                              [f<Family> -<:father>-> m<Member>])
        families-with-fathers (pattern {:pattern-expansion-context :emf}
                                       ([g]
                                          [f<Family> -<:father>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -<:father>-> m<Member>
                                           :when (famconst f)
                                           :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-eager-pattern-emf
  (let [lazy-pattern1 (pattern {:pattern-expansion-context :emf} [g]
                              [f<Family> -<:father>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :emf, :eager true} [g]
                                [f<Family> -<:father>-> m<Member>])
        lazy-pattern2 (pattern {:pattern-expansion-context :emf} [g]
                               [m1<Member> --> <> --> m2<Member>
                                :when (distinct? m1 m2)
                                :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:pattern-expansion-context :emf, :eager true} [g]
                                [m1<Member> --> <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fm) (eager-pattern1 fm)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fm)) (set (eager-pattern2 fm))))))

;;# Generic

(defpattern families-with-fathers-simple-generic
  [g]
  [f<Family> -<:father>-> m<Member>])

(defpattern families-with-fathers-simple-kw-generic
  [g]
  [f<Family> -<:father>-> m<Member>])

(deftest test-families-with-fathers-simple-generic
  (is (= 3
         (count (families-with-fathers-simple-generic fm))
         (count (families-with-fathers-simple-kw-generic fm))))
  (is (= (families-with-fathers-simple-generic fm)
         (families-with-fathers-simple-kw-generic fm))))

(defpattern families-with-fathers-generic
  ([g]
     [f<Family> -<:father>-> m<Member>])
  ([g famconst]
     [f<Family> -<:father>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-generic
  (is (= 3 (count (families-with-fathers-generic fm))))
  (is (= 3 (count (families-with-fathers-generic fm (constantly true)))))
  (is (= 2 (count (families-with-fathers-generic fm #(= "Smith" (emf/eget % :lastName)))))))

(defpattern same-family-generic
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-generic
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-generic
  (is (= 62
         (* 2 (count (same-family-distinct-generic fm)))
         (count (same-family-generic fm)))))

(defpattern given-fam-with-all-members-generic
  [g fam]
  [fam --> mem<Member>
   ;; Test that the special :map result form works
   :as :map])

(deftest test-given-fam-with-all-members-generic
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        r (given-fam-with-all-members-generic fm fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (:fam %)) r))))

(defpattern long-anon-pattern-generic
  [g fam]
  [fam --> <Member> --> <Family> -<:sons>-> <> --> x<Family>
   :when (not= fam x)
   ;; Test that the special :vector result form works
   :as :vector])

(deftest test-long-anon-pattern-generic
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        ofsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                          (= "Smith Avenue 4" (emf/eget f :street))))
                             (emf/eallcontents fm 'Family)))
        r (long-anon-pattern-generic fm fsmith)]
    (is (= 3 (count r)))
    (is (= r [[fsmith ofsmith] [fsmith ofsmith] [fsmith ofsmith]]))))

(deftest test-letpattern-generic
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -<:father>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -<:father>-> m<Member>])
                 ([g famconst]
                    [f<Family> -<:father>-> m<Member>
                     :when (famconst f)
                     :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (g/aval % :lastName))))))))

(deftest test-pattern-generic
  (let [families-with-fathers-simple (pattern [g] [f<Family> -<:father>-> m<Member>])
        families-with-fathers (pattern ([g]
                                          [f<Family> -<:father>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -<:father>-> m<Member>
                                           :when (famconst f)
                                           :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-eager-pattern-generic
  (let [lazy-pattern1 (pattern [g]
                               [f<Family> -<:father>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :generic, :eager true} [g]
                                [f<Family> -<:father>-> m<Member>])
        lazy-pattern2 (pattern [g] [m1<Member> --> <> --> m2<Member>
                                    :when (distinct? m1 m2)
                                    :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:eager true} [g]
                                [m1<Member> --> <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fm) (eager-pattern1 fm)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fm)) (set (eager-pattern2 fm))))))
