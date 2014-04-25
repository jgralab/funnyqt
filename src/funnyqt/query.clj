(ns funnyqt.query
  "Generic functions like quantified expressions."
  (:require [clojure.core.reducers :as r]
            [flatland.ordered.set  :as os]
            [funnyqt.utils         :as u]
            [funnyqt.generic       :as g]))

;;# Quantified Expressions

(def ^{:doc "Returns logical true, iff `pred` holds forall elements in `coll`."
       :arglists '([pred coll])}
  forall? every?)

(defn exists?
  "Returns logical true iff `pred` holds at least for one element in `coll`."
  [pred coll]
  (some pred coll))

(defn exists1?
  "Returns logical true iff `pred` holds for exactly one element in `coll`."
  [pred coll]
  (let [s (filter pred coll)]
    ;; There must be one and no other
    (and (seq s) (not (next s)))))

;;# Sequence Functions

(defn no-dups
  "Returns a lazy sequence of the elements of coll with duplicates removed."
  [coll]
  (let [step (^:once fn* step [xs ^java.util.Set seen]
               (lazy-seq
                ((^:once fn* [xs ^java.util.Set seen]
                         (when-let [s (seq xs)]
                           (let [f (first s)]
                             (if (.contains seen f)
                               (recur (rest s) seen)
                               (do
                                 (.add seen f)
                                 (cons f (step (rest s) seen)))))))
                 xs seen)))]
      (step coll (java.util.HashSet.))))

(defn member?
  "Returns true iff `e` is a member of `coll`."
  [e ^java.util.Collection coll]
  (if (seq coll)
    (.contains coll e)
    false))

(defn the
  "Returns the only element of seq `s` (which satisfies `pred`) and errors if
  `s` contains more or less elements."
  ([s]
     (if-let [f (first s)]
       (if (next s)
         (u/errorf "seq contains more than one element: %s" (print-str s))
         f)
       (u/error "seq contains zero elements!")))
  ([pred s]
     (the (filter pred s))))

(defn ^:private pred-seq-internal
  [s p acc]
  (lazy-seq
   (if (seq s)
     (pred-seq-internal (rest s)
                        (first s)
                        (conj acc [p (first s)]))
     acc)))

(defn pred-seq
  "Predecessor Seq: Returns a lazy seq of pairs of seq `s`s elements.
  Each pair has the form [elems-predecessor-in-s elem]."
  [s]
  (pred-seq-internal s nil []))

(defn ^:private succ-seq-internal
  [s acc]
  (lazy-seq
   (if (seq s)
     (succ-seq-internal (rest s)
                        (conj acc [(first s) (fnext s)]))
     acc)))

(defn succ-seq
  "Successor Seq: Returns a lazy seq of pairs of seq `s`s elements.
  Each pair has the form [elem elems-successor-in-s]."
  [s]
  (succ-seq-internal s []))

(defn ^:private pred-succ-seq-internal
  [s pp p acc]
  (lazy-seq
   (if (seq s)
     (pred-succ-seq-internal (rest s)
                             p
                             (first s)
                             (conj acc [pp p (first s)]))
     (conj acc [pp p nil]))))

(defn pred-succ-seq
  "Predecessor-Successor Seq: Returns a lazy seq of triples of seq `s`s elements.
  Each triple has the form [pred cur succ]."
  [s]
  (rest (pred-succ-seq-internal s nil nil [])))

;;# Logical (higher-order) funs

(defmacro xor
  "Logical XOR: returns logical true iff an odd number of arguments is true."
  ([] false)
  ([x] x)
  ([x y & more]
     (if (seq more)
       `(xor ~(last more)
             (xor ~@(butlast more) ~x ~y))
       `(let [x# ~x, y# ~y]
          (if x#
            (if y# false x#)
            (if y# y# false))))))

(defn xor*
  "Logical XOR: returns true iff an odd number of arguments is true.
  Implemented as a function, so not short-cirquiting, but you can pass
  it to higher-order functions."
  [& xs]
  (reduce #(xor %1 %2) false xs))

(defn and*
  "Logical AND, implemented as a function, so not short-cirquiting, but
  you can pass it to higher-order functions."
  [& xs]
  (reduce #(and %1 %2) true xs))

(defmacro nand
  "Logical NAND."
  [& xs]
  `(not (and ~@xs)))

(defn nand*
  "Logical NAND, implemented as a function, so not short-cirquiting, but
  you can pass it to higher-order functions."
  [& xs]
  (not (apply and* xs)))

(defn or*
  "Logical OR, implemented as a function, so not short-cirquiting, but
  you can pass it to higher-order functions."
  [& xs]
  (reduce #(or %1 %2) nil xs))

(defmacro nor
  "Logical NOR."
  [& xs]
  `(not (or ~@xs)))

(defn nor*
  "Logical NOR, implemented as a function, so not short-cirquiting, but you
  can pass it to higher-order functions."
  [& xs]
  (not (apply or* xs)))

(defn xor-fn
  "Takes a seq of predicates `ps` and returns a varargs function that returns
  logical true iff an odd number of the predicates returns true."
  [& ps]
  (fn [& args]
    (loop [good 0, p ps]
      (if (seq p)
        (let [r (apply (first p) args)]
          (recur (if r (inc good) good) (rest p)))
        (odd? good)))))

(defn and-fn
  "Takes a seq of predicates `ps` and returns a varargs function that returns
  logical true iff all predicates return true.
  If no predicate is given, the returned fn returns constantly true."
  [& ps]
  (if (seq ps)
    (fn [& args]
      (apply (apply every-pred ps) args))
    (constantly true)))

(defn nand-fn
  "Takes a seq of predicates `ps` and returns a varargs function that returns
  logical true iff at least one predicate returns false.
  If no predicate is given, the returned fn returns constantly false."
  [& ps]
  (complement (apply and-fn ps)))

(defn or-fn
  "Takes a seq of predicates `ps` and returns a varargs function that returns
  logical true iff at least one of the predicates returns true.
  If no predicate is given, the returned fn returns constantly false."
  [& ps]
  (if (seq ps)
    (fn [& args]
      (apply (apply some-fn ps) args))
    (constantly false)))

(defn nor-fn
  "Takes a seq of predicates `ps` and returns a varargs function that returns
  logical true iff none of the predicate returns true.
  If no predicate is given, the returned fn returns constantly true."
  [& ps]
  (complement (apply or-fn ps)))

;;# Sorting

(defn seq-compare
  "Returns a sequence comparator function that compares 2 sequences element by
  element according to the given comparators `cmps`, i.e., the first 2 elements
  are compared with the first comparator, the second 2 elements with the second
  comparator, and so on.  Evaluates only as many comparators as are needed to
  distinguish the sequences, i.e., evaluates the comparators until one returns
  non-zero.

  `cmps` must be comparator functions that get 2 elements and returns 0 if the
  elements are equal, a negative integer if the first is \"smaller\", or a
  positive integer if the second is \"smaller\".

  If all comparators evaluate to 0, then the hash-codes are used as a last
  resort.

  Example: Sort a seq of 3-tuples of form [number number string] with
  descending order of the first component, ascending order of second component,
  and ascending orded of third components.  Clearly, for numbers - is a valid
  comparator, and for the strings we use compare which sorts lexicographically.

    (sort (seq-compare #(- %2 %1) - compare)
          [[1 10 \"b\"] [3 7 \"b\"] [1 2 \"b\"] [1 10 \"c\"] [3.0 17 \"a\"]])
    ;=> ([3 7 \"b\"] [3.0 17 \"a\"] [1 2 \"b\"] [1 10 \"b\"] [1 10 \"c\"])"
  [& cmps]
  (fn [a b]
    (or (first (remove zero? (map #(%1 %2 %3) cmps a b)))
        (- (hash a) (hash b)))))

(defn sort-topologically
  "Returns the objects `obs` as topologically sorted vector or false if there's
  a cycle.  `deps-fn` is a function that given an object of `objs` returns its
  \"dependencies\", i.e., objects that need to be sorted before it."
  [vs deps-fn]
  (let [vs (set vs)]
    (loop [rem vs, known  #{}, sorted []]
      (if (seq rem)
        (let [gs (group-by (fn [v]
                             (every? #(member? % known)
                                     (filter vs (deps-fn v))))
                           rem)
              good (gs true)
              bad (gs false)]
          ;;(println (count rem) ": good" (count good) "bad" (count bad))
          (if (seq good)
            (recur bad
                   (into known good)
                   (into sorted good))
            false))
        sorted))))

;;# Regular Path Expressions

(defn ^:private p-apply
  [v p]
  (cond
   ;; funs: -->
   (fn? p)          (p v)
   ;; funs with params: [--> 'Foo], [p-alt --> <>--]
   (vector? p)      (apply (first p) v (rest p))
   ;; adjacences / that-role names
   (u/prop-name? p) (into (os/ordered-set)
                          (r/mapcat #(g/reducible-adjs* % p) (u/oset v)))
   :else (u/errorf "Don't know how to apply %s." p)))

(defn reachables
  "Returns the ordered set of vertices reachable from `v` by via the path
  description `p`.
  `v` may be a vertex or a seq of vertices."
  [v p]
  (p-apply v p))

(defn p-seq
  "Path sequence starting at `v` and traversing `p`.
  `v` may be a vertex or a seq of vertices.
  `p` is a varargs seq of path descriptions."
  [v & p]
  (u/oset (r/reduce p-apply v p)))

(defn p-opt
  "Path option starting at `v` and maybe traversing `p`.
  `v` may be a vertex or a seq of vertices.
  `p` is a path description."
  [v p]
  (u/into-oset v (p-apply v p)))

(defn p-alt
  "Path alternative starting at `v` and traversing one of `p`.
  `v` may be a vertex or a seq of vertices.
  `p` is a varags seq of the alternative path descriptions."
  [v & p]
  (into (flatland.ordered.set/ordered-set)
        (r/mapcat #(p-apply (u/oset v) %) p)))

(defn ^:private p-*-or-+
  [v p ret]
  (let [n (into (flatland.ordered.set/ordered-set)
                (r/remove ret (u/oset (p-apply v p))))]
    (if (seq n)
      (recur n p (u/into-oset ret n))
      ret)))

(defn p-*
  "Path iteration starting at `v` and traversing `p` zero or many times.
  `v` may be a vertex or a seq of vertices.
  `p` is a path description."
  [v p]
  (p-*-or-+ v p (u/oset v)))

(defn p-+
  "Path iteration starting at `v` and traversing `p` one or many times.
  `v` may be a vertex or a seq of vertices.
  `p` is a path description."
  [v p]
  (p-*-or-+ v p (flatland.ordered.set/ordered-set)))

(defn p-exp
  "Path exponent starting at `v` and traversing `p` `n` times, or at least `l`
  and at most `u` times.
  `v` may be a vertex or a seq of vertices.
  `n` or `l` and `u` are integers with `l` <= `u`.
  `p` is a path description."
  ([v l u p]
     {:pre [(<= l u) (>= l 0) (>= u 0)]}
     (loop [i (- u l), s (p-exp v l p)]
       (if (pos? i)
         (let [ns (into s (p-apply s p))]
           (if (= (count s) (count ns))
             s
             (recur (dec i) ns)))
         s)))
  ([v n p]
     {:pre [(>= n 0)]}
     (if (zero? n)
       (u/oset v)
       (recur (p-apply v p) (dec n) p))))

(defn p-restr
  "Path restriction concerning `ts` and `pred` on each object in `objs`.
  ts is a type specification (see `funnyqt.generic/type-matcher`), `pred` a
  predicate."
  ([objs ts]
     (p-restr objs ts identity))
  ([objs ts pred]
     (let [objs (u/oset objs)]
       (if (seq objs)
         (let [tm (g/type-matcher (first objs) ts)]
           (into (os/ordered-set)
                 (r/filter (every-pred tm pred) objs)))
         objs))))
