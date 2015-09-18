(ns funnyqt.query
  "Generic query functions like regular path expressions & quantified expressions."
  (:require [flatland.ordered.set :as os]
            [funnyqt
             [generic :as g]
             [utils :as u]]))

;;# Quantified Expressions

(def ^{:doc "Returns logical true, iff `pred` holds forall elements in `coll`."
       :arglists '([pred coll])}
  forall? every?)

(defn exists?
  "Returns logical true iff `pred` holds at least for one element in `coll`."
  [pred coll]
  (some pred coll))

(defn exist-n?
  "Returns logical true iff `pred` holds for exactly `n` elements in `coll`."
  [n pred coll]
  (let [s (filter pred coll)
        good (take n s)
        bad  (drop n s)]
    ;; There must be one and no other
    (and (= (count good) n) (not (seq bad)))))

;;# Sequence Functions

(defn member?
  "Returns true iff `e` is a member of `coll`."
  [e ^java.util.Collection coll]
  (if (seq coll)
    (.contains coll e)
    false))

(defn the
  "Returns the only element of sequable `s` (which satisfies `pred`) and errors
  if `s` contains more or less elements."
  ([s]
   (let [s (seq s)
         f (nth s 0 ::not-found)]
     (if (= f ::not-found)
       (u/errorf "Seq contains zero elements!")
       (if (= ::not-found (nth (rest s) 0 ::not-found))
         f
         (u/errorf "Seq contains more than one element!")))))
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

(defn seq-comparator
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

    (sort (seq-comparator #(- %2 %1) - compare)
          [[1 10 \"b\"] [3 7 \"b\"] [1 2 \"b\"] [1 10 \"c\"] [3.0 17 \"a\"]])
    ;=> ([3 7 \"b\"] [3.0 17 \"a\"] [1 2 \"b\"] [1 10 \"b\"] [1 10 \"c\"])"
  [& cmps]
  (fn [a b]
    (or (first (remove zero? (map #(%1 %2 %3) cmps a b)))
        (- (hash a) (hash b)))))

(defn sort-topologically
  "Returns the objects `els` as topologically sorted vector or false if there's
  a cycle.  `deps-fn` is a function that given an object of `els` returns its
  \"dependencies\", i.e., objects that need to be sorted before it."
  [deps-fn els]
  (let [els (set els)]
    (loop [rem els, known  #{}, sorted []]
      (if (seq rem)
        (let [gs (group-by (fn [n]
                             (every? #(member? % known)
                                     (filter els (deps-fn n))))
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

(defn p-apply
  "Applies the regular path expression `p` to the start node or set of start
  nodes `n`."
  [n p]
  (cond
   ;; funs: -->
   (fn? p)          (p n)
   ;; funs with params: [--> 'Foo], [p-alt --> <>--]
   (vector? p)      (apply (first p) n (rest p))
   ;; adjacences / that-role names
   (u/prop-name? p) (into (os/ordered-set)
                          (g/adjs*-transducer p)
                          (u/oset n))
   :else (u/errorf "Don't know how to apply %s." p)))

(defprotocol ISimpleRegularPathExpression
  (--> [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one forward edge.
  May be restricted by a type specification (TG) or reference
  specification (EMF) `spec` and a predicate `pred` on the edge.  The 3-arity
  version with `pred` is not supported for EMF because there are no first-class
  edges in EMF.")
  (---> [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one forward edge
  which must not be a containment edge.  May be restricted by a type
  specification (TG) or reference specification (EMF) `spec` and a predicate
  `pred` on the edge.  The 3-arity version with `pred` is not supported for EMF
  because there are no first-class edges in EMF.")
  (<-- [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one backward edge.
  May be restricted by a type specification (TG) or reference
  specification (EMF) `spec` and a predicate `pred` on the edge.  For EMF, only
  bidirectional references are considered, and the 3-arity version with `pred`
  is not supported because there are no first-class edges in EMF.")
  (<--- [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one backward edge
  which must not be a containment edge.  May be restricted by a type
  specification (TG) or reference specification (EMF) `spec` and a predicate
  `pred` on the edge.For EMF, only bidirectional references are considered, and
  the 3-arity version with `pred` is not supported because there are no
  first-class edges in EMF.")
  (<-> [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one edge, no
  matter if incoming or outgoing.  May be restricted by a type specification
  (TG) or reference specification (EMF) `spec` and a predicate `pred` on the
  edge.  For EMF, only bidirectional references are considered, and the 3-arity
  version with `pred` is not supported because there are no first-class edges
  in EMF.")
  (<--> [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one edge which
  must not be a containment edge, no matter if incoming or outgoing.  May be
  restricted by a type specification (TG) or reference specification (EMF)
  `spec` and a predicate `pred` on the edge.  For EMF, only bidirectional
  references are considered, and the 3-arity version with `pred` is not
  supported because there are no first-class edges in EMF.")
  (<>-- [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one (strict)
  containment edge from whole to part (`n` is the container of the result), no
  matter if incoming or outgoing.  May be restricted by a type specification
  (TG) or reference specification (EMF) `spec` and a predicate `pred` on the
  edge.  The 3-arity version with `pred` is not supported for EMF because there
  are no first-class edges in EMF.")
  (--<> [n] [n spec] [n spec pred]
    "Returns the ordered set of nodes reachable from `n` via one (strict)
  containment edge from part to whole (`n` is contained in the result), no
  matter if incoming or outgoing.  May be restricted by a type specification
  (TG) or reference specification (EMF) `spec` and a predicate `pred` on the
  edge.  The 3-arity version with `pred` is not supported for EMF because there
  are no first-class edges in EMF."))

(extend-protocol ISimpleRegularPathExpression
  java.util.Collection
  (-->
    ([n]           (u/into-oset (os/ordered-set) (mapcat -->) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(--> % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(--> % spec pred)) n)))
  (--->
    ([n]           (u/into-oset (os/ordered-set) (mapcat --->) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(---> % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(---> % spec pred)) n)))
  (<--
    ([n]           (u/into-oset (os/ordered-set) (mapcat <--) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(<-- % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(<-- % spec pred)) n)))
  (<---
    ([n]           (u/into-oset (os/ordered-set) (mapcat <---) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(<--- % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(<--- % spec pred)) n)))
  (<->
    ([n]           (u/into-oset (os/ordered-set) (mapcat <->) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(<-> % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(<-> % spec pred)) n)))
  (<-->
    ([n]           (u/into-oset (os/ordered-set) (mapcat <-->) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(<--> % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(<--> % spec pred)) n)))
  (<>--
    ([n]           (u/into-oset (os/ordered-set) (mapcat <>--) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(<>-- % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(<>-- % spec pred)) n)))
  (--<>
    ([n]           (u/into-oset (os/ordered-set) (mapcat --<>) n))
    ([n spec]      (u/into-oset (os/ordered-set) (mapcat #(--<> % spec)) n))
    ([n spec pred] (u/into-oset (os/ordered-set) (mapcat #(--<> % spec pred)) n))))

(defn p-seq
  "Path sequence starting at `n` and traversing `ps`.
  `n` may be a node or a collection of nodes.
  `ps` is a varargs seq of regular path expressions."
  [n & ps]
  (u/oset (reduce p-apply n ps)))

(defn p-opt
  "Path option starting at `n` and maybe traversing `p`.
  `n` may be a node or a collection of nodes.
  `p` is a path expression."
  [n p]
  (u/into-oset n (p-apply n p)))

(defn p-alt
  "Path alternative starting at `n` and traversing each alternative in `ps`.
  `n` may be a node or a collection of nodes.
  `ps` is a varags seq of the alternative path expressions."
  [n & ps]
  (into (flatland.ordered.set/ordered-set)
        (mapcat #(p-apply (u/oset n) %))
        ps))

(defn ^:private p-*-or-+
  [n p ret]
  (let [n (into (flatland.ordered.set/ordered-set)
                (remove ret)
                (u/oset (p-apply n p)))]
    (if (seq n)
      (recur n p (u/into-oset ret n))
      ret)))

(defn p-*
  "Path iteration starting at `n` and traversing `p` zero or many times.
  `n` may be a node or a collection of nodes.
  `p` is a path expression."
  [n p]
  (p-*-or-+ n p (u/oset n)))

(defn p-+
  "Path iteration starting at `n` and traversing `p` one or many times.
  `n` may be a node or a collection of nodes.
  `p` is a path expression."
  [n p]
  (p-*-or-+ n p (flatland.ordered.set/ordered-set)))

(defn p-exp
  "Path exponent starting at `n` and traversing `p` `i` times, or at least `l`
  and at most `u` times.
  `n` may be a node or a collection of nodes.
  `n` or `l` and `u` are integers with `l` <= `u`.
  `p` is a path expression."
  ([n l u p]
     {:pre [(<= l u) (>= l 0) (>= u 0)]}
     (loop [i (- u l), s (p-exp n l p)]
       (if (pos? i)
         (let [ns (into s (p-apply s p))]
           (if (= (count s) (count ns))
             s
             (recur (dec i) ns)))
         s)))
  ([n i p]
     {:pre [(>= i 0)]}
     (if (zero? i)
       (u/oset n)
       (recur (p-apply n p) (dec i) p))))

(defn p-restr
  "Path restriction concerning `ts` and `pred` on node `n` or each node in `n`.
  `ts` is a type specification (see `funnyqt.generic/type-matcher`), `pred` a
  predicate on nodes."
  ([n spec]
   (p-restr n spec identity))
  ([n spec pred]
   (let [n (u/oset n)]
     (if (seq n)
       (let [tm (g/type-matcher (first n) spec)]
         (into (os/ordered-set)
               (filter (every-pred tm pred))
               n))
       n))))
