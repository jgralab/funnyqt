(ns funnyqt.generic
  "Generic functions like quantified expressions."
  (:use [funnyqt.utils :only [error add-long-doc!]]))

(add-long-doc! "TODO")

;;* Quantified Expressions

(def ^{:doc "Returns logical true, iff `pred' holds forall elements in `coll'."}
  forall? every?)

(defn exists?
  "Returns logical true, iff `pred' holds at least for one element in `coll'."
  [pred coll]
  (some pred coll))

(defn exists1?
  "Returns logical true, iff `pred' holds for exactly one element in `coll'."
  [pred coll]
  (let [s (filter pred coll)]
    ;; There must be one and no other
    (and (seq s) (not (next s)))))

;;* Sequence Functions

(defn member?
  "Returns true, iff `e' is a member of `coll'."
  [e coll]
  (some #(= e %) coll))

(defn the
  "Returns the only element of seq `s' (which satisfies `pred') and errors if
  `s' contains more or less elements."
  ([s]
     (if-let [f (first s)]
       (if (next s)
         (error (format "seq contains more than one element!"))
         f)
       (error (format "seq contains zero elements!"))))
  ([pred s]
     (the (filter pred s))))

(defn- pred-seq-internal
  [s p acc]
  (lazy-seq
   (if (seq s)
     (pred-seq-internal (rest s)
                        (first s)
                        (conj acc [p (first s)]))
     acc)))

(defn pred-seq
  "Predecessor Seq: Returns a lazy seq of pairs of seq `s's elements.
  Each pair has the form [elems-predecessor-in-s elem]."
  [s]
  (pred-seq-internal s nil []))

(defn- succ-seq-internal
  [s acc]
  (lazy-seq
   (if (seq s)
     (succ-seq-internal (rest s)
                        (conj acc [(first s) (first (next s))]))
     acc)))

(defn succ-seq
  "Successor Seq: Returns a lazy seq of pairs of seq `s's elements.
  Each pair has the form [elem elems-successor-in-s]."
  [s]
  (succ-seq-internal s []))

;;* Logical (higher-order) funs

(defn xor
  "Logical XOR: returns true, iff exactly one argument is true.
  (xor) returns false."
  ([] false)
  ([f & r]
     (loop [t false, f (conj r f)]
       (if(seq f)
         (let [fv (first f)]
           (cond
            (and t fv)       false
            (and (not t) fv) (recur true (rest f))
            :else            (recur t (rest f))))
         t))))

(defn xor-fn
  "Takes a seq of predicates `ps' and returns a varargs function that returns
  logical true, iff exactly one of the predicates returns true."
  [& ps]
  (fn [& args]
    (loop [good false, p ps]
      (if (seq p)
        (let [r (apply (first p) args)]
          (if (and good r)
            false  ;; second true value, so stop it!
            (recur (or r good) (rest p))))
        good))))

(defn and-fn
  "Takes a seq of predicates `ps' and returns a varargs function that returns
  logical true, iff all predicates return true.
  If no predicate is given, the returned fn returns constantly true."
  [& ps]
  (if (seq ps)
    (fn [& args]
      (apply (apply every-pred ps) args))
    (constantly true)))

(defn nand-fn
  "Takes a seq of predicates `ps' and returns a varargs function that returns
  logical true, iff at least one predicate returns false.
  If no predicate is given, the returned fn returns constantly false."
  [& ps]
  (complement (apply and-fn ps)))

(defn or-fn
  "Takes a seq of predicates `ps' and returns a varargs function that returns
  logical true, iff at least one of the predicates returns true.
  If no predicate is given, the returned fn returns constantly false."
  [& ps]
  (if (seq ps)
    (fn [& args]
      (apply (apply some-fn ps) args))
    (constantly false)))

(defn nor-fn
  "Takes a seq of predicates `ps' and returns a varargs function that returns
  logical true, iff none of the predicate returns true.
  If no predicate is given, the returned fn returns constantly true."
  [& ps]
  (complement (apply or-fn ps)))

;;* Sorting

(defn seq-compare
  "Returns a sequence comparator function that compares 2 sequences element by
  element according to the given comparators `cmps', i.e., the first 2 elements
  are compared with the first comparator, the second 2 elements with the second
  comparator, and so on.  Evaluates only as many comparators as are needed to
  distinguish the sequences, i.e., evaluates the comparators until one returns
  non-zero.

  `cmps' must be comparator functions that get 2 elements and returns 0, if the
  elements are equal, a negative integer, if the first is \"smaller\", or a
  positive integer, if the second is \"smaller\".

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

