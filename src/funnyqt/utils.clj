(ns funnyqt.utils
  "Generic utility functions."
  (:require clojure.pprint)
  (:require [clojure.string :as str])
  (:use ordered.set))

;;# Conversion to OrderedSet

(defprotocol OrderedSetConvertible
  (to-oset [this]
    "Converts this into an ordered set."))

(extend-protocol OrderedSetConvertible
  ordered.set.OrderedSet
  (to-oset [this]
    this)

  java.util.Collection
  (to-oset [this]
    (into (ordered-set) this))

  Object
  (to-oset [this]
    (ordered-set this))

  nil
  (to-oset [this] (ordered-set)))

(defn into-oset
  "Returns an ordered-set of all given arguments.  Collection args are
  converted into ordered-sets and united.  (into-oset nil) => #{}."
  ([to from]
     (into (to-oset to)
           (if (coll? from)
             from
             (to-oset from))))
  ([to from & froms]
     (reduce into (into-oset to from) froms)))

(defmacro assert-flat-oset
  "Asserts that obj is an ordered set containing no collections, only flat
  objects.  (Only evaluates, iff clojure.core/*assert* is true.)"
  [obj]
  `(assert
    (let [s# ~obj]
      (and (instance? ordered.set.OrderedSet s#)
           (every? (complement coll?) s#)))
    "The given object is no OrderedSet, or it is but is not flat."))

;;# Fiddeling with qnames

(defn qname?
  "Returns true, iff `n` is possibly a element qualified name.
  Only checks, if `n` is a symbol, a keyword, or a string."
  [n]
  (or (symbol? n) (keyword? n) (string? n)))

(defn type-spec?
  "Returns true, iff `n` is possibly a type specification.
  Examples for valid type specs:
    pkg.Foo, :Bar!, \"bla.Bla\", [Foo Bar !Baz], [:and !Foo !Bar]"
  [n]
  (or (qname? n)
      (and (vector? n)
           (let [x (first n)]
             (or (qname? x) (= x :and) (= x :or) (= x :xor) (= x :nor) (= x :nand))))))

(defn type-with-modifiers
  "Given a type name with modifiers (Foo, !Foo, Foo!, !Foo!), returns a tuple
  of form [neg name exact]."
  [^String name]
  (let [neg (.startsWith name "!")
        ext (.endsWith   name "!")
        name (.substring name
                         (if neg 1 0)
                         (if ext (dec (.length name)) (.length name) ))]
    [neg name ext]))

(defn split-qname
  "Given a qualified name `qn` as string, symbol, or keyword, returns a vector
  of the form [\"foo.baz\" \"Bar\" \"foo.baz.Bar\"], i.e., package name, simple
  name, qname.

  For an attribute qualified name foo.baz.Bar.attr, it returns
    [\"foo.baz.Bar\" \"attr\" \"foo.baz.Bar.attr\"],
  i.e., owning element qname, attribute name, attribute qname."
  [qn]
  (let [qstr (name qn)
        liod (.lastIndexOf qstr ".")]
    (if (== liod -1)
      ["" qstr qstr]
      (let [sn (subs qstr (inc liod))
            pn (subs qstr 0 liod)]
        [pn sn qstr]))))

;;# Throwing exceptions

(defmacro error
  "Throws an exception with the given message and cause."
  ([msg]
     `(error ~msg nil))
  ([msg cause]
     `(throw (RuntimeException. ~msg ~cause))))

(defmacro errorf
  "Throws an exception with the given `msg` and `objs` passed to `format`.
  `msg` is a format string."
  [msg & objs]
  `(error (format ~msg ~@objs)))

;;# Debugging

(defn pr-identity
  "Returns and pretty prints the given argument `x` (preceeded by an optional
  `title`."
  ([x]
     (clojure.pprint/pprint x)
     x)
  ([title x]
     (print title)
     (pr-identity x)))

;;# Timing

(defn time-str
  "Converts a time value `in` in nanoseconds to a string \"<time> <unit>\".
  Valid units are :nano, :micro, :milli, :sec, and :auto meaning to convert to
  a unit in which there are at most 4 digits before the decimal separator."
  [in unit]
  (case unit
    :nano  (str in " ns")
    :micro (str (double (/ in 1000)) " µs")
    :milli (str (double (/ in 1000000)) " ms")
    :sec   (str (double (/ in 1000000000)) " s")
    :auto (cond
           (> in (Math/pow 10 9)) (time-str in :sec)
           (> in (Math/pow 10 6)) (time-str in :milli)
           (> in (Math/pow 10 3)) (time-str in :micro)
           :else (time-str in :nano))))

(defmacro timing
  "Times the execution of `form` and returns its result.
  Additionally, prints (format fmt args), where two new formatters are
  available:

    %T: the timing information with an appropriate unit
    %T(nano|micro|milli|sec): forces the given unit
    %R: the result of evaluating `expr`
    %F: the input form that is timed

  Example:

    user> (timing \"%s It took %T to eval %F to %R.\" (take 10 (iterate inc 0)) \";\")
    ; It took 0.031708 msecs to eval (take 10 (iterate inc 0)) to (0 1 2 3 4 5 6 7 8 9).
    (0 1 2 3 4 5 6 7 8 9)"
  [fmt form & args]
  (let [unit (second (re-matches #".*%T([^ ]*).*" fmt))]
    `(let [st# (System/nanoTime)
           result# ~form
           et# (- (System/nanoTime) st#)]
       (println (format (-> ~fmt
                            (str/replace #"%T[^ ]*" (time-str et# ~(if unit
                                                                     (keyword unit)
                                                                     :auto)))
                            (str/replace "%R" (print-str result#))
                            (str/replace "%F" (print-str (quote ~form))))
                        ~@args))
       result#)))

;;# Compilation

(defmacro compile-if
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`.

  (compile-if (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (do-cool-stuff-with-fork-join)
    (fall-back-to-executor-services))"
  [exp then else]
  (if (try (eval exp)
           (catch Throwable _ false))
    `(do ~then)
    `(do ~else)))

(defmacro compile-when
  "Evaluate `exp` and if it returns logical true and doesn't error, expand to
  `then`.  Else expand to `else`.

  (compile-when (and (< (:minor *clojure-version*) 4)
                     (= (:major *clojure-version*) 1))
    ;; Can't live without mapv and filterv!
    (defn mapv [...] ...)
    (defn filterv [...] ...))"
  [exp & then]
  (if (try (eval exp)
           (catch Throwable _ false))
    `(do
       ~@then)))

