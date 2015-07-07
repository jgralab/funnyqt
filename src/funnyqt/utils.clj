(ns funnyqt.utils
  "Generic utility functions, e.g., for signaling errors, debugging, and profiling, "
  (:require [clojure pprint
             [repl :as repl]
             [string :as str]]
            [flatland.ordered.set :as os])
  (import (org.pcollections PCollection ArrayPSet)))

;;# Conversion to OrderedSet

(defprotocol IOrderedSetConvertible
  (oset [this]
    "Converts this into an ordered set."))

(extend-protocol IOrderedSetConvertible
  flatland.ordered.set.OrderedSet
  (oset [this]
    this)

  java.util.Collection
  (oset [this]
    (into (os/ordered-set) this))

  Object
  (oset [this]
    (os/ordered-set this))

  nil
  (oset [this] (os/ordered-set)))

(defn into-oset
  "Returns an ordered-set of all given arguments which must be collections."
  ([to from]
   (into (oset to) from))
  ([to xform from]
   (into (oset to) xform from)))

(defmacro assert-flat-oset
  "Asserts that obj is an ordered set containing no collections, only flat
  objects.  (Only evaluates, iff clojure.core/*assert* is true.)"
  [obj]
  `(assert
    (let [s# ~obj]
      (and (instance? flatland.ordered.set.OrderedSet s#)
           (every? (complement coll?) s#)))
    "The given object is no OrderedSet, or it is but is not flat."))

;;# Fiddeling with qnames

(defn qname?
  "Returns true, iff `n` is possibly a element qualified name.
  Qualified names are denoted as symbols."
  [n]
  (and (symbol? n) (re-matches #"!?(?:(?:\w|[.])+\.)?[a-zA-Z]\w*!?" (name n))))

(defn prop-name?
  "Returns true, iff `n` is possibly a property name.
  Property names (attributes, roles, references) are denoted as keywords."
  [n]
  (and (keyword? n) (re-matches #"[a-z]\w*" (name n))))

(defn type-spec?
  "Returns true, iff `n` is possibly a type specification.
  Examples for valid type specs:
    pkg.Foo, Bar!, bla.Bla, [Foo Bar !Baz], [:and !Foo !Bar]"
  [n]
  (or (qname? n)
      (and (vector? n)
           (let [x (first n)]
             (or (qname? x)
                 (#{:and :or :xor :nor :nand} x))))))

(defn type-with-modifiers
  "Given a type name with modifiers (Foo, !Foo, Foo!, !Foo!), returns a tuple
  of form [neg name exact]."
  [^String name]
  (let [neg (.startsWith name "!")
        ext (.endsWith   name "!")
        name (symbol (.substring name
                                 (if neg 1 0)
                                 (if ext (dec (.length name)) (.length name) )))]
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

(defn replace-word [s old new]
  "In string `s` replace all occurences of `old` with `new`.
  The difference to (str/replace s \"\bold\b\" \"new\") is that \"foo\" won't
  be replaced in \"foo-bar\", i.e., in contrast to normal regexps, - is treated
  as a word character."
  (str/replace s (re-pattern (str "(\\A|[^-\\w])"
                                  old
                                  "(\\Z|[^-\\w])"))
               (str "$1" new "$2")))

;;# Throwing exceptions

(defmacro error
  "Throws an exception with the given message and cause."
  ([msg]
   `(error ~msg nil))
  ([msg cause]
   `(throw (java.lang.Exception. ~msg ~cause))))

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
    :sec   (str (double (/ in 1000000000)) " sec")
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
  ; It took 311.945 µs to eval (take 10 (iterate inc 0)) to (0 1 2 3 4 5 6 7 8 9).
  (0 1 2 3 4 5 6 7 8 9)"
  [fmt form & args]
  (let [unit (second (re-matches #"(?s).*%T(nano|micro|milli|sec)?.*" fmt))]
    `(let [st# (System/nanoTime)
           result# ~form
           et# (- (System/nanoTime) st#)]
       (println (format (-> ~fmt
                            (str/replace #"%T(nano|micro|milli|sec)?"
                                         (time-str et# ~(if (seq unit)
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

;;# Macro writing utils

(defn ^:private tree-count [form]
  (cond
    (coll? form) (reduce + 1 (map tree-count form))
    :else 1))

(defn prewalk
  "Do a pre-order traversal of `form` calling `skip-fn` and `edit-fn` on subforms.
  If `skip-fn` returns logical true, this subform is is skipped in the
  traversal.  Else, `edit-fn` is called on the subform and its result replaces
  the original subform.  The replacement is not subject to any further
  traversal."
  [edit-fn skip-fn form]
  (let [a (atom 0)]
    (clojure.walk/prewalk (fn [el]
                            (cond
                              (skip-fn el)  (do (reset! a (dec (tree-count el))) el)
                              (pos? @a)     (do (swap! a dec) el)
                              :else         (let [x (edit-fn el)]
                                              (when (not= x el)
                                                (reset! a (dec (tree-count x))))
                                              x)))
                          form)))

;;# Protocol Checks

(defn satisfies-protocol? [x protocol msg]
  (if (satisfies? protocol x)
    true
    (do
      (when msg
        (println (format "%s doesn't satisfy protocol %s => %s"
                         x (:name (meta (:var protocol))) msg)))
      ;; We return nil here so that we can use it in place where a seq is
      ;; expected.
      nil)))

;;# Misc

(def ^:private ^java.lang.reflect.Method AbstractElist-isUnique
  (-> (.getDeclaredMethod org.eclipse.emf.common.util.AbstractEList
                          "isUnique" (make-array Class 0))
      (doto (.setAccessible true))))

(def ^:private empty-object-array (make-array Object 0))

(defn unique-coll?
  "Returns true if `l` is a unique collection, i.e., any element may occur at
  most once."
  [l]
  (or (and (instance? org.eclipse.emf.common.util.AbstractEList l)
           (.invoke AbstractElist-isUnique l empty-object-array))
      (instance? java.util.Set l)
      (instance? org.eclipse.emf.common.util.UniqueEList l)
      (instance? java.util.Map l)))

(defn array-pset
  "Returns an ArrayPSet containing the given args.  ArrayPSets cannot contain
  nil, so nil args are not considered, e.g., (array-pset nil) returns the empty
  ArrayPSet."
  {:inline-arities #{0 1}
   :inline (fn
             ([] `(ArrayPSet/empty))
             ([x] `(if-let [x# ~x]
                     (.plus (ArrayPSet/empty) x#)
                     (ArrayPSet/empty))))}
  ([] (ArrayPSet/empty))
  ([x] (if x
         (.plus (ArrayPSet/empty) x)
         (ArrayPSet/empty)))
  ([x y & more]
   (loop [m more, s (-> (ArrayPSet/empty) (.plus x) (.plus y))]
     (if (seq more)
       (recur (rest more) (if-let [x (first more)]
                            (.plus s x)
                            s))
       s))))

(defn ^:private shortcut-when-let-bindings
  "Converts :when-let [y (foo x)] to :let [y (foo x)] :when y."
  [bindings]
  (loop [p bindings, nb []]
    (if (seq p)
      (if (= :when-let (first p))
        (recur (nnext p)
               (let [[var exp & too-many :as clause] (fnext p)]
                 (when too-many
                   (errorf "Unsupported :when-let binding: :when-let %s" clause))
                 (into nb [:let [var exp] :when var])))
        (recur (nnext p) (conj nb (first p) (second p))))
      nb)))

(defn iterator
  "Returns a java.util.Iterator for the given coll which may also be nil or a
  map.  Anything implementing Iterable will do."
  ^java.util.Iterator [coll]
  (cond
    (nil? coll) (.iterator clojure.lang.PersistentList/EMPTY)
    (instance? Iterable coll) (.iterator ^Iterable coll)
    (instance? java.util.Iterator coll) coll
    (instance? java.util.Map coll) (.iterator (.entrySet ^java.util.Map coll))
    :else (errorf "Don't know how to create iterator for instance of class %s"
                  (class coll))))

(defn ^:private for+-doseq+-helper [what seq-exprs body]
  (let [seq-exprs (shortcut-when-let-bindings seq-exprs)
        [bind exp] seq-exprs]
    (condp = bind
      :let `(let ~exp
              ~(for+-doseq+-helper what (vec (nnext seq-exprs)) body))
      :when `(if ~exp
               ~(for+-doseq+-helper what (vec (nnext seq-exprs)) body)
               (sequence nil))
      ;; default
      (if (seq seq-exprs)
        `(~what ~seq-exprs ~@body)
        (if (or (= what `for-1)
                (= what `for))
          (vec body)
          `(do ~@body))))))

(defmacro doseq-1
  "Like clojure.core/doseq but based on iterators without support for chunked
  seqs and :while.  The expressions must all be Iterable, Iterators, or Maps."
  [seq-exprs & body]
  (when-not (vector? seq-exprs)
    (error "doseq-1/doseq+ require a vector for its binding"))
  (when-not (even? (count seq-exprs))
    (error "doseq-1/doseq+ require an even number of forms in binding vector"))
  (when (seq seq-exprs)
    (let [[key val & remainder] seq-exprs
          remainder (vec remainder)]
      (cond
        ;; Normal bindings possibly with destructuring forms
        (or (symbol? key)
            (vector? key)
            (map? key))
        `(let [it# (iterator ~val)]
           (while (.hasNext it#)
             (let [~key (.next it#)]
               (doseq+ ~remainder ~@body))))
        ;;---
        (= :when key) `(when ~val
                         (doseq+ ~remainder ~@body))
        ;;---
        (= :let key)  `(let ~val
                         (doseq+ ~remainder ~@body))
        :default      (errorf "%s %s is currently unsupported in doseq+." key val)))))

(defmacro doseq+
  "An enhanced version of clojure.core/doseq with the following additional
  features.

  - :let [var exp,...]    may occur as first element
  - :when exp             may occur as first element
  - :when-let [var expr]  bindings

  As a special case, (doseq+ [] x) executes x once."
  [seq-exprs & body]
  (for+-doseq+-helper `doseq-1 seq-exprs body))

(defmacro for-1
  "Like clojure.core/for but based on iterators without support for chunked
  seqs.  The expressions in seq-exprs must all be Iterable, Iterators, or
  Maps."
  [seq-exprs body-expr]
  (when-not (vector? seq-exprs)
    (error "for-1/for+ require a vector for its binding"))
  (when-not (even? (count seq-exprs))
    (error "for-1/for+ require an even number of forms in binding vector"))
  (let [to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (with-meta (gensym "s__")
                                {:tag 'java.util.Iterator})
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur ~gxs))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                     `(let [iterys# ~(emit-bind next-groups)
                                            fs# (seq (iterys# (iterator ~next-expr)))]
                                        (if fs#
                                          (concat fs# (~giter ~gxs))
                                          (recur ~gxs)))
                                     :else `(cons ~body-expr
                                                  (~giter ~gxs))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           #_(comment "OUTER")
                           (lazy-seq
                            (loop [~gxs ~gxs]
                              (when (.hasNext ~gxs)
                                (let [~bind (.next ~gxs)]
                                  ~(do-mod mod-pairs))))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")]
                          `(fn ~giter [~gxs]
                             #_(comment "INNERMOST")
                             (lazy-seq
                              (loop [~gxs ~gxs]
                                (when (.hasNext ~gxs)
                                  (let [~bind (.next ~gxs)]
                                    ~(do-mod mod-pairs))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
       (iter# (iterator ~(second seq-exprs))))))

(defmacro for+
  "An enhanced version of clojure.core/for with the following additional
  features.

  - :let [var exp,...]    may occur as first element
  - :when exp             may occur as first element
  - :when-let [var expr]  bindings

  As a special case, (for+ [] x) returns [x]."
  [seq-exprs body-expr]
  (for+-doseq+-helper `for-1 seq-exprs [body-expr]))

(definline mapc
  "Like map but for side-effects only.  Returns nil."
  [f coll]
  `(doseq-1 [x# ~coll]
     (~f x#)))

(defn fn-name
  "Returns the name of the given function f."
  [f]
  (let [^String s (repl/demunge (pr-str (class f)))
        i (.lastIndexOf s (int \/))]
    (subs s (inc i))))

(defn map-nn
  "map non-nil: Like `map` but return only the seq of values for which (f x) is
  non-nil.  Thus, the resulting sequence may be shorter than `coll`.  This is
  actually a transducer variant of (remove nil? (map f coll))."
  [f coll]
  (sequence (comp (map f)
                  (remove nil?))
            coll))
