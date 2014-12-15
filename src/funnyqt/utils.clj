(ns funnyqt.utils
  "Generic utility functions."
  (:require clojure.pprint
            [clojure.string :as str]
            [flatland.ordered.set :as os]))

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
  ([to from & froms]
     (reduce into (into-oset to from) froms)))

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

(defn deep-vectorify
  "Convert the collection `coll` to a vector.  If `coll` is a collection
  containing collections, do it recursively."
  [coll]
  (when (map? coll)
    (errorf "Cannot deep-vectorify map %s." coll))
  (if (coll? coll)
    (mapv deep-vectorify coll)
    coll))

(def ^:private ^java.lang.reflect.Method AbstractElist-isUnique
  (-> (.getDeclaredMethod org.eclipse.emf.common.util.AbstractEList
                          "isUnique" (make-array Class 0))
      (doto (.setAccessible true))))

(defn unique-coll?
  "Returns true if `l` is a unique collection, i.e., any element may occur at
  most once."
  [l]
  (or (instance? java.util.Set l)
      (instance? org.eclipse.emf.common.util.UniqueEList l)
      (and (instance? org.eclipse.emf.common.util.AbstractEList l)
           (.invoke AbstractElist-isUnique l (make-array Object 0)))))

(defn ^:private shortcut-when-let-vector [lv]
  (letfn [(whenify [s]
            (if (coll? s)
              (mapcat (fn [v] [:when v]) s)
              [:when s]))]
    (mapcat (fn [[s v]]
              (concat [:let [s v]]
                      (whenify s)))
            (partition 2 lv))))

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

(defn for+-doseq+-helper [what seq-exprs & body]
  (let [seq-exprs (shortcut-when-let-bindings seq-exprs)
        [bind exp] seq-exprs]
    (condp = bind
      :let `(let ~exp
              ~(apply for+-doseq+-helper what (vec (nnext seq-exprs)) body))
      :when `(when ~exp
               ~(apply for+-doseq+-helper what (vec (nnext seq-exprs)) body))
      ;; default
      (if (seq seq-exprs)
        `(~what ~seq-exprs ~@body)
        (when (= what `for)
          [(first body)])))))

(defmacro for+
  "An enhanced version of clojure.core/for with the following additional
  features.

  - :let [var exp,...]    may occur as first element
  - :when exp             may occur as first element
  - :when-let [var expr]  bindings

  As a special case, (for+ [] x) returns [x]."
  [seq-exprs body-expr]
  (for+-doseq+-helper `for seq-exprs body-expr))

(defmacro doseq+
  "An enhanced version of clojure.core/doseq with the following additional
  features.

  - :let [var exp,...]    may occur as first element
  - :when exp             may occur as first element
  - :when-let [var expr]  bindings"
  [seq-exprs & body]
  (apply for+-doseq+-helper `doseq seq-exprs body))
