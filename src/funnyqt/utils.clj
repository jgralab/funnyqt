(ns funnyqt.utils
  "Generic utility functions."
  (:use ordered.set))

(defn into-oset
  "Returns an ordered-set of all given arguments.  Collection args are
  converted into ordered-sets and united.  (into-oset nil) => #{}."
  ([]
     (ordered-set))
  ([to]
     (cond
      (nil? to)                             (ordered-set)
      (instance? ordered.set.OrderedSet to) to
      (coll? to)                            (into (ordered-set) to)
      :else                                 (ordered-set to)))
  ([to from]
     (into (into-oset to) (into-oset from)))
  ([to from & froms]
     (reduce into-oset (into-oset to from) froms)))

(defn qname?
  "Returns true, iff `n' is possibly a element qualified name.
  Only checks, if `n' is a symbol, a keyword, or a string."
  [n]
  (or (symbol? n) (keyword? n) (string? n)))

(defn type-spec?
  "Returns true, iff `n' is possibly a type specification.
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
  "Given a qualified name `qn' as string, symbol, or keyword, returns a vector
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

;;** Throwing exceptions

(defn error
  "Throws an exception with the given message and cause."
  ([msg]
     (error msg nil))
  ([msg cause]
     (throw (RuntimeException. msg cause))))

;;** Compilation

(defmacro ignore-if-not
  "Evaluate `body' only if `exp' evaluates to true and doesn't throw an
  exception.  Useful for stuff that requires JDK 1.7 classes.

  (ignore-if-not (Class/forName \"java.util.concurrent.ForkJoinTask\")
    (defn my-cool-fj-fn
      \"Do Stuff with ForkJoinTask\"
      [x]
      (magick x))"
  [exp & body]
  (let [[r m] (try
                [(eval exp) "Ok!"]
                (catch Exception e
                  [nil (str (.getMessage e))]))]
    (if r
      `(do ~@body)
      `(do
         (println ~(format "%s: so some stuff won't be compiled." m))
         (comment ~@body)))))

;;** Docs

(defmacro add-long-doc!
  "Add :long-doc metadata to the current namespace."
  [doc]
  `(alter-meta! *ns* assoc :long-doc ~doc))
