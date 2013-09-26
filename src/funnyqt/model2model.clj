(ns funnyqt.model2model
  (:require [funnyqt.utils        :as u]
            [funnyqt.protocols    :as p]
            [funnyqt.query        :as q]
            [clojure.tools.macro  :as tm]))

(defn ^:private args-types-map [from]
  (loop [f from, r {}]
    (if (seq f)
      (if (and (symbol? (first f))
               (coll? (second f)))
        (recur (nnext f) (assoc r (first f) (second f)))
        (recur (next f)  (assoc r (first f) nil)))
      r)))

(defn ^:private rule? [form]
  (if-let [[name & body] form]
    (let [m (apply hash-map (apply concat (partition 2 body)))]
      (and (or (contains? m :disjuncts)
               (contains? m :from)
               (contains? m :to))
           (if (not (contains? m :from))
             (u/errorf "All rules must contain a :from clause: %s" form)
             true)
           (if (contains? m :disjuncts)
             (and
              (or (vector? (:disjuncts m))
                  (u/errorf ":disjuncts must be a vector: %s" form))
              (if (or (contains? m :to))
                (u/errorf (str ":disjuncts rules may have a :when/:when-let clause "
                               "and a body, but no :to: %s")
                          form)
                true))
             true)
           (if (contains? m :to)
             (or (vector? (:to m))
                 (u/errorf ":to must be a vector: %s" form))
             true)
           (if (and (contains? (meta name) :top)
                    (not= (count (keys (args-types-map (:from m)))) 1))
             (u/errorf "Top-level rules must declare exactly one argument: %s" form)
             true)))
    (u/errorf "neither helper nor rule: %s" form)))

(def ^{:dynamic true
       :doc "A map {rule {input output}}."}
  *trace*)

(defn ^:private rule-as-map [rule]
  (let [[name & body] rule]
    (assoc (loop [b body, v []]
             (if (seq b)
               (if (keyword? (first b))
                 (recur (nnext b) (conj v (first b) (second b)))
                 (apply hash-map (conj v :body b)))
               (apply hash-map v)))
      :name name)))

(defn ^:private make-disjunct-rule-calls [args gens]
  (map (fn [w] `(apply ~w ~args)) gens))

(defn ^:private make-trace-lookup [args rule]
  `(get ((deref *trace*) ~(keyword (name rule))) ~args))

(defn ^:private type-constrs [a-t-m wrap-in-and]
  (let [form (for [[e t] a-t-m
                   :when t]
               `(p/has-type? ~e ~t))]
    (if wrap-in-and
      (cons `and form)
      form)))

(defn ^:private create-vector [v outs]
  (let [v (loop [v v, r []]
            (if (seq v)
              (if (= (first (nnext v)) :model)
                (recur (nnext (nnext v)) (conj (into r (take 2 v))
                                               (nth v 3)))
                (recur (nnext v) (conj (into r (take 2 v))
                                       (first outs))))
              r))
        v (partition 3 v)]
    (vec (mapcat (fn [[sym type model]]
                   [sym `(p/create-element! ~model ~type)])
                 v))))

(defn ^:private disjunct-rules [rule-map]
  (seq (take-while #(not= :result %) (:disjuncts rule-map))))

(defn ^:private convert-rule [outs rule-map]
  (let [a-t-m (args-types-map (:from rule-map))
        arg-vec (vec (keys a-t-m))
        create-vec (create-vector (:to rule-map) outs)
        created (mapv first (partition 2 create-vec))
        retval (if (= (count created) 1)
                 (first created)
                 created)
        wl-vars (map first (partition 2 (:when-let rule-map)))
        creation-form (when (seq created)
                        `(let ~create-vec
                           (swap! *trace* update-in [~(keyword (:name rule-map))]
                                  assoc ~arg-vec ~retval)
                           ~@(:body rule-map)
                           ~retval))
        wl-and-creation-form (if (:when-let rule-map)
                               `(let ~(vec (:when-let rule-map))
                                  (when (and ~@wl-vars)
                                    ~creation-form))
                               creation-form)
        when-wl-and-creation-form (if (:when rule-map)
                                    `(when ~(or (:when rule-map) true)
                                       ~wl-and-creation-form)
                                    wl-and-creation-form)]
    (when-let [uks (seq (disj (set (keys rule-map))
                              :name :from :to :when :when-let :body :disjuncts))]
      (u/errorf "Unknown keys in rule: %s" uks))
    `(~(:name rule-map) ~arg-vec
      ~(if-let [d (:disjuncts rule-map)]
         (let [drs (disjunct-rules rule-map)
               d (take-last 2 d)
               result-spec (if (= :result (first d))
                             (second d)
                             (gensym "disj-rule-result"))
               disj-calls-and-body `(let [r# (or ~@(make-disjunct-rule-calls arg-vec drs))]
                                      (when-let [~result-spec r#]
                                        ~@(:body rule-map)
                                        r#))]
           `(when (and ~@(type-constrs a-t-m false)
                       ~(or (:when rule-map) true))
              ~(if (:when-let rule-map)
                 `(when-let ~(:when-let rule-map)
                    ~disj-calls-and-body)
                 disj-calls-and-body)))
         `(or ~(make-trace-lookup arg-vec (:name rule-map))
              (when ~(type-constrs a-t-m true)
                ~when-wl-and-creation-form))))))

(defmacro deftransformation
  "Creates a model-to-model transformation named `name` with the declared
 `in-models`, `out-models`, and optional additional `args`.  For example,

    (deftransformation foo2bar [[in1 in2] [out] x y z]
      ...)

  declares a transformation named foo2bar that receives 2 input models in1 and
  in2, creates elements in the single output model out, and has three
  additional parameters x, y, and z.

  In the rest of the transformation spec, rules and functions are defined.

  Rules
  =====

  Rules are defined similarily, but they are identified by several keywords.
  A plain mapping rule has the form:

    (a2b
      :from [a 'InClass, x]
      :when (some-predicate? a)
      :when-let [x (some-fn a)]
      :to [b 'OutClass, c 'OutClass2]
      (do-stuff-with a b c))

  :from declares the number and types of elements for which this rule is
  applicable.  Providing types is optional.  So the rule above has to be called
  with 2 arguments a and x, and the first one needs to be of metamodel type
  InClass.
  :when constrains the input elements to those satisfying some predicate.
  The :when clause is optional.
  :when-let has a vector of variable-expr pairs.  The expressions are evaluated
  and bound to the respective vars.  The rule may only be applied if the vars
  are non-nil (which makes the \"when\"-part in :when-let).
  :to is a vector of output elements (paired with their types) that are to be
  created.

  If there are multiple output models, the :to spec has to state in which model
  a given object has to be created, e.g.,

    :to [b 'OutClass  :model out1,
         c 'OutClass2 :model out2]

  Following these special keyword-clauses, arbitrary code may follow, e.g., to
  set attributes and references of the newly created objects by calling other
  rules.

  A rule always returns the elements that where created for a given input
  element in terms of the :to clause.  If the called rule's :to clause creates
  only one object, the result of a call is this object.  If its :to clause
  creates multiple objects, the result of a call is a vector of the created
  objects in the order of their declaration in :to.

  Disjunctive Rules
  =================

  Besides normal mapping rules, there are disjunctive rules.  Those have the
  following form:

    (x2y
      :from [x]
      :disjuncts [a2b c2d ... :result y]
      (optional-body-using y))

  Disjunctive rules mustn't have a :to clause, but :when/:when-let are
  supported.  When a disjunctive rule is applied, it tries the given disjunct
  rules in the declared order.  The first one whose constraints and :from type
  match gets applied.  An optional :result spec may be the last thing in
  the :disjuncts vector.  If a disjunct rule could be applied, the result is
  bound to that spec and can be used in the optional body.  The spec may be a
  symbol or any destructuring form supported by let.

  Top-level Rules
  ===============

  At least one rule has to be declared top-level using ^:top metadata:

    (^:top a2b
       :from [a 'A]
       ;; same as above
       ...)

  When the transformation gets executed, top-level rules are applied to
  matching elements automatically (unless there's a main function; see below).
  All other rules have to be called from the top-level rules explicitly.
  Top-level rules must have exactly one element declared in their :from clause.

  Functions
  =========

  Functions are just arbitrary local helpers.  They are to be defined in the
  syntax of function definitions in `letfn`, that is, they support for
  overloading etc.

  There may be one special function called `main`.  This function must not have
  parameters.  If defined, it acts as the entry point to the transformation,
  i.e., it is called automatically and is responsible for calling the
  transformation's rules appropriately.  If there's a `main` function, the
  ^:top metadata attached to rules has no effect, that is, they are not called
  automatically.

  So you usually have either top-level rules or a `main` function.  The former
  is simpler and usually suffices while the latter provides more control to the
  user.  For example, there may be cases where some entry rule has to be called
  with model elements that are sorted topologically, or your transformation
  needs to bind additional dynamic vars.  In such cases, defining a `main`
  function allows you to do so.

  Transformation Trace
  ====================

  The transformation function returns the trace of the transformation as a map
  of the form:

    {:rule1 {[in1 in2] out1, ...}
     :rule2 {[in] [out1 out2], ...}
     ...}

  In that example, it is obvious that rule1 creates just one target element for
  two given input elements, whereas rule2 creates two output elements for one
  given input element.  In other words, rule1 has 2 elements declared in :from
  and 2 elements in :to, and for rule2 it's the other way round.

  Transformation Inheritance
  ==========================

  By providing :extends metadata to the transformation (a symbol or a vector of
  symbols denoting other transformations), one can declare the new
  transformation to extend one or many other transformations.

    (deftransformation ^{:extends foo2bar-base} foo2bar
     \"Transforms a foo model to a bar model.\"
     [foo bar]
     ...)

  The transformation foo2bar extends foo2bar-base here.  This means that
  foo2bar contains its own rules and functions plus the ones defined in
  foo2bar-base.  If foo2bar defines a function or rule that's already defined
  by foo2bar-base, then foo2bar's version overrides the version from
  foo2bar-base.  foo2bar calls all top-level relations defined by itself and
  foo2bar-base.

  Note that transformation inheritance doesn't mangle transformation parameters
  in any way.  If a base transformation uses an argument vector [src trg] but
  the extending transformation uses [s t], you'll get a compile error if
  inherited/non-overridden rules use either one of them.  So in general, it is
  advisable that extending transformations have the very same parameters as the
  extended transformations, plus optionally some more parameters."

  {:arglists '([name [[& in-models] [& out-models] & args] & rules-and-fns])}
  [name & more]
  (let [[name more] (tm/name-with-attributes name more)
        [args rules-and-fns] (if (vector? (first more))
                               [(first more) (next more)]
                               (u/errorf "Error: arg vector missing!"))
        [ins outs aa] (let [[i o & aa] args]
                        (cond
                         (nil? i) (u/errorf "No input models given.")
                         (nil? o) (u/errorf "No output models given.")
                         (not (vector? i)) (u/errorf "input models must be a vector.")
                         (not (vector? o)) (u/errorf "output models must be a vector.")
                         :else [i o aa]))
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     rules-and-fns)
        rules (apply hash-map (mapcat (fn [r] [(first r) (rule-as-map r)]) rules))
        fns   (apply hash-map (mapcat (fn [f] [(first f) f]) fns))
        [rules fns] (if-let [extended (:extends (meta name))]
                      (do
                        (when-not (or (symbol? extended)
                                      (and (vector? extended)
                                           (every? symbol? extended)))
                          (u/errorf (str "The value of :extends must be a symbol or a "
                                         "vector of symbols denoting transformations: "
                                         "%s")
                                    extended))
                        (let [extended (if (coll? extended) extended [extended])]
                          [(apply merge (conj (mapv #(::rules (meta (resolve %)))
                                                    extended)
                                              rules))
                           (apply merge (conj (mapv #(::fns   (meta (resolve %)))
                                                    extended)
                                              fns))]))
                              [rules fns])
        top-rules   (filter #(:top (meta (first %))) rules)
        rule-by-name (fn [n]
                       (or (get rules n) (u/errorf "No such rule: %s" n)))
        main-fn (get fns 'main)
        collect-type-specs (fn ct [rule-map]
                             (if-let [disj-rules (disjunct-rules rule-map)]
                               (let [specs (set (map (comp ct rule-by-name) disj-rules))]
                                 (if (= 1 (count specs))
                                   (first specs)
                                   (vec specs)))
                               (remove nil? (vals (args-types-map (:from rule-map))))))
        type-spec (vec (cons :or (distinct (remove nil? (mapcat collect-type-specs
                                                                (vals top-rules))))))
        rule-specs (map (partial convert-rule outs) (vals rules))
        elem-var (gensym "elem")]
    (when-not (or main-fn (seq top-rules))
      (u/error (str "At least one rule has to be declared as top-level rule, "
                    "or there has to be a main function!")))
    (when (and main-fn (not (zero? (count (second main-fn)))))
      (u/error "The main function mustn't have any parameters."))
    (when (and main-fn (seq top-rules))
      (println (str "There's a main function and " (count top-rules)
                    " top-level rules.  Top-level rules are only applied "
                    "automatically if there's no main function.")))
    `(defn ~name ~(merge (meta name)
                         {::rules (list 'quote rules)
                          ::fns   (list 'quote fns)})
       [~@ins ~@outs ~@aa]
       (binding [*trace* (atom {})]
         (letfn [~@(vals fns)
                 ~@rule-specs]
           ~@(if main-fn
               `[(~'main)]
               (for [m ins]
                 `(doseq [~elem-var (p/elements ~m ~type-spec)]
                    ~@(for [r (keys top-rules)]
                        `(~r ~elem-var)))))
           @*trace*)))))
