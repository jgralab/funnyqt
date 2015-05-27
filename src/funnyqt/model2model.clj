(ns funnyqt.model2model
  (:require [funnyqt.utils        :as u]
            [funnyqt.generic      :as g]
            [funnyqt.query        :as q]
            [clojure.tools.macro  :as tm]
            [flatland.ordered.map :as om]))

(defn ^:private args-types-map [from]
  (loop [f from, r (om/ordered-map)]
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
           (if-not (contains? m :from)
             (u/errorf "Rules must contain a :from clause: %s" form)
             true)
           (or (vector? (:from m))
               (u/errorf ":from must be a vector: %s" form))
           (or (>= (count (:from m)) 1)
               (u/errorf ":from must declare at least one input element: %s" form))
           (if-not (or (contains? m :to) (contains? m :disjuncts))
             (u/errorf "Rules must contain either a :to or a :disjuncts clause: %s" form)
             true)
           (if (contains? m :disjuncts)
             (and
              (or (vector? (:disjuncts m))
                  (u/errorf ":disjuncts must be a vector: %s" form))
              (if (contains? m :to)
                (u/errorf (str ":disjuncts rules may have a :let/:when/:when-let clause "
                               "and a body, but no :to: %s")
                          form)
                true))
             true)
           (if (contains? m :to)
             (and (or (vector? (:to m))
                      (u/errorf ":to must be a vector: %s" form))
                  (or (>= (count (:to m)) 1)
                      (u/errorf ":to must declare at least one output element: %s" form)))
             true)
           (if (contains? m :id)
             (or (and (vector? (:id m))
                      (= 2 (count (:id m)))
                      (symbol? (first (:id m))))
                 (u/errorf ":id must have form [id id-exp]: %s" form))
             true)
           (if (and (contains? (meta name) :top)
                    (not= (count (keys (args-types-map (:from m)))) 1))
             (u/errorf "Top-level rules must declare exactly one argument: %s" form)
             true)))
    (u/errorf "neither helper nor rule: %s" form)))

(def ^{:dynamic true
       :doc "An atom holding a map {rule {input output}}. Used internally."}
  *trace*)

(def ^{:dynamic true
       :doc "A map {rule {input output}} used for initializing *trace* if bound."}
  *initial-trace* nil)

(defn resolve-in
  "Resolve the input `in` of `rule` (given as keyword) in the transformation
  trace.  If the `rule` has only one input element, that has to be provided.
  If it has multiple input elements, a vector containing all of them in the
  order of the rule's :from clause has to be provided.  The return value is
  either the single output element of `rule` or a vector of output elements in
  the order of the rule's :to clause if the rule creates many output elements.
  If the rule has no :to clause at all, then the value of the rule's body is
  returned.

  Note that resolve-in doesn't work for disjunctive rules because those have no
  trace mappings themselves and just delegate to the disjunct rules.

  Also note that resolve-in is seldomly useful because (some-rule in) either
  transforms `in` or resolves it if it has been transformed already.  The only
  place where resolve-in is really needed is with rules with :id
  and :dup-id-eval clauses where one wants to test in the body if the current
  in elements have already been transformed."
  [rule in]
  (get-in @*trace* [rule in]))

(defn ^:private rule-as-map [rule]
  (let [[name & body] rule]
    (assoc (loop [b body, v [], constrs-and-bindings []]
             (if (seq b)
               (do
                 (when (and (keyword? (first b))
                            (not (#{:from :let :to :when :when-let :disjuncts :id :dup-id-eval}
                                  (first b))))
                   (u/errorf "Unknown clause in rule %s: %s %s" name (first b) (second b)))
                 (cond
                   (#{:let :when-let :when} (first b))
                   (recur (nnext b)
                          v
                          (conj constrs-and-bindings (first b) (second b)))
                   ;;---
                   (keyword? (first b))
                   (recur (nnext b)
                          (conj v (first b) (second b))
                          constrs-and-bindings)
                   ;;---
                   :else (recur nil (conj v :body b) constrs-and-bindings)))
               (let [m (apply hash-map v)]
                 (if (seq constrs-and-bindings)
                   (assoc m :cs-and-bs constrs-and-bindings)
                   m))))
           :name name)))

(defn ^:private make-disjunct-rule-calls [arg-vec gens]
  (map (fn [w] `(apply ~w ~arg-vec)) gens))

(defn ^:private type-constrs [a-t-m wrap-in-and]
  (let [form (for [[e t] a-t-m
                   :when t]
               `(g/has-type? ~e ~t))]
    (if wrap-in-and
      (cons `and form)
      form)))

(defn ^:private create-vector [v outs]
  ;; [a 'A
  ;;  b 'B :in foo
  ;;  c 'C {:p1 "foo"}
  ;;  d 'D :in foo {:p1 "foo"}
  ;;  e 'E {:p1 "foo"} :in foo
  ;;  f (rule-call ...)]       ;; f is initialized by another rule
  (letfn [(model? [v]
            (or (when (= (get v 2) :in) 3)
                (when (and (= (get v 3) :in)
                           (map? (get v 2)))
                  4)))
          (props? [v]
            (or (when (map? (get v 2)) 2)
                (when (and (map? (get v 4))
                           (= (get v 2) :in))
                  4)))]
    (let [v (loop [v v, r []]
              (if (seq v)
                (let [m (model? v)
                      p (props? v)]
                  (cond
                   (and m (not p))
                   (recur (subvec v 4)
                          (conj r [(first v) (second v) (nth v 3) nil]))
                   ;;---
                   (and (not m) p)
                   (recur (subvec v 3)
                          (conj r [(first v) (second v) nil (nth v 2)]))
                   ;;---
                   (and m p)
                   (recur (subvec v 5)
                          (conj r [(first v) (second v) (nth v m) (nth v p)]))
                   ;;---
                   ;; no model and no props
                   :else (recur (subvec v 2)
                                (conj r [(first v) (second v) nil nil]))))
                r))]
      (vec (mapcat (fn [[sym type model prop-map]]
                     [sym (if (and (seq? type) (= 'quote (first type)))
                            ;; type is a type-spec
                            `(g/create-element! ~(or model (first outs)) ~type ~prop-map)
                            ;; type is an expression
                            (if (or model prop-map)
                              (u/errorf "%s is initialized by expression %s where no model and prop-map are supported: %s"
                                        sym type [model prop-map])
                              type))])
                   v)))))

(defn ^:private disjunct-rules [rule-map]
  (seq (take-while (fn [x]
                     (when-not (or (symbol? x)
                                   (= x :as))
                       (u/errorf "Invalid :disjuncts spec (%s): %s" x (:disjuncts rule-map)))
                     (not= :as x))
                   (:disjuncts rule-map))))

(defn ^:private convert-rule [outs rule-map]
  (let [a-t-m (args-types-map (:from rule-map))
        arg-vec (vec (keys a-t-m))
        trace-src (if (= 1 (count arg-vec))
                    (first arg-vec)
                    arg-vec)
        create-vec (create-vector (:to rule-map) outs)
        created (mapv first (partition 2 create-vec))
        retval (if (= (count created) 1)
                 (first created)
                 created)
        existing (gensym "existing")
        [id id-exp] (:id rule-map)
        id-form (fn [body]
                  (if id
                    `(let [~id ~id-exp] ~body)
                    body))
        creation-form `(let ~create-vec
                         (swap! *trace* update-in [~(keyword (:name rule-map))]
                                assoc ~trace-src ~retval)
                         ~@(when (:id rule-map)
                             `[(swap! *trace* update-in [~(keyword (:name rule-map))]
                                      assoc ~id ~retval)])
                         ~@(:body rule-map)
                         ~retval)
        handle-cs-and-bs (fn [body]
                           (loop [cab (reverse (partition 2 (:cs-and-bs rule-map)))
                                  form body]
                             (if (seq cab)
                               (let [[k v] (first cab)]
                                 (recur (next cab)
                                        (condp = k
                                          :let `(let ~v ~form)
                                          :when-let `(let ~v
                                                       (when (and ~@(map first (partition 2 v)))
                                                         ~form))
                                          :when `(when ~v
                                                   ~form))))
                               form)))
        handle-type-constrs (fn [body]
                              (let [tcs (type-constrs a-t-m false)]
                                (if (seq tcs)
                                  `(when (and ~@tcs)
                                     ~body)
                                  body)))]
    `(~(:name rule-map) ~arg-vec
      ~(id-form
        (if-let [d (:disjuncts rule-map)]
          (let [drs (disjunct-rules rule-map)
                result-spec (let [r (take-last 2 d)]
                              (if (= :as (first r))
                                (second r)
                                (gensym "disj-rule-result")))
                handle-disj-calls (fn [body]
                                    `(when-let [~result-spec (or ~@(make-disjunct-rule-calls
                                                                    arg-vec drs))]
                                       ~@body
                                       ~result-spec))]
            (handle-type-constrs
             (handle-cs-and-bs
              (handle-disj-calls (:body rule-map)))))
          `(let [~existing (resolve-in ~(keyword (:name rule-map))
                                       ~(if (:id rule-map) id trace-src))]
             ~(if (and (:id rule-map)
                       (:dup-id-eval rule-map))
                `(if (and ~existing (not (contains? @*trace* ~trace-src)))
                   ~(handle-cs-and-bs
                     (if (seq created)
                       `(let ~[retval existing]
                          ~@(:body rule-map)
                          ~retval)
                       `(do ~@(:body rule-map))))
                   (when ~(type-constrs a-t-m true)
                     ~(handle-cs-and-bs creation-form)))
                `(or ~existing
                     ~(handle-type-constrs
                       (handle-cs-and-bs creation-form))))))))))

(defmacro deftransformation
  "Creates a model-to-model transformation named `name` with the declared
  input, output, input-output models, and further arguments in `args`.  For
  example,

    (deftransformation foo2bar [^:in in1 ^:in in2 ^:out out x y z]
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
      :id   [id [(aval a :name) x]]
      :dup-id-eval true
      :when (some-predicate? a)
      :when-let [v (some-fn a)]
      :let  [y (some-other-fn a x)
             z (some-other-fn2 a x y)]
      :to   [b 'OutClass
             c 'OutClass2
             d (a2d a)]
      (do-stuff-with a x v b y z b c d))

  :from declares the number and types of elements for which this rule is
  applicable.  Providing types is optional thus making it possible to have
  strings or numbers as input elements.  So the rule above has to be called
  with 2 arguments a and x, and the first one needs to be of metamodel type
  InClass.

  :id is a var-expression tuple.  The expression should compute an identity of
  the rule's input elements.  Traceability links are managed also from the
  elements' identity to the elements created in that rule.  :id is optional.
  By default, if the rule is called again with different elements that have the
  same identity, the results of the first evaluation are immediately returned.
  If :dup-id-eval is true and the rule is called with different elements that
  have the same identity, then the constraints are checked again, :let-bindings
  are established, the existing output elements are retrieved and bound to the
  vars in :to (without creating new elements nor setting properties as
  specified by the output elements' prop-maps), and then the body is evaluated
  again (with the new similar input elements and the old existing output
  elements).  This can be used to merge duplicate elements in the source to one
  canonical output element that subsumes the properties of all input elements.
  To distinguish if body is evaluated the first time or an additional time for
  a similar element, check if (resolve-in :rule-name id) returns nil.  If so,
  it's the first time.  Else, it's an additional time and you might want to use
  only add!-operations and no set!-operations.

  :when constrains the input elements to those satisfying some predicate.
  The :when clause is optional.

  :when-let has a vector of variable-expression pairs.  The expressions are
  evaluated and bound to the respective vars.  The rule may only be applied if
  all the vars are non-nil (which makes the \"when\"-part in :when-let).
  The :when-let is optional, and it is evaluated after :from and :when already
  matched.

  :let has a vector of variable-expression pairs.  The expressions are
  evaluated and bound to the respective vars.  The :let is evaluated
  after :when-let has matched, that is, the vars bound by :when-let may be used
  in the :let expressions, but not the other way round.

  :to is a vector of output elements (paired with their types) that are to be
  created.  As can be seen with the last example element d, an element may also
  be created by invoking another rule.

  If there are multiple output models, the :to spec may state in which model a
  given object has to be created, e.g.,

    :to [b 'OutClass  :in out1,
         c 'OutClass2 :in out2
         d (a2d a)]

  If there are multiple output models but an element in :to doesn't specify the
  target model explicitly, the first output model in the transformation's
  output model vector is used as a default.  Output elements created by other
  rules must not have an :in specification because that's the responsibility of
  the called rule (a2d, in the case above).

  The :to vector may also specify values for the newly created element's
  properties (attributes and references).  Those a specified using a map.

    :to [b 'OutClass  {:name \"Some Name\", ...},
         c 'OutClass2 {:name \"Other name\", :links b}
         d (a2d a)]

  If a target element specification contains both a property map and a :in
  spec, they may occur in any order.  Like the :in spec, property maps are not
  allowed for elements that are created by other rules such as the element d
  above which is created by the rule a2d.

  Following these special keyword-clauses, arbitrary code may follow, e.g., to
  set attributes and references of the newly created objects by calling other
  rules.

  A rule always returns the elements that where created for the given input
  elements (or rather their identity) in terms of the :to clause.  If the
  called rule's :to clause creates only one object, the result of a call is
  this object.  If its :to clause creates multiple objects, the result of a
  call is a vector of the created objects in the order of their declaration
  in :to.

  Disjunctive Rules
  =================

  Besides normal mapping rules, there are disjunctive rules.  Those have the
  following form:

    (x2y
      :from [x]
      :disjuncts [a2b c2d ... :as y]
      (optional-body-using y))

  Disjunctive rules mustn't have a :to clause, but :let/:when/:when-let are
  supported.  When a disjunctive rule is applied, it tries the given disjunct
  rules in the declared order.  The first one whose constraints and :from type
  match gets applied.  An optional :as clause may be the last thing in
  the :disjuncts vector.  If a disjunct rule could be applied, the result is
  bound to that clause's var and can be used in the optional body.  The spec
  may be a symbol or any destructuring form supported by let.

  Top-level Rules
  ===============

  A rule can be declared as top-level rule using ^:top metadata:

    (^:top a2b
       :from [a 'A]
       ;; same as above
       ...)

  When the transformation gets executed, all top-level rules are applied to
  matching elements automatically (unless there's a main function; see below).
  All other rules have to be called from the top-level rules explicitly.
  Top-level rules must have exactly one element declared in their :from clause.

  Functions
  =========

  Functions are just arbitrary local helpers.  They are to be defined using the
  syntax of function definitions in `letfn`, that is, they support for
  overloading, etc.

  There may be one special function called `main`.  This function must not have
  parameters.  If defined, it acts as the entry point to the transformation,
  i.e., it is called automatically and is responsible for calling the
  transformation's rules appropriately.  If there's a `main` function, the
  ^:top metadata attached to rules has no effect, that is, they are not called
  automatically anymore, but now that's the job of the `main` function.

  So you usually have either top-level rules or a `main` function.  The former
  is simpler and usually suffices while the latter provides more control to the
  user.  For example, a transformation needs to bind additional dynamic vars.
  In this case, defining a `main` function allows you to do so.

  Transformation Trace
  ====================

  The transformation function returns the trace of the transformation as a map
  of the form:

    {:rule1 {[in1 in2] out1, ...}
     :rule2 {in [out1 out2], ...}
     ...}

  In that example, it is obvious that rule1 creates just one target element for
  two given input elements, whereas rule2 creates two output elements for one
  given input element.  In other words, rule1 has 2 elements declared in :from
  and 1 element in :to, and for rule2 it's the other way round.

  Transformation Inheritance
  ==========================

  By providing :extends clause after the transformation's argument vector (a
  symbol or a vector of symbols denoting other transformations), one can
  declare the new transformation to extend one or many other transformations.

    (deftransformation foo2bar
     \"Transforms a foo model to a bar model.\"
      [^:in foo ^:out bar]
      :extends foo2bar-base
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
  advisable that extending transformations declare the very same parameters in
  the same order as the extended transformations, plus optionally some more
  parameters."

  {:arglists '([name [args] extends-clause? & rules-and-fns])}
  [name & more]
  (let [[name more] (tm/name-with-attributes name more)
        [args more] (if (vector? (first more))
                      [(first more) (next more)]
                      (u/errorf "Error: arg vector missing!"))
        [ins outs other-args] [(filter #(let [m (meta %)]
                                          (or (:in m) (:inout m)))
                                       args)
                               (filter #(let [m (meta %)]
                                          (or (:out m) (:inout m)))
                                       args)
                               (remove #(let [m (meta %)]
                                          (or (:in m) (:out m) (:inout m)))
                                       args)]
        [extended more] (if (= :extends (first more))
                          [(fnext more) (nnext more)]
                          [nil          more])
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     more)
        rules (apply om/ordered-map (mapcat (fn [r] [(first r) (rule-as-map r)]) rules))
        fns   (apply hash-map (mapcat (fn [f] [(first f) f]) fns))
        [rules fns] (if extended
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
                               (let [own-spec (q/the (vals (args-types-map
                                                            (:from rule-map))))
                                     specs (set (map (comp ct rule-by-name) disj-rules))]
                                 (or own-spec
                                     (if (= 1 (count specs))
                                       (first specs)
                                       (cons :or (vec specs)))))
                               ;; `the` is ok cause top-level rules have only one input element
                               (q/the (vals (args-types-map (:from rule-map))))))
        type-spec (let [specs (distinct (map collect-type-specs
                                             (vals top-rules)))]
                    (if (q/member? specs nil)
                      nil ;; Some rule is applicable to any elements, so we
                          ;; cannot restrict on type
                      (vec (cons :or specs))))
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
                         {;; The argument entries aren't actually used but just
                          ;; serve documentation and debugging purposes.
                          ::input-models `'~ins
                          ::output-models `'~outs
                          ::additional-args `'~other-args
                          ;; Those are used by transformation inheritance.
                          ::rules (list 'quote rules)
                          ::fns   (list 'quote fns)})
       [~@args]
       (binding [*trace* (atom (or *initial-trace* {}))]
         (letfn [~@(vals fns)
                 ~@rule-specs]
           ~@(if main-fn
               `[(~'main)]
               (for [m ins]
                 `(doseq [~elem-var (g/elements ~m ~type-spec)]
                    ~@(for [r (keys top-rules)]
                        `(~r ~elem-var)))))
           @*trace*)))))
