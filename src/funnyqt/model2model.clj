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

(defn ^:private convert-rule [outs rule]
  (let [m (rule-as-map rule)
        a-t-m (args-types-map (:from m))
        arg-vec (vec (keys a-t-m))
        create-vec (create-vector (:to m) outs)
        created (mapv first (partition 2 create-vec))
        retval (if (= (count created) 1)
                 (first created)
                 created)
        wl-vars (map first (partition 2 (:when-let m)))
        creation-form (when (seq created)
                        `(let ~create-vec
                           (swap! *trace* update-in [~(keyword (:name m))]
                                  assoc ~arg-vec ~retval)
                           ~@(:body m)
                           ~retval))
        wl-and-creation-form (if (:when-let m)
                               `(let ~(vec (:when-let m))
                                  (when (and ~@wl-vars)
                                    ~creation-form))
                               creation-form)
        when-wl-and-creation-form (if (:when m)
                                    `(when ~(or (:when m) true)
                                       ~wl-and-creation-form)
                                    wl-and-creation-form)]
    (when-let [uks (seq (disj (set (keys m))
                              :name :from :to :when :when-let :body :disjuncts))]
      (u/errorf "Unknown keys in rule: %s" uks))
    `(~(:name m) ~arg-vec
      ~(if-let [d (:disjuncts m)]
         (let [drs (disjunct-rules m)
               d (take-last 2 d)
               result-spec (if (= :result (first d))
                             (second d)
                             (gensym "disj-rule-result"))
               disj-calls-and-body `(let [r# (or ~@(make-disjunct-rule-calls arg-vec drs))]
                                      (when-let [~result-spec r#]
                                        ~@(:body m)
                                        r#))]
           `(when (and ~@(type-constrs a-t-m false)
                       ~(or (:when m) true))
              ~(if (:when-let m)
                 `(when-let ~(:when-let m)
                    ~disj-calls-and-body)
                 disj-calls-and-body)))
         `(or ~(make-trace-lookup arg-vec (:name m))
              (when ~(type-constrs a-t-m true)
                ~when-wl-and-creation-form))))))

(defmacro deftransformation
  "Creates a model-to-model transformation named `name`.

  `args` specifies the transformations input/output models.  It is a vector of
  input models and output models.  Both input and output are again vectors.
  E.g., a transformation receiving two input models and instantiating objects
  in one single output model would have the args [[in1 in2] [out]].

  In the rest of the transformation spec, rules and functions are defined.
  Functions are to be defined in the syntax of function definitions in
  `letfn`.

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

  Following these special keyword-clauses, arbitrary code may follow, e.g., to
  set attributes and references of the newly created objects.

  If there are multiple output models, the :to spec has to state in which model
  a given object has to be created, e.g.,

    :to [b 'OutClass  :model out1,
         c 'OutClass2 :model out2]

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

  In a rule's body, other rules may be called.  A rule always returns the
  elements that where created for a given input element in terms of the :to
  clause.  If the called rule's :to clause creates only one object, the result
  of a call is this object.  If its :to clause creates multiple objects, the
  result of a call is a vector of the created objects in the order of their
  declaration in :to.

  At least one rule has to be declared top-level using ^:top metadata:

    (^:top a2b
       :from [a 'A]
       ;; same as above
       ...)

  When the transformation gets executed, top-level rules are applied to
  matching elements automatically.  All other rules have to be called from the
  top-level rules explicitly.  Top-level rules must have exactly one element
  declared in their :from clause.

  The transformation function returns the trace of the transformation as a map
  of the form:

    {:rule1 {[in1 in2] out1, ...}
     :rule2 {[in] [out1 out2], ...}
     ...}

  In that example, it is obvious that rule1 creates just one target element for
  two given input elements, whereas rule2 creates two output elements for one
  given input element.  In other words, rule1 has 2 elements declared in :from
  and 2 elements in :to, and for rule2 it's the other way round."

  {:arglists '([name args & rules-and-fns])}
  [name & more]
  (let [[name more] (tm/name-with-attributes name more)
        [args rules-and-fns] (if (vector? (first more))
                               [(first more) (next more)]
                               (u/errorf "Error: arg vector missing!"))
        [ins outs] (let [[i o] args]
                     (cond
                      (nil? i) (u/errorf "No input models given.")
                      (nil? o) (u/errorf "No output models given.")
                      (not (vector? i)) (u/errorf "input models must be a vector.")
                      (not (vector? o)) (u/errorf "output models must be a vector.")
                      :else [i o]))
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     rules-and-fns)
        top-rules   (filter #(:top (meta (first %))) rules)
        rule-by-name (fn [n]
                       (let [rs (filter #(= n (first %)) rules)]
                         (cond
                          (empty? rs) (u/errorf "No such rule: %s" n)
                          (fnext rs)  (u/errorf "Multiple rules named %s: %s" n rs)
                          :else (first rs))))
        collect-type-specs (fn ct [r]
                             (let [m (rule-as-map r)]
                               (if-let [disj-rules (disjunct-rules m)]
                                 (let [specs (set (map (comp ct rule-by-name) disj-rules))]
                                   (if (= 1 (count specs))
                                     (first specs)
                                     (vec specs)))
                                 (remove nil? (vals (args-types-map (:from m)))))))
        type-spec (vec (cons :or (distinct (remove nil? (mapcat collect-type-specs top-rules)))))]
    (when-not (seq top-rules)
      (u/errorf "At least one rule has to be declared as top-level rule."))
    `(defn ~name ~(meta name)
       [~@ins ~@outs]
       (binding [*trace*            (atom {})]
         (letfn [~@fns
                 ~@(map (partial convert-rule outs) rules)]
           ~@(for [m ins]
               `(doseq [elem# (p/elements ~m ~type-spec)]
                  (doseq [r# ~(mapv first top-rules)]
                    (r# elem#))))
           @*trace*)))))
