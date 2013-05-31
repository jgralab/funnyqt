(ns funnyqt.declarative
  (:use [funnyqt.protocols        :only [has-type?]])
  (:use [funnyqt.utils            :only [errorf]])
  (:use [funnyqt.query            :only [member? xor]])
  (:require [flatland.ordered.map :as om])
  (:require [funnyqt.emf          :as emf])
  (:require [funnyqt.tg           :as tg])
  (:use [clojure.tools.macro      :only [name-with-attributes macrolet mexpand-all]]))

(defn ^:private rule? [form]
  (if-let [[name args & body] form]
    (let [m (apply hash-map (apply concat (partition 2 body)))]
      (and (or (contains? m :generalizes)
               (contains? m :from)
               (contains? m :to))
           (if (contains? m :generalizes)
             (and
               (or (vector? (:generalizes m))
                   (errorf "Error in %s: :generalizes must be a vector" form))
               (if (or (contains? m :from) (contains? m :to)
                         (contains? m :when-let))
                 (errorf "Error in %s: :generalize rules may have a :when clause but no :from, :to, or :when-let." form)
                 true))
             true)
           (if (xor (contains? m :from) (contains? m :to))
             (errorf "Error in %s: rules must contain :from and :to, %s."
                     form "or neither of both")
             true)
           (if (contains? m :to)
             (or (vector? (:to m))
                 (errorf "Error in %s: :to must be a vector." form))
             true)))
    (errorf "Error in %s: neither helper nor rule." form)))

(def ^{:dynamic true
       :doc "Actions deferred to the end of the transformation."}
  *deferred-actions*)

(def ^{:dynamic true
       :doc "A map {rule {input output}}."}
  *trace*)

(defmacro deferred
  "Captures a thunk (closure) that evaluates `body` as the last step of the
  transformation."
  [& body]
  `(swap! *deferred-actions* conj (fn [] ~@body)))

(defn ^:private rule-as-map [rule]
  (let [[name args & body] rule]
    (assoc (loop [b body, v []]
             (if (seq b)
               (if (keyword? (first b))
                 (recur (nnext b) (conj v (first b) (second b)))
                 (apply hash-map (conj v :body b)))
               (apply hash-map v)))
      :name name
      :args args)))

(defn ^:private make-lookups [arg where gens]
  (cons `(get ((deref *trace*) ~(keyword (name where))) ~arg)
        (map (fn [w] `(~w ~arg))
             gens)))

(defn ^:private type-constr [e t]
  (if t
    `(has-type? ~e ~t)
    true))

(defn ^:private create-vector [v outs]
  (let [v (loop [v v, r []]
            (if (seq v)
              (if (= (first (nnext v)) :model)
                (recur (nnext (nnext v)) (conj (into r (take 2 v))
                                               (outs (nth v 3))
                                               (nth v 3)))
                (recur (nnext v) (conj (into r (take 2 v))
                                       (outs (ffirst outs))
                                       (ffirst outs))))
              r))
        v (partition 4 v)]
    (vec (mapcat (fn [[sym type mk model]]
                   [sym (if (= mk :tg)
                          `(tg/create-vertex! ~model ~type)
                          `(emf/ecreate! ~model ~type))])
                 v))))

(defn ^:private convert-rule [outs rule]
  (let [m (rule-as-map rule)
        _ (when (> (count (:args m)) 1)
            (errorf "Error: Rules must have exactly one argument: %s" (:name m)))
        arg (first (:args m))
        create-vec (create-vector (:to m) outs)
        created (mapv first (partition 2 create-vec))
        retval (if (= (count created) 1)
                 (first created)
                 created)
        wl-vars (map first (partition 2 (:when-let m)))
        creation-form (when (seq created)
                        `(let ~create-vec
                           (swap! *trace* update-in [~(keyword (:name m))]
                                  assoc ~arg ~retval)
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
                              :name :args :from :to :when :when-let :body :generalizes))]
      (errorf "Unknown keys in declarative rule: %s" uks))
    `(~(:name m) ~(:args m)
      (when ~arg
        ~(if-let [gens (:generalizes m)]
           `(when ~(or (:when m) true)
              (or ~@(make-lookups arg (:name m) gens)))
           `(or ~@(make-lookups arg (:name m) nil)
                ;; type constraint & :when constraint
                (when ~(type-constr arg (:from m))
                  ~when-wl-and-creation-form)))))))

(defmacro deftransformation
  "Creates a declarative, ATL-like transformation named `name`.

  `args` specifies the transformations input/output models.  It is a vector of
  input models and output models.  Both input and output are again vectors of
  model specs.  A model spec is a name followed by a model kind (:emf or :tg).
  E.g., a transformation receiving two JGraLab TGraphs as input and
  instantiating objects in an output EMF model would have the args [[in1 :tg,
  in2 :tg] [out :emf]].

  In the rest of the transformation spec, rules and functions are defined.
  Functions are to be defined in the syntax of function definitions in
  `letfn`.

  Rules are defined similarily, but they are identified by several keywords.
  A plain mapping rule has the form:

    (a2b [a]
       :from 'InClass
       :when (some-predicate? a)
       :when-let [x (some-fn a)]
       :to [b 'OutClass, c 'OutClass2]
       (do-stuff-with a b c))

  :from declares the type of elements for which this rule is applicable.
  :when constraints the input elements to those satisfying some predicate.
  The :when clause is optional.
  :when-let receives a vector of variable-expr pairs.  The expressions are
  evaluated and bound to the respective vars.  The rule may only be applied if
  the vars are non-nil (which makes the \"when\"-part in :when-let).
  :to is a vector of output elements (paired with their types) that are to be
  created.  Following these special keyword-clauses, arbitrary code may follow,
  e.g., to set attributes and references of the newly created objects.

  If there are multiple output models, the :to spec has to state in which model
  a given object has to be created, e.g.,

    :to [b 'OutClass :model out1,
         c 'OutClass2 :model out2]

  A generalizing rule has the form

    (x2y [x]
      :generalizes [a2b c2d ...])

  Generalizing rules mustn't have :from/:to/:when-let clauses, but :when is
  supported.  When a generalizing rule is applied, it tries the specializing
  rules in the declared order.  The first one whose constraints and :from type
  match gets applied.

  In a rule's body, other rules may be called.  A rule always returns the
  elements that where created for a given input element.  If the called
  rule's :to clause creates only one object, the result of a call is this
  object.  If its :to clause creates multiple objects, the result of a call is
  a vector of the created objects in the order of their declaration in :to.

  At least one rule has to be declared top-level using ^:top metadata:

    (^:top a2b [a]
       ;; same as above
       ...)

  When the transformation gets executed, top-level rules are applied to
  matching elements automatically.  All other rules have to be called from the
  top-level rules explicitly.

  The transformation function returns the trace of the transformation as a map
  of the form:

    {:rule1 {input1 output1, ...}
     :rule2 {input [output1 output2], ...}
     ...}

  In that example, it is obvious that rule1 creates just one target element for
  a given input element, whereas rule2 creates two output elements per input
  element."

  {:arglists '([name args & rules-and-fns])}
  [name & more]
  (let [[name more] (name-with-attributes name more)
        [args rules-and-fns] (if (vector? (first more))
                               [(first more) (next more)]
                               (errorf "Error: arg vector missing!"))
        [ins outs] (let [i (first args)
                         o (second args)]
                     (cond
                      (nil? i) (errorf "No input models given.")
                      (nil? o) (errorf "No output models given.")
                      (not (vector? i)) (errorf "Error: input models must be a vector.")
                      (not (vector? o)) (errorf "Error: output models must be a vector.")
                      :else [(apply om/ordered-map i) (apply om/ordered-map o)]))
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     rules-and-fns)
        top-rules   (filter #(:top (meta (first %))) rules)
        rule-by-name (fn [n]
                       (let [rs (filter #(= n (first %)) rules)]
                         (cond
                          (empty? rs) (errorf "No such rule: %s" n)
                          (fnext rs)  (errorf "Multiple rules named %s: %s" n rs)
                          :else (first rs))))
        collect-type-specs (fn ct [r]
                             (let [m (rule-as-map r)]
                               (if-let [grs (:generalizes m)]
                                 (let [specs (set (map (comp ct rule-by-name) grs))]
                                   (if (= 1 (count specs))
                                     (first specs)
                                     (vec specs)))
                                 (:from m))))
        type-spec (vec (cons :or (distinct (remove nil? (map collect-type-specs top-rules)))))]
    (when-not (seq top-rules)
      (errorf "At least one rule has to be declared as top-level rule."))
    `(defn ~name ~(meta name)
       [~@(keys ins) ~@(keys outs)]
       (binding [*deferred-actions* (atom [])
                 *trace*            (atom {})]
         (letfn [~@fns
                 ~@(map (partial convert-rule outs) rules)]
           ~@(for [m (keys ins)
                   :let [kind (ins m)]]
               `(doseq [elem# ~(if (= :tg kind)
                                 `(tg/vseq ~m ~type-spec)
                                 `(emf/eallobjects ~m ~type-spec))]
                  (doseq [r# ~(mapv first top-rules)]
                    (r# elem#))))
           @*trace*)))))

