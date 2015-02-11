(ns funnyqt.pmatch
  "Pattern Matching."
  (:require clojure.set
            clojure.string
            [clojure.tools.macro       :as m]
            [clojure.data.priority-map :as pm]
            [funnyqt.generic           :as g]
            [funnyqt.query             :as q]
            [funnyqt.query.tg          :as qtg]
            [funnyqt.utils             :as u]
            [funnyqt.tg                :as tg]
            [funnyqt.emf               :as emf]))

;;# Pattern to pattern graph

(defn ^:private vertex-sym?
  "Returns [match id <type>] if sym is a vertex symbol."
  [sym]
  (and (symbol? sym)
       (re-matches #"([a-zA-Z0-9_]*)(<[a-zA-Z0-9._!]*>)?" (name sym))))

(defn ^:private edge-sym?
  "Returns [match arrow id <type> arrow] if sym is an edge symbol."
  [sym]
  (and
   (symbol? sym)
   (or (re-matches #"<-.*-" (name sym))
       (re-matches #"-.*->" (name sym)))
   (re-matches #"(<?-)([!a-zA-Z0-9_]*)(<[a-zA-Z0-9._!:]*>)?(->?)" (name sym))))

(defn ^:private name-and-type
  ([sym]
   (name-and-type sym nil))
  ([sym cur-edge]
   (when (and cur-edge (edge-sym? sym))
     (u/errorf "Dangling edge in pattern: %s" cur-edge))
   (if (or (vertex-sym? sym) (edge-sym? sym))
     (let [[_ s ^String t] (re-matches #"(?:<-|-)?([!a-zA-Z0-9_]+)?(?:<([a-zA-Z0-9._!:]*)>)?(?:-|->)?"
                                       (name sym))]
       [(and (seq s) (symbol s))
        (and (seq t) (if (.startsWith t ":")
                       (keyword (subs t 1))
                       (symbol t)))])
     (u/errorf "No valid pattern symbol: %s" sym))))

(defn ^:private neg-edge-sym? [sym]
  (and (edge-sym? sym)
       (= '! (first (name-and-type sym)))))

(defn ^:private edge-dir [esym]
  (if (edge-sym? esym)
    (if (re-matches #"<-.*" (name esym))
      :in
      :out)
    (u/errorf "%s is not edge symbol." esym)))

(def ^:private pattern-schema
  (tg/load-schema (clojure.java.io/resource "pattern-schema.tg")))

(defn ^:private binding-bound-vars
  "Returns the symbols bound by a :for, :let, :nested, or :when-let in pattern
  `p`.  Does not recurse into the expressions of the binding forms.  The
  symbols are returned as a vector of distinct elements in declaration order."
  [p]
  (loop [p p, r []]
    (if (seq p)
      (let [cur (first p)]
        (if (#{:for :let :when-let :nested} cur)
          (recur (nnext p) (let [sym-exprs (partition 2 (fnext p))]
                             (into r
                                   (mapcat (fn [[sym expr]]
                                             (cond
                                               (symbol? sym)
                                               [sym]
                                               ;; Constraints
                                               (= :when sym)
                                               nil
                                               ;; Bindings with :let
                                               (#{:when-let :let} sym)
                                               (map first (partition 2 expr))
                                               ;; Vector destructuring [a b]
                                               (vector? sym)
                                               sym
                                               ;; Map destructuring {a :a, b :b}
                                               (and (map? sym) (symbol? (first (keys sym))))
                                               (keys sym)
                                               ;; Map destructuring {:keys [a b] :as m}
                                               (and (map? sym) (keyword? (first (keys sym))))
                                               (let [syms (:keys cur)]
                                                 (if-let [as (:as cur)]
                                                   (conj syms as)
                                                   syms))
                                               ;;---
                                               :else (u/errorf "Cannot handle %s %s" sym expr)))
                                           sym-exprs))))
          (recur (next p) r)))
      (vec (distinct r)))))

(defn ^:private pattern-bound-vars
  "Returns the symbols bound by node and edge symbols in pattern `p`.  Does not
  recurse into :nested or :alternative clauses.  Substracts the symbols that
  are bound by a binding form cause those take precedence.  The symbols are
  returned as a vector of distinct elements in declaration order."
  [p]
  (loop [p p, r []]
    (if (seq p)
      (let [cur (first p)]
        (cond
          (vertex-sym? cur)
          (let [[_ id] (vertex-sym? cur)]
            (recur (next p) (if (seq id)
                              (conj r (symbol id))
                              r)))
          ;;---
          (edge-sym? cur)
          (let [[_ _ id] (edge-sym? cur)]
            (recur (next p) (if (seq id)
                              (conj r (symbol id))
                              r)))
          (#{:nested :when :while :when-let :for :let} cur)
          (recur (nnext p) r)
          ;;---
          :else (recur (next p) r)))
      ;; binding-bound-vars take precedence, so substract them.
      (let [bbv (into #{} (binding-bound-vars p))]
        (vec (remove bbv (distinct r)))))))

(defn ^:private used-vars
  "Returns the set of local variables (symbols) that are used in `form`.
  E.g., those are the dependencies the form needs to evaluate."
  [form]
  (cond
    ;; let & when-let & if-let & loop
    (and (seq? form)
         (or (= (first form) `let)
             (= (first form) `loop)
             (= (first form) `when-let)
             (= (first form) `if-let)))
    (set (concat (mapcat used-vars (map second (partition 2 (second form))))
                 (mapcat used-vars (nnext form))))
    ;; exclude quoted symbols
    (and (seq? form)
         (= (first form) 'quote))
    #{}
    ;; vectors & sets
    (or (vector? form)
        (set? form))
    (set (mapcat used-vars form))
    ;; funcalls
    (seq? form)
    (set (mapcat used-vars (rest form)))
    ;; symbols
    (and (symbol? form)
         (not (namespace form))) #{form}))

(defn ^:private vars-used-in-constr-or-binding
  "Returns the set of vars (symbols) that are used (not declared!) in the
  expressions of constraints and binding forms."
  [p]
  (loop [p p, r #{}]
    (if (seq p)
      (let [cur (first p)]
        (if (#{:let :for :when :while :when-let} cur)
          (let [[kw expr] p]
            (condp = kw
              ;; the constraint expression
              :when  (recur (nnext p) (into r (used-vars expr)))
              :while (recur (nnext p) (into r (used-vars expr)))
              ;; the exp in :when-let [var exp]
              :when-let (recur (nnext p) (into r (used-vars (second expr))))
              ;; the exps is :let/for [a exp1, b exp2, ...]
              (recur (nnext p) (into r (apply clojure.set/union
                                              (map (fn [[var exp]]
                                                     (used-vars exp))
                                                   (partition 2 expr)))))))
          (recur (next p) r)))
      r)))

(def ^:dynamic *pattern-expansion-context*
  "Defines the expansion context of a pattern, i.e., if a pattern expands into
  a query on a TGraph or an EMF model.  The possible values are :generic
  (default), :tg and :emf.

  The value :generic makes patterns expand into code that works for both EMF
  and TGraph models.  More precisely, the code only assumes that the
  INeighbors, IAdjacenciesInternal, and IElements protocols are extended upon
  the concrete model representation's classes.

  Note that the expansion of the :emf and :tg expansion context is slightly
  faster than the expansion of the :generic context.  Furthermore, the :tg
  expansion context allows for more expressive patterns in that edges may be
  match-bound (x<X> -e<E>-> y<Y>).  This is forbidden for :emf (since there are
  no first-class edges) and :generic.

  Usually, you won't bind this variable directly (using `binding`) but instead
  you specify the expansion context for a given pattern using the `attr-map` of
  a `defpattern` or `letpattern` form, or you declare the expansion context for
  a complete namespace using `:pattern-expansion-context` metadata for the
  namespace."
  :generic)

(defn ^:private get-and-remove-key-from-vector
  "Removes `key` from `v` and also the element after `key` if `get-val` is true.
  Returns [new-pattern key-or-val]."
  [v key get-val]
  (loop [nv [], ov v]
    (if (seq ov)
      (if (= key (first ov))
        (if get-val
          [(vec (concat nv (nnext ov))) (fnext ov)]
          [(vec (concat nv (next ov))) (first ov)])
        (recur (conj nv (first ov)) (rest ov)))
      [nv nil])))

(defn ^:private build-nested-pattern [pattern model-sym binding-var-set pattern-var-set]
  (let [pattern-meta (meta pattern)
        new-bindings (binding-bound-vars pattern)
        new-pattern-els (pattern-bound-vars pattern)
        argvec (into [model-sym]
                     (clojure.set/intersection
                      (clojure.set/union binding-var-set
                                         pattern-var-set)
                      (clojure.set/union (set new-bindings)
                                         (set new-pattern-els))))
        new-only-bindings (clojure.set/difference
                           (clojure.set/union (set new-bindings)
                                              (set new-pattern-els))
                           (clojure.set/union binding-var-set
                                              pattern-var-set))
        [pattern result-form] (get-and-remove-key-from-vector pattern :as true)
        result-form (or result-form
                        (zipmap (map keyword new-only-bindings)
                                new-only-bindings))]
    `((pattern ~(merge {:pattern-expansion-context *pattern-expansion-context*}
                       pattern-meta)
               ~argvec ~(conj pattern :as result-form))
      ~@argvec)))

(defn ^:private negative-spec-to-when-empty [negative-spec model-sym binding-var-set pattern-var-set]
  ;; negative-spec is [...neg-pattern...]
  [:when `(empty? ~(build-nested-pattern negative-spec model-sym binding-var-set pattern-var-set))])

(defn ^:private positive-spec-to-when-seq [positive-spec model-sym binding-var-set pattern-var-set]
  ;; positive-spec is [...pos-pattern...]
  [:when `(seq ~(build-nested-pattern positive-spec model-sym binding-var-set pattern-var-set))])

(defn ^:private logical-operator-spec-to-when-op-seq [operator pattern-specs model-sym binding-var-set pattern-var-set]
  ;; pattern-specs is [[p1] [p2] [p3]]
  (let [nested-patterns (map (fn [ps]
                               `(seq ~(build-nested-pattern ps model-sym binding-var-set pattern-var-set)))
                             pattern-specs)]
    [:when (case operator
             :and  `(and    ~@nested-patterns)
             :or   `(or     ~@nested-patterns)
             :xor  `(q/xor  ~@nested-patterns)
             :nand `(q/nand ~@nested-patterns)
             :nor  `(q/nor  ~@nested-patterns))]))

(defn ^:private alternative-spec-to-for [alternative-specs model-sym binding-var-set pattern-var-set]
  ;; alternative-specs is [[p1] [p2]...]
  (let [new-bindings (fn [p]
                       (let [new-bindings (set (into (binding-bound-vars p)
                                                     (pattern-bound-vars p)))]
                         (clojure.set/difference new-bindings binding-var-set pattern-var-set)))
        all-syms (vec (sort (set (mapcat new-bindings alternative-specs))))
        interpose-nils-for-undefined (fn [bindings all-syms]
                                       (vec (for [sym all-syms]
                                              (if (q/member? sym bindings)
                                                sym
                                                nil))))
        alternative-specs (for [aspec alternative-specs]
                            (do
                              (when (second (get-and-remove-key-from-vector aspec :as true))
                                (u/errorf "Alternative patterns mustn't have an :as clause! %s" aspec))
                              (conj aspec :as (interpose-nils-for-undefined (new-bindings aspec)
                                                                            all-syms))))
        patterns (map #(build-nested-pattern % model-sym binding-var-set pattern-var-set)
                      alternative-specs)]
    `[:for [~all-syms (q/no-dups (concat ~@patterns))]]))

(defn ^:private nested-specs-to-let [nested-specs model-sym binding-var-set pattern-var-set]
  ;; nested-specs is [np1 [...pattern...], np2 [...pattern...]]
  [:let (vec (mapcat
              (fn [[npvar np]]
                [npvar (build-nested-pattern np model-sym binding-var-set pattern-var-set)])
              (partition 2 nested-specs)))])

(defn ^:private get-name [elem]
  (when-let [n (tg/value elem :name)]
    (symbol n)))

(defn ^:private anon? [elem]
  (or (g/has-type? elem 'NegPatternEdge)
      (not (get-name elem))))

(defn ^:private get-type [elem]
  (when (g/has-type? elem '[APatternVertex PatternEdge
                            NegPatternEdge ArgumentEdge])
    (when-let [^String t (tg/value elem :type)]
      (if (.startsWith t ":")
        (read-string t)
        (symbol t)))))

(defn pattern-spec-to-pattern-graph [pname argvec pattern-spec isomorphic]
  (let [pname-str (if pname (name pname) "--anonymous--")
        argset (into #{} argvec)
        model-sym (first argvec)
        pg (let [g (tg/new-graph pattern-schema pname-str)]
             (tg/set-value! g :patternName pname-str)
             g)
        get-by-name (fn [n]
                      (when n
                        (first (filter #(= (name n) (tg/value % :name))
                                       (concat (tg/vseq pg 'APatternVertex)
                                               (tg/eseq pg '[PatternEdge ArgumentEdge]))))))
        get-or-make-v (fn [n t binding-var-set]
                        (let [[v iso-fn] (or (when-let [v (get-by-name n)]
                                               [v false])
                                             (let [v (tg/create-vertex!
                                                      pg (cond
                                                           (argset n)           'ArgumentVertex
                                                           (binding-var-set n)  'BindingVarVertex
                                                           :else                'PatternVertex)
                                                      {:name (when n (str n))})]
                                               [v (fn []
                                                    (when (and n isomorphic)
                                                      (when-let [constrs (seq (for [other (tg/vseq pg 'APatternVertex)
                                                                                    :when (not= other v)]
                                                                                `(not (identical? ~n ~(get-name other)))))]
                                                        (let [constr (tg/create-vertex! pg 'Constraint)]
                                                          (tg/set-value! constr :form (pr-str `[:when (and ~@constrs)]))
                                                          (tg/create-edge! pg 'Precedes v constr)))))]))]
                          (when t (tg/set-value! v :type (str t)))
                          [v iso-fn]))]
    (loop [pattern-spec pattern-spec
           lv (tg/create-vertex! pg 'Anchor)
           binding-var-set #{}
           pattern-var-set #{}]
      (when (seq pattern-spec)
        (let [cur (first pattern-spec)]
          (cond
            ;; Constraints and non-pattern binding forms
            (#{:when :while :let :when-let :for} cur)
            (let [v (tg/create-vertex! pg (condp = cur
                                            :when     'Constraint
                                            :while    'Constraint
                                            :when-let 'ConstraintAndBinding
                                            'Binding))]
              (binding [*print-meta* true]
                (tg/set-value! v :form
                               (if (= :for cur)
                                 (str (pr-str (fnext pattern-spec)) "]")
                                 (str "[" (str (pr-str cur)  " ")
                                      (pr-str (fnext pattern-spec)) "]"))))
              (tg/create-edge! pg 'Precedes lv v)
              (recur (nnext pattern-spec)
                     v
                     (into binding-var-set (binding-bound-vars [cur (fnext pattern-spec)]))
                     pattern-var-set))
            ;; Negative patterns: :negative [a --> b]
            (= :negative cur)
            (recur (vec (concat (negative-spec-to-when-empty (fnext pattern-spec) model-sym binding-var-set pattern-var-set)
                                (nnext pattern-spec)))
                   lv binding-var-set pattern-var-set)
            ;; Positive patterns: :positive [a --> b]
            (= :positive cur)
            (recur (vec (concat (positive-spec-to-when-seq (fnext pattern-spec) model-sym binding-var-set pattern-var-set)
                                (nnext pattern-spec)))
                   lv binding-var-set pattern-var-set)
            ;; Patterns with logical ops: :or [[a --> b] [a --> c]]
            (contains? #{:and :or :xor :nand :nor} cur)
            (recur (vec (concat (logical-operator-spec-to-when-op-seq
                                 cur (fnext pattern-spec) model-sym binding-var-set pattern-var-set)
                                (nnext pattern-spec)))
                   lv binding-var-set pattern-var-set)
            ;; Alternative patterns: :alternative [[a -<:x>-> b] [a -<:y>-> b]]
            (= :alternative cur)
            (let [translation (alternative-spec-to-for (fnext pattern-spec) model-sym binding-var-set pattern-var-set)]
              (recur (vec (concat translation (nnext pattern-spec)))
                     lv binding-var-set pattern-var-set))
            ;; Nested patterns: :nested [p1 [a --> b], p2 [a --> c]]
            (= :nested cur)
            (recur (vec (concat (nested-specs-to-let (fnext pattern-spec) model-sym binding-var-set pattern-var-set)
                                (nnext pattern-spec)))
                   lv binding-var-set pattern-var-set)
            ;; Edge symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (edge-sym? cur) (let [[n t] (name-and-type cur)
                                  nsym (second pattern-spec)
                                  [nvn nvt] (name-and-type nsym cur)
                                  [nv iso-fn] (get-or-make-v nvn nvt binding-var-set)]
                              (when (and (or (nil? t) (keyword? t)) (= :in (edge-dir cur)))
                                (u/errorf
                                 "References may only specified in forward direction but got %s"
                                 cur))
                              (let [e (apply tg/create-edge!
                                             pg (cond
                                                  (= '! n)   'NegPatternEdge
                                                  (argset n) 'ArgumentEdge
                                                  :else      'PatternEdge)
                                             (if (= :out (edge-dir cur))
                                               [lv nv]
                                               [nv lv]))]
                                (when (and n (not (g/has-type? e 'NegPatternEdge)))
                                  (tg/set-value! e :name (name n))
                                  (when isomorphic
                                    (doseq [other (tg/eseq pg '[:and APatternEdge !NegPatternEdge])
                                            :when (not= e other)
                                            :when (get-name other)
                                            :let [constr (tg/create-vertex!
                                                          pg 'Constraint
                                                          {:form (pr-str `[:when (not (identical?
                                                                                       ~n
                                                                                       ~(get-name other)))])})]]
                                      (tg/create-edge! pg 'Precedes nv constr))))
                                (when t (tg/set-value! e :type (str t))))
                              (when iso-fn (iso-fn))
                              (recur (nnext pattern-spec) nv binding-var-set
                                     (let [pvs pattern-var-set
                                           pvs (if n (conj pvs n) pvs)]
                                       (if nvn (conj pvs nvn) pvs))))
            ;; Vertex symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (vertex-sym? cur) (let [[n t] (name-and-type cur)
                                    [v iso-fn] (get-or-make-v n t binding-var-set)]
                                (when iso-fn
                                  (tg/create-edge! pg 'Precedes lv v)
                                  (iso-fn))
                                (recur (rest pattern-spec) v binding-var-set
                                       (if n
                                         (conj pattern-var-set n)
                                         pattern-var-set)))
            :else (u/errorf "Don't know how to handle pattern-spec part: %s" cur)))))
    ;; Remove Precedes edges which are parallel to APatternEdges.  Those are
    ;; the result of specs like [a<A> b<B> a --> b]
    (doseq [pv (tg/vseq pg 'APatternVertex)
            :let [target-incs-map (reduce (fn [m i]
                                            (update m (tg/that i) conj i))
                                          {} (tg/iseq pv nil :out))]]
      (doseq [[target incs] target-incs-map
              :let [ps (filter (g/type-matcher pg 'Precedes) incs)]]
        (doseq [p (if (= (count incs) (count ps))
                    (next ps)
                    ps)]
          (g/delete! p))))
    ;; Check for disconnected components.
    (let [vset (u/oset (tg/vseq pg))
          a (q/the (tg/vseq pg 'Anchor))
          reachables #(q/p-* % qtg/<->)]
      (when-let [disc (seq (clojure.set/difference vset (reachables a)))]
        (u/errorf "Disconnected nodes: %s" disc))
      ;; Suggest anchoring at argument nodes.
      (when-let [argv (seq (tg/vseq pg 'ArgumentVertex))]
        (let [find-first-pv (fn ffpv [a]
                              (if (g/has-type? a 'APatternVertex)
                                a
                                (first (map ffpv (map tg/that (tg/iseq a 'Precedes :out))))))]
          (when-not (g/has-type? (find-first-pv a) 'ArgumentVertex)
            (println
             (format "The pattern %s could perform better by anchoring it at an argument node."
                     (tg/value pg :patternName)))))))
    pg))

;;# Patter graph to pattern comprehension

(def ^:private ^:dynamic *conversion-opts* nil)

(defn ^:private conj-done [done & elems]
  (into done (mapcat #(if (tg/edge? %)
                        (vector % (tg/inverse-edge %))
                        (vector %))
                     elems)))

(defn ^:private anon-vec [start done]
  (loop [cur start, done done, vec []]
    (if (and cur (anon? cur))
      (cond
        (tg/edge? cur)   (recur (tg/that cur)
                                (conj-done done cur)
                                (conj vec cur))
        (tg/vertex? cur) (recur (let [ns (remove done (tg/iseq cur 'PatternEdge))]
                                  (if (> (count ns) 1)
                                    (u/errorf "Must not happen!")
                                    (first ns)))
                                (conj-done done cur)
                                (conj vec cur))
        :else (u/errorf "Unexpected %s." cur))
      (if cur
        (conj vec cur)
        vec))))

(defn ^:private init-queue [pg]
  (pm/priority-map (q/the (tg/vseq pg 'Anchor)) 0))

(defn ^:private enqueue-incs [cur queue done]
  (let [incs (remove (fn [i]
                       (or (done i)
                           (when-let [t (get-type i)]
                             (and (not (tg/normal-edge? i))
                                  (keyword? t)))))
                     (tg/iseq cur))]
    (into queue (map (fn [i] [i (tg/id i)]) incs))))

(defn ^:private do-anons [anon-vec-transformer-fn startsym av done]
  (let [target-node (last av)]
    (cond
      (anon? target-node)
      [:when `(seq ~(anon-vec-transformer-fn startsym av done))]
      ;;---
      (done target-node)
      `[:when (q/member? ~(get-name target-node)
                         (q/no-dups ~(anon-vec-transformer-fn startsym av done)))]
      ;;---
      (g/has-type? target-node 'ArgumentVertex)
      `[:when-let [~(get-name target-node)
                   (and ~(get-name target-node)
                        (q/member? ~(get-name target-node)
                                   (q/no-dups ~(anon-vec-transformer-fn startsym av done)))
                        ~(get-name target-node))]]
      ;;---
      (g/has-type? target-node '[:or PatternVertex PatternEdge])
      [(get-name target-node)
       `(q/no-dups ~(anon-vec-transformer-fn startsym av done))]
      ;;---
      :else (u/errorf "Don't know how to handle anon-vec %s." av))))

(defn ^:private deps-defined?
  "Returns true if all nodes defined before the COB cob have been processed."
  [cob done]
  (q/forall? done (map tg/that (tg/iseq cob 'Precedes :in))))

(defn ^:private check-undone [pg done]
  (when-let [undone-vertices (seq (remove done (tg/vseq pg)))]
    (u/errorf "Some vertices were't reached: %s" undone-vertices)))

;;** Models with first-class edges

;;*** TG

(defn pattern-graph-to-for+-bindings-tg [argvec pg]
  (let [gsym (first argvec)
        inc-dir (fn [e]
                  ;; -<SomeEC>-> and <-<SomeEC>- consider edge direction, but
                  ;; --> and -<:role>-> do not in order to stay compatible with
                  ;; the generic version.
                  (cond
                    (nil? (get-type e))     nil
                    (keyword? (get-type e)) nil
                    (tg/normal-edge? e)     :out
                    :else                   :in))
        inc-type (fn [e]
                   ;; -<_>-> accepts all edges of all ECs, and since an "edge
                   ;; class" name is given (the underscore), also the direction
                   ;; is considered.
                   (when-let [ec (get-type e)]
                     (if (= ec '_)
                       nil
                       ec)))
        anon-vec-to-for (fn [start-sym av done]
                          (let [[v r]
                                (loop [cs start-sym, av av, r []]
                                  (if (seq av)
                                    (let [el (first av)
                                          ncs (gensym)]
                                      (recur ncs
                                             (rest av)
                                             (if (tg/vertex? el)
                                               (into r `[:let [~ncs (tg/that ~cs)]
                                                         ;; When el is already
                                                         ;; done, its type is
                                                         ;; already checked,
                                                         ;; too.
                                                         ~@(when-let [t (and (not (done el))
                                                                             (get-type el))]
                                                             ;; We assume
                                                             ;; there's already
                                                             ;; a type-matcher
                                                             ;; named t
                                                             [:when `(~t ~ncs)])])
                                               (into r `[~ncs (tg/iseq ~cs ~(inc-type el)
                                                                       ~(inc-dir el))]))))
                                    [cs r]))]
                            (if (== 2 (count r))
                              (second r) ;; only one binding [G_NNNN exp]
                              `(u/for+ ~r ~v))))]
    (loop [queue (init-queue pg)
           done #{}
           bf []]
      (if (seq queue)
        (let [[cur cur-prio] (peek queue)]
          (if (done cur)
            (recur (pop queue) done bf)
            (g/type-case cur
              Anchor
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     bf)
              PatternVertex
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (into bf `[~(get-name cur) (tg/vseq ~gsym ~(get-type cur))]))
              ArgumentVertex
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (if (done cur)
                       bf
                       (into bf (let [bf-addon `[:when-let [~(get-name cur) ~(get-name cur)]]]
                                  (if (get-type cur)
                                    (into bf-addon
                                          ;; We assume there's already a
                                          ;; type-matcher named like the type.
                                          `[:when (~(get-type cur) ~(get-name cur))])
                                    bf-addon)))))
              BindingVarVertex ;; They're bound by ConstraintOrBinding
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (if (get-type cur)
                       ;; We assume there's already a type-matcher named like
                       ;; the type.
                       (into bf `[:when (~(get-type cur) ~(get-name cur))])
                       bf))
              ConstraintOrBinding
              (if (deps-defined? cur done)
                (recur (enqueue-incs cur (pop queue) done)
                       (apply conj-done done cur (tg/iseq cur 'Precedes :in))
                       (into bf (read-string (tg/value cur :form))))
                (u/errorf "Deps of %s are undefined." cur))
              PatternEdge
              (if (anon? cur)
                (let [av (anon-vec cur done)
                      target-node (last av)
                      done (conj-done done cur)
                      last-in-av (last av)]
                  (recur (enqueue-incs target-node (pop queue) done)
                         (let [done (apply conj-done done cur av)]
                           (if (tg/edge? last-in-av)
                             (conj-done done (tg/that last-in-av))
                             done))
                         (into bf (let [bindings (do-anons anon-vec-to-for (get-name (tg/this cur)) av done)]
                                    (if (tg/edge? last-in-av)
                                      (conj bindings :let
                                            [(get-name (tg/that last-in-av))
                                             `(tg/that ~(get-name last-in-av))])
                                      bindings)))))
                (let [trg (tg/that cur)
                      done (conj-done done cur)
                      av (if (anon? trg) (anon-vec trg done) nil)
                      last-in-av (last av)]
                  (recur (enqueue-incs trg (pop queue) done)
                         (let [done (apply conj-done done trg av)]
                           (if (tg/edge? last-in-av)
                             (conj-done done (tg/that last-in-av))
                             done))
                         (apply conj bf `~(get-name cur)
                                `(tg/iseq ~(get-name (tg/this cur)) ~(inc-type cur)
                                          ~(inc-dir cur))
                                (cond
                                  (done trg) `[:when (= ~(get-name trg) (tg/that ~(get-name cur)))]
                                  (anon? trg) (let [bindings (do-anons anon-vec-to-for
                                                                       (get-name cur)
                                                                       av done)]
                                                (if (tg/edge? last-in-av)
                                                  (conj bindings :let
                                                        [(get-name (tg/that last-in-av))
                                                         `(tg/that ~(get-name last-in-av))])
                                                  bindings))
                                  ;;---
                                  (g/has-type? trg 'ArgumentVertex)
                                  `[:when-let [~(get-name trg) ~(get-name trg)]
                                    ~@(when (get-type trg)
                                        ;; We assume there's already a
                                        ;; type-matcher named like the type.
                                        `[:when (~(get-type trg) ~(get-name trg))])
                                    :when (= ~(get-name trg) (tg/that ~(get-name cur)))]
                                  ;;---
                                  :else (concat
                                         [:let `[~(get-name trg) (tg/that ~(get-name cur))]]
                                         (when-let [t (get-type trg)]
                                           ;; We assume there's already a
                                           ;; type-matcher named like the type.
                                           `[:when (~t ~(get-name trg))])))))))
              ArgumentEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)]
                (recur (enqueue-incs trg (pop queue) done)
                       (conj-done done cur trg)
                       (vec
                        (concat
                         bf
                         (when-let [t (inc-type cur)]
                           (if (keyword? t)
                             (u/errorf "Argument edge type cannot be a keyword! %s" t)
                             ;; We assume there's already a type-matcher named
                             ;; like the type.
                             `[:when (~t ~(get-name cur))]))
                         `[:when (= ~(get-name src) (tg/this ~(get-name cur)))]
                         (cond
                           (done trg) `[:when (= ~(get-name trg) (tg/that ~(get-name cur)))]
                           ;;---
                           (g/has-type? trg 'ArgumentVertex)
                           `[:when-let [~(get-name trg) ~(get-name trg)]
                             ~@(when (get-type trg)
                                 ;; We assume there's already a type-matcher
                                 ;; named like the type.
                                 `[:when (~(get-type trg) ~(get-name trg))])
                             :when (= ~(get-name trg) (tg/that ~(get-name cur)))]
                           ;;---
                           :else (concat
                                  `[:let [~(get-name trg) (tg/that ~(get-name cur))]]
                                  (when-let [t (get-type trg)]
                                    ;; We assume there's already a type-matcher
                                    ;; named like the type.
                                    `[:when (~t ~(get-name trg))])))))))
              NegPatternEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)
                    done (conj-done done cur)]
                (if (done trg)
                  (recur (enqueue-incs trg (pop queue) done)
                         (conj-done done cur trg)
                         (into bf `[:when (not (q/exists?
                                                #(= ~(get-name trg) (tg/that %))
                                                (tg/iseq ~(get-name src) ~(inc-type cur)
                                                         ~(inc-dir cur))))]))
                  (recur (enqueue-incs trg (pop queue) done)
                         (conj-done done cur trg)
                         (into bf (if (anon? trg)
                                    (if-let [tt (get-type trg)]
                                      `[:when (empty? (filter
                                                       ;; We assume there's
                                                       ;; already a
                                                       ;; type-matcher named
                                                       ;; like the type.
                                                       #(~tt (tg/that %))
                                                       (tg/iseq ~(get-name src) ~(inc-type cur)
                                                                ~(inc-dir cur))))]
                                      `[:when (empty? (tg/iseq ~(get-name src) ~(inc-type cur)
                                                               ~(inc-dir cur)))])
                                    `[~(get-name trg) (tg/vseq ~gsym ~(get-type trg))
                                      :when (not (q/exists?
                                                  #(= ~(get-name trg) (tg/that %))
                                                  (tg/iseq ~(get-name src) ~(inc-type cur)
                                                           ~(inc-dir cur))))])))))
              Precedes
              (recur (assoc (pop queue) (tg/that cur) 0) (conj-done done cur) bf))))
        (do
          (check-undone pg done)
          bf)))))

;;** Models with only references/roles

(defn pattern-graph-to-for+-bindings-only-refs-base
  "Internal transformer function that given a pattern argument vector `argvec`,
  a pattern graph `pg` and an `elements-fn`, a `role-fn` and a `neighbors-fn`
  transforms the pattern graph to a comprehension binding form."
  [argvec pg elements-fn role-fn neighbors-fn]
  (let [gsym (first argvec)
        get-edge-type (fn [e]
                        (when-let [t (get-type e)]
                          (if (keyword? t)
                            t
                            (u/errorf "Reference name must be a keyword but was %s." t))))
        anon-vec-to-for (fn [start-sym av done]
                          (let [[v r]
                                (loop [cs start-sym, av av, r []]
                                  (if (seq av)
                                    (let [el (first av)
                                          ncs (if (tg/vertex? el) cs (gensym))]
                                      (recur ncs
                                             (rest av)
                                             (if (tg/vertex? el)
                                               ;; When el is already done, its
                                               ;; type is already checked, too.
                                               (into r (when-let [t (and (not (done el))
                                                                         (get-type el))]
                                                         ;; We assume there's
                                                         ;; already a
                                                         ;; type-matcher named
                                                         ;; like the type.
                                                         `[:when (~t ~ncs)]))
                                               (into r `[~ncs ~(if-let [t (get-edge-type el)]
                                                                 `(~role-fn ~cs ~t)
                                                                 `(~neighbors-fn ~cs))]))))
                                    [cs r]))]
                            (if (== 2 (count r))
                              (second r) ;; only one binding [G_NNNN exp]
                              `(u/for+ ~r ~v))))]
    ;; Check there are only anonymous edges.
    (when-not (every? anon? (tg/eseq pg 'APatternEdge))
      (u/errorf "Edges mustn't be named with models with only refs: %s"
                (mapv g/describe (remove anon? (tg/eseq pg 'APatternEdge)))))
    (loop [queue (init-queue pg)
           done #{}
           bf []]
      (if (seq queue)
        (let [[cur cur-prio] (peek queue)]
          (if (done cur)
            (recur (pop queue) done bf)
            (g/type-case cur
              Anchor
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     bf)
              PatternVertex
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (into bf `[~(get-name cur) (~elements-fn ~gsym ~(get-type cur))]))
              ArgumentVertex
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (if (done cur)
                       bf
                       (into bf (let [bf-addon `[:when-let [~(get-name cur) ~(get-name cur)]]]
                                  (if (get-type cur)
                                    (into bf-addon
                                          ;; We assume there's already a
                                          ;; type-matcher named like the type.
                                          `[:when (~(get-type cur) ~(get-name cur))])
                                    bf-addon)))))
              BindingVarVertex ;; Actually bound by ConstraintOrBinding
              (recur (enqueue-incs cur (pop queue) done)
                     (conj-done done cur)
                     (if (get-type cur)
                       ;; We assume there's already a type-matcher named like
                       ;; the type.
                       (into bf `[:when (~(get-type cur) ~(get-name cur))])
                       bf))
              ConstraintOrBinding
              (if (deps-defined? cur done)
                (recur (enqueue-incs cur (pop queue) done)
                       (apply conj-done done cur (tg/iseq cur 'Precedes :in))
                       (into bf (read-string (tg/value cur :form))))
                (u/errorf "Deps of %s are undefined." cur))
              PatternEdge
              (if (anon? cur)
                (let [av (anon-vec cur done)
                      target-node (last av)
                      done (conj-done done cur)]
                  (recur (enqueue-incs target-node (pop queue) (apply conj-done done av))
                         (apply conj-done done cur av)
                         (into bf (do-anons anon-vec-to-for
                                            (get-name (tg/this cur)) av done))))
                (u/errorf "Edges cannot be match-bound with models with just refs: %s" (g/describe cur)))
              NegPatternEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)
                    done (conj-done done cur)]
                (if (done trg)
                  (recur (enqueue-incs trg (pop queue) done)
                         (conj-done done cur trg)
                         (into bf `[:when (not (q/member? ~(get-name trg)
                                                          ~(if-let [t (get-edge-type cur)]
                                                             `(~role-fn ~(get-name src) ~t)
                                                             `(~neighbors-fn ~(get-name src)))))]))
                  (recur (enqueue-incs trg (pop queue) done)
                         (conj-done done cur trg)
                         (into bf (if (anon? trg)
                                    (if-let [tt (get-type trg)]
                                      `[:when (empty? (filter
                                                       ;; We assume there's
                                                       ;; already a
                                                       ;; type-matcher named
                                                       ;; like the type.
                                                       #(~tt %)
                                                       ~(if-let [t (get-edge-type cur)]
                                                          `(~role-fn ~(get-name src) ~t)
                                                          `(~neighbors-fn ~(get-name src)))))]
                                      `[:when (empty? ~(if-let [t (get-edge-type cur)]
                                                         `(~role-fn ~(get-name src) ~t)
                                                         `(~neighbors-fn ~(get-name src))))])
                                    `[~(get-name trg) (~elements-fn ~gsym ~(get-type trg))
                                      :when (not (q/member? ~(get-name trg)
                                                            ~(if-let [t (get-edge-type cur)]
                                                               `(~role-fn ~(get-name src) ~t)
                                                               `(~neighbors-fn ~(get-name src)))))])))))
              ArgumentEdge
              (u/errorf "There mustn't be argument edges for models with just refs: %s" (g/describe cur))
              Precedes
              (recur (assoc (pop queue) (tg/that cur) 0) (conj-done done cur) bf))))
        (do
          (check-undone pg done)
          bf)))))

;;*** EMF

(defn eget-1
  "Only for internal use."
  [eo r]
  (when-let [x (emf/eget-raw eo r)]
    (if (instance? java.util.Collection x)
      x
      ;; Use set literal so that the resulting coll is unique according to
      ;; utils/unique-coll?.
      #{x})))

(defn pattern-graph-to-for+-bindings-emf [argvec pg]
  (pattern-graph-to-for+-bindings-only-refs-base argvec pg `emf/eallcontents `eget-1 `emf/erefs))

;;*** Generic

(defn pattern-graph-to-for+-bindings-generic [argvec pg]
  (pattern-graph-to-for+-bindings-only-refs-base argvec pg `g/elements `g/adjs `g/neighbors))

;;** defpattern and friends

(defn ^:private argvec-to-hash-map
  "Converts an argument vector [a b c] to a map {:a a, :b b, :c c}."
  [argvec]
  (zipmap (map #(keyword (name %)) argvec)
          argvec))

(defn bindings-to-argvec
  "Rips out the symbols declared in `bindings`.
  `bindings` is a binding vector with the syntax of `for`."
  [bindings]
  (loop [p bindings, l []]
    (if (seq p)
      (cond
        ;; Handle :let [x y, [u v] z]
        (or (= :let (first p))
            (= :for (first p))
            (= :when-let (first p)))
        (recur (rest (rest p))
               (vec (concat l
                            (loop [ls (first (rest p)) bs []]
                              (if (seq ls)
                                (recur (rest (rest ls))
                                       (let [v (first ls)]
                                         (if (coll? v)
                                           (into bs v)
                                           (conj bs v))))
                                bs)))))
        ;; Ignore :when (exp ...) and :while (exp ...)
        (#{:when :while} (first p)) (recur (nnext p) l)
        ;; A vector destructuring form
        (vector? (first p)) (recur (nnext p) (vec (concat l (remove #(= % '&) (first p)))))
        ;; A map destructuring form: {a :a, b :b} or {:keys [a b c] :or {:a 1} :as m}
        (map? (first p)) (recur (nnext p)
                                (vec (concat l
                                             (cond
                                               (symbol? (first (keys (first p))))
                                               (keys (first p))
                                               ;;---
                                               (keyword? (first (keys (first p))))
                                               (let [syms (:keys (first p))]
                                                 (if-let [as (:as (first p))]
                                                   (conj syms as)
                                                   syms))
                                               ;;---
                                               :else (u/errorf "Cannot handle %s" (first p))))))
        ;; That's a normal binding
        (symbol? (first p)) (recur (rest (rest p)) (conj l (first p)))
        ;; Ups, what's that???
        :else (u/errorf "Cannot handle %s" (first p)))
      (vec (distinct l)))))

(def pattern-graph-transform-function-map
  "A map from techspace to pattern graph transformers."
  {:emf     pattern-graph-to-for+-bindings-emf
   :tg      pattern-graph-to-for+-bindings-tg
   :generic pattern-graph-to-for+-bindings-generic})

(defn ^:private replace-id
  ([new-id name old-id type]
   (symbol (str new-id type)))
  ([new-id name head old-id type tail]
   (symbol (str head new-id type tail))))

(defn ^:private replace-ids-in-bindings [bindings rmap]
  (vec (mapcat (fn [[sym exp]]
                 (if-let [new-id (get rmap (keyword sym))]
                   [new-id exp]
                   [sym exp]))
               (partition 2 bindings))))

(defn ^:private normalize-extended [extended]
  ;; A single extended has one of the following forms:
  ;;   - some-pattern     ; extend some-pattern
  ;;   - (some-pattern)   ; ditto
  ;;   - (some-pattern 2) ; extend 3rd pattern of some-pattern which has
  ;;                      ; multiple arities.
  ;;   - (some-pattern :a a1 :b b1) ; extend some-pattern with renamings.
  ;;   - (some-pattern 2 :a a1) ; extend 3rd pattern of some-pattern with
  ;;                            ; renamings.
  (let [[name & more] (if (symbol? extended)
                        [extended]
                        extended)
        [num keyvals] (if (integer? (first more))
                        [(first more) (next more)]
                        [0 more])
        replace-map (apply hash-map keyvals)]
    [name num replace-map]))

(declare get-extended-pattern)
(defn ^:private rename-extended [pattern rmap]
  ;; rmap is {:oldId newId, ...}
  (loop [p pattern, r []]
    (if (seq p)
      (let [cur (first p)]
        (cond
          ;; Vertex syms
          (vertex-sym? cur)
          (let [[_ oid _ :as match] (vertex-sym? cur)]
            (recur (next p)
                   (conj r (if (contains? rmap (keyword oid))
                             (apply replace-id (get rmap (keyword oid)) match)
                             cur))))
          ;; Edge syms
          (edge-sym? cur)
          (let [[_ _ oid _ _ :as match] (edge-sym? cur)]
            (recur (next p)
                   (conj r (if (contains? rmap (keyword oid))
                             (apply replace-id (get rmap (keyword oid)) match)
                             cur))))
          ;; :let/:when-let/:for bindings
          (#{:let :when-let :for} cur)
          (recur (nnext p)
                 (into r [cur (replace-ids-in-bindings (fnext p) rmap)]))
          ;; Transitive :extends
          (= :extends cur)
          (recur (vec (concat (mapcat get-extended-pattern (fnext p)) (nnext p)))
                 r)
          ;; Else
          :else (recur (next p) (conj r cur))))
      r)))

(defn ^:private remove-isomorphic-distinct-and-as-specs [pattern-spec]
  (-> pattern-spec
    (get-and-remove-key-from-vector :isomorphic false)
    first
    (get-and-remove-key-from-vector :distinct false)
    first
    (get-and-remove-key-from-vector :as true)
    first))

(def ^:dynamic ^:private *letpattern-pattern-specs*
  "A map from letpattern-defined patterns (symbols) to their vector of pattern
  specifications.  Bound during compilation to expand :extends clauses."
  {})

(defn ^:private get-extended-pattern [extended]
  (let [[name num replace-map] (normalize-extended extended)]
    (if-let [pattern-specs-or-var (or (get *letpattern-pattern-specs* name)
                                      (resolve name))]
      (if-let [pattern-specs (if (var? pattern-specs-or-var)
                               (::pattern-specs (meta pattern-specs-or-var))
                               pattern-specs-or-var)]
        (if-let [pattern-spec (get pattern-specs num)]
          (rename-extended (remove-isomorphic-distinct-and-as-specs pattern-spec) replace-map)
          (u/errorf "No %. pattern spec for extended pattern %s." num name))
        (u/errorf "No pattern specs for extended pattern %s." name))
      (u/errorf "Cannot resolve extended pattern %s." name))))

(defn ^:private merge-extends [pattern]
  (loop [p pattern, r []]
    (if (seq p)
      (if (= :extends (first p))
        (recur (nnext p) (vec (concat r (mapcat get-extended-pattern (fnext p)))))
        (recur (next p) (conj r (first p))))
      r)))

(defn ^:private transform-pattern-spec
  "Transforms patterns like [a<X> -<role>-> b<Y>] to a binding vector suitable
  for `funnyqt.utils/for+`.  That vector contains metadata :distinct, :as, and
  also the pattern graph as :pattern-graph.

  (Only for internal use.)"
  [name pattern-spec args]
  (let [[pattern-spec distinct]   (get-and-remove-key-from-vector pattern-spec :distinct false)
        [pattern-spec result]     (get-and-remove-key-from-vector pattern-spec :as       true)
        [pattern-spec isomorphic] (get-and-remove-key-from-vector pattern-spec :isomorphic false)
        pattern-spec (merge-extends pattern-spec)
        pgraph (pattern-spec-to-pattern-graph name args pattern-spec isomorphic)
        transform-fn (pattern-graph-transform-function-map *pattern-expansion-context*)]
    (if transform-fn
      (binding [*conversion-opts* (assoc (meta name)
                                         :distinct distinct)]
        (with-meta (transform-fn args pgraph)
          {:distinct      distinct
           :as            result
           :pattern-graph pgraph}))
      (u/errorf "The pattern expansion context is not set.\n%s"
                "See `*pattern-expansion-context*` in the pmatch namespace."))))


(defn ^:private get-and-remove-constraint-from-vector
  "Removes all constraints depending only on the syms in set `deps` from `v`.
  Returns [nv constrs] where nv is the new vector and constrs is the seq of
  constraints depending only on `deps`."
  [v deps]
  (loop [v v, nv [], cs []]
    (if-let [[x y & r] (seq v)]
      (if (and (= :when x) (clojure.set/subset? (used-vars y) deps))
        (recur r nv (conj cs y))
        (recur r (conj nv x y) cs))
      [nv cs])))

(defn ^:private create-type-matchers [model-var pgraph]
  (into []
        (mapcat (fn [t]
                  (when (and (symbol? t)
                             (not= t '_))
                    `[~t (g/type-matcher ~model-var '~t)])))
        (distinct
         (map get-type
              (concat (tg/vseq pgraph 'APatternVertex)
                      (tg/eseq pgraph 'APatternEdge))))))

(defn ^:private convert-spec [name args-and-pattern]
  (when (> (count args-and-pattern) 2)
    (u/errorf "Pattern %s has too many components (should have only args and pattern vector)." name))
  (let [[args pattern-spec] args-and-pattern]
    (when-not (and (vector? args) (vector? pattern-spec))
      (u/errorf "Pattern %s is missing the args or pattern vector. Got %s." name args-and-pattern))
    (let [bf (transform-pattern-spec name pattern-spec args)
          result-form (or (when-let [result-form (:as (meta bf))]
                            (condp = result-form
                              :vector (bindings-to-argvec bf)
                              :map (argvec-to-hash-map
                                    (bindings-to-argvec bf))
                              result-form))
                          (argvec-to-hash-map
                           (bindings-to-argvec bf)))]
      `(~args
        (let [~@(create-type-matchers (first args) (:pattern-graph (meta bf)))]
          ~(if (and (:eager (meta name))
                    (not (:sequential (meta name)))
                    (<= 2 (.availableProcessors (Runtime/getRuntime)))
                    ;; The first binding is :when-let [p p] if the pattern starts
                    ;; with an argument vertex p.  In that case, we can't
                    ;; parallelize.
                    (not (keyword? (first bf)))
                    (>= (count (bindings-to-argvec bf)) 2))
             ;; Eager, Parallel Case
             (let [[sym expr & rbf] bf
                   default-min-partition-size 16
                   default-partitions-per-cpu 32
                   [MIN-PARTITION-SIZE PARTITIONS-PER-CPU] (cond
                                                             (vector? (:eager (meta name)))
                                                             (let [[mps ppc] (:eager (meta name))]
                                                               [(or mps default-min-partition-size)
                                                                (or ppc default-partitions-per-cpu)])
                                                             ;;---
                                                             (true? (:eager (meta name)))
                                                             [default-min-partition-size
                                                              default-partitions-per-cpu]
                                                             ;;---
                                                             :else (u/errorf
                                                                    (str ":eager option must be true or "
                                                                         "a vector of the form "
                                                                         "[MIN-PARTITION-SIZE PARTITIONS-PER-CPU] "
                                                                         "but was %s")
                                                                    (:eager (meta name))))
                   rbf (vec rbf)
                   [rbf constraints] (get-and-remove-constraint-from-vector rbf #{sym})
                   vectorvar (gensym "vector")
                   combinevar (gensym "combine!")
                   chm (with-meta (gensym "chm") {:tag 'java.util.concurrent.ConcurrentHashMap})]
               `(let [~vectorvar (into []
                                       ~@(when (seq constraints)
                                           [`(filter (fn [~sym] (and ~@constraints)))])
                                       ~expr)
                      ;; We want to have about 20 packages of work per CPU, e.g.,
                      ;; we want to have 20 * #CPUs partitions.  This means the
                      ;; vector has to be partitioned into partitions of size
                      ;; #vector / (20 * #CPUs).  That allows to keep all cores
                      ;; busy if some partitions are much more expensive than
                      ;; others.  However, partitions mustn't be too small, too.
                      ;; Else the parallelization suffers from the coordination
                      ;; overhead.
                      n# (max ~MIN-PARTITION-SIZE
                              (long (/ (count ~vectorvar)
                                       (* ~PARTITIONS-PER-CPU
                                          (.availableProcessors (Runtime/getRuntime))))))
                      ~@(when (:distinct (meta bf))
                          `[~chm (java.util.concurrent.ConcurrentHashMap.
                                  (* ~(count (bindings-to-argvec rbf)) (count ~vectorvar))
                                  0.75 (.availableProcessors (Runtime/getRuntime)))])
                      ~combinevar ~(if (:distinct (meta bf))
                                     `(constantly ~chm)
                                     `clojure.core.reducers/cat)
                      reduce!# ~(if (:distinct (meta bf))
                                  `(fn [_# ~sym]
                                     (u/doseq+ ~rbf (.putIfAbsent ~chm ~result-form Boolean/TRUE))
                                     ~chm)
                                  `(fn [coll# ~sym]
                                     (~combinevar coll# (u/for+ ~rbf ~result-form))))
                      finalize# ~(if (:distinct (meta bf))
                                   `(fn [~chm] (sequence (.keySet ~chm)))
                                   `sequence)]
                  (finalize#
                   (clojure.core.reducers/fold n# ~combinevar reduce!# ~vectorvar))))
             ;; Lazy Case
             (let [code `(u/for+ ~bf ~result-form)
                   code (if (:distinct (meta bf))
                          `(q/no-dups ~code)
                          code)]
               (if (:eager (meta name))
                 `(doall ~code)
                 `(sequence ~code)))))))))

(defmacro defpattern
  "Defines a pattern with `name`, optional `doc-string`, optional `attr-map`,
  an `args` vector, and a `pattern-spec` vector.  The first argument in `args`
  must denote the model which the pattern is applied to.

  When applied to a model, it returns the sequence of all matches.  By default,
  this seq is a lazy seq.

  Node and Edge Symbols
  =====================

  In the simplest case, `pattern-spec` contains only symbols for nodes and
  edges.

    v<V>            ; A node of type V identified as v
    v<V> -<:e>-> v  ; A node v of type V referencing itself with an e-reference

  V is a qualified name of a node type.  Edge types are either reference names
  given as keywords or qualified edge type names in case the model
  representation has first-class edges.  If it does, the edges can also be
  matched and added to the match results by adding an identifier.

  Every edge symbol in a pattern must have a node symbol at its start and at
  its end.

    v<V> -e<E>-> v

  Anonymous Node and Edge Symbols
  ===============================

  Both the identifiers and the types enclosed in angle brackets are optional.
  So this is a valid pattern, too.

    [v --> <V> -<:e>-> <> --> x<X>]

  This pattern matches an arbitrary node v that is connected to an X-node x via
  some arbitrary reference leading to some V-node from which an e-reference
  leads some arbitrary other node from which another arbitrary reference leads
  to x.  There may be many such paths between v and x but anonymous nodes and
  edges specify only the existence of a matching node, edge, or path.

  Argument Node and Edge Symbols
  ==============================

  Patterns may also include the arguments given to the defpattern, in which
  case those are assumed to be bound to one single node or edge, depending on
  their usage in the pattern, e.g., arg must be a node and -arg-> must be an
  edge.

  For example, the following pattern receives an argument s which according to
  the usage in a pattern must be a node.

    (defpattern foobar [m s] [s --> t])

  The pattern returns matches of the form {:s s, :t t} where s is the node
  given as argument and t is a node referenced from s.

  Constraints
  ===========

  Patters may further include arbitrary constraints that must hold for a valid
  match using :when clauses:

    [v --> w
     :when (pred1? v)
     :when (not (pred2? w))]

  Negative Edges
  ==============

  Patterns may contain negative edges indicated by edge symbols with name !.
  Those must not exist for a match to succeed.  For example, the following
  declares that there must be a foo reference from v to w, but w must have no
  outgoing edges at all, and v must not reference w with its bar reference.

    [v -<:foo>-> w -!-> <>
     v -!<:bar>-> w]

  Isomorphic Matching
  ===================

  By default, FunnyQT performs homomorphic matching, i.e., in a pattern

    [v1<V> -<E>-> v2<V>]

  it is possible that both v1 and v2 are matched to the same node in the host
  graph in case an E-edge starts and ends at that node.  This behavior can be
  changed to isomorphic matching using the :isomorphic keyword.

    [v1<V> -<E>-> v2<V> :isomorphic]

  Here, v1 and v2 are guaranteed to be matched to different nodes.  This
  behavior applies to all non-anonymous nodes and edges in the pattern but not
  to local bindings and comprehension bindings (which see below).

  Local Bindings
  ==============

  Moreover, a pattern may bind further variables using :let and :when-let which
  also become part of the matches.

    [v --> w
     :let [a (foo v), b (bar v)]
     :when-let [c (baz w)]]

  Hereby, the variables bound by :let (a and b) are taken as-is whereas the
  variables bound by :when-let must be logically true in order to match.  That
  is, in the example above, a and b could be nil, but c has to be logically
  true (i.e., not nil and not false) for a match to occur.  a, b, and c are
  also part of the matches.

  Comprehension Bindings
  ======================

  Patterns may also contain usual comprehension binding forms using :for, i.e.,
  pairs of variables and expressions.

    [v --> w
     :for [u (p-seq w [p-+ [p-alt <>-- :someRef]])]]

  Again, u is also part of the matches.

  Pattern Inheritance
  ===================

  Patterns can be composed of other named patterns bound by either defpattern
  or letpattern using :extends clauses.

    (defpattern a-A [m] [a<A>])
    (defpattern b-B [m] [b<B>])
    (defpattern a-b2 [m] [:extends [a-A, (b-B :b b2)]
                          a --> b2])

  In the example above, the pattern a-b2 extends a-A, which means a-A's pattern
  is also part of a-b2's pattern.  It also extends b-B, however, the node named
  b in b-B should be named b2 in a-b2.  The pattern a-b2 is completely
  equivalent to the following definition.

    (defpattern a-b2* [m] [a<A> b2<B> a --> b2])

  Extend clauses may be transitive, and there may be multiple :extends clauses
  in a pattern.

  Extending patterns may also override the types of elements of the extended
  patterns.  For example,

    (defpattern b1-b2 [m] [:extends [(a-b2 :a b1)]
                           b1<B>]

  defines that b1-b2 extends a-b2 thereby renaming a to b1.  Additionally, b1's
  type is defined to be B instead of A as defined by a-b2.

  In case an extended pattern is overloaded on arity, the :extends clause may
  specify which pattern to take using [:extends [(some-pattern 1)]] where the 1
  denotes the pattern of the second version.  0, i.e., the first version, is
  the default.  After this index, the renamings follow.

  The modifier keywords :isomorphic and :distinct as well as :as-clauses are
  not propagated from extended to extending patterns, thus it is up to the
  extending pattern to specify those if needed.

  Negative Patterns
  =================

  A pattern may include arbitrarily many negative patterns which implement
  negative application conditions (NACs).  In order for the pattern to match,
  all included negative patterns must not match.

    [b<B>
     :negative [b -<:t>-> c1<C> -<:t>-> a<A>
                b -<:t>-> c2<C> -<:t>-> a
                :isomorphic]]

  This pattern matches all B-nodes b which are not connected to two different
  C-nodes c1 and c2 that both reference the same A-node a.

  If a negative pattern is not connected to the surrounding pattern, then it
  acts as a global NAC.  Note that this is generally not a good idea since it
  will be checked for every match of the surrounding pattern.  That is, every
  check except the first one is needless effort unless the model is modified
  from another thread so that the value of the NAC might change in between.

  Positive Patterns
  =================

  A patter may include arbitrarily many positive patters which implement
  positive application conditions (PACs).  In order for the pattern to match,
  all included positive patterns must also match.  The main difference between
  PACs and including the PAC in the surrounding pattern is that the nodes and
  edges of the PACs are not part of the matches, and the number of the PACs
  matches also don't contribute to the number of matches of the surrounding
  pattern.

    [b<B>
     :negative [b -<:t>-> c1<C> -<:t>-> a<A>
                b -<:t>-> c2<C> -<:t>-> a
                :isomorphic]]

  This pattern matches all B-nodes b which are connected to two different
  C-nodes c1 and c2 that both reference the same A-node a.

  If a positive pattern is not connected to the surrounding pattern, then it
  acts as a global PAC.  Like with negative patterns, this is probably not a
  good idea since the PACs are checked for every match of the surrounding
  pattern so it is needless effort in case the value of the PAC cannot change
  in the mean-time.

  Also note that if two nodes of the outer pattern are only connected by
  elements in a PAC, there is a serious performance penalty because first the
  outer pattern is matched (with quadratic effort for two unconnected nodes),
  and only if a match has been found, the PACs are checked in its context.

  Logically Combined Patterns
  ===========================

  Logically combined patterns extend NACs/PACs with logical combinators.  The
  general syntax is :operator [[ps1] [ps2] ...] where :operator is one
  of :and, :or, :xor, :nor, or :nand, and psI are normal pattern
  specifications.  In order for the outer pattern to match, the logically
  combined pattern must match according to the given operator.

  For example, the pattern

    [c<C>
     :xor [[c -<:t>-> c]
           [c -<:t>-> <B>]]]

  matches all C-nodes c which either reference themselves or some B-node with
  their c-reference (but not both!).

  Like negative and positive patterns, logically combined pattern are mere
  constraints, i.e., the symbols contained in the pattern specs ps1, ps2 etc
  are not part of the surrounding outer pattern's matches.

  As said, logically combined patterns are just extensions to NACs/PACs.  These
  equivalences hold:

    [...
     :and [[ps1] [ps2]]]

  is equivalent to

    [...
     :positive [ps1]
     :positive [ps2]]

  and

    [...
     :nor [[ps1] [ps2]]]

  is equivalent to

    [...
     :negative [ps1]
     :negative [ps2]]

  Alternative Patterns
  ====================

  Alternative patterns allow to specify variable parts in a pattern.  In
  contrast to negative/positive and logically combined patterns, they are not
  only constraints but the elements matched by the alternative patterns are
  part of the outer pattern's matches.

  Take this example pattern spec:

    [c<C>
     :alternative [[c -<:t>-> a<A!>]
                   [c -<:t>-> x<A> -<:s>-> a<A!>]]
     a -<:t>-> b<B>]

  The pattern matches C-nodes c, A-nodes a, and B-nodes b where c references a
  with its t-reference (first alternative), or where c references an A-node x
  with its t-reference which in turn references a with its s-reference (second
  alternative).  Additionally, a references b with its t-reference.

  The matches of this pattern have the form {:c #<C>, :a #<A>, :x #<A>}, i.e.,
  they have an entry for every identifier contained in the outer pattern spec,
  and every identifier in any of the alternative pattern specs.  In case a
  match is produced by the first alternative, the value of the :x key is nil.

  Note that due to the implementation it is important that no node symbols that
  are ought to be matched by the alternative patterns occur before those.
  E.g., if the above pattern was written

    [c<C>
     a -<:t>-> b<B>
     :alternative [[c -<:t>-> a<A!>]
                   [c -<:t>-> x<A> -<:s>-> a<A!>]]]

  the alternatives would be tested for every combination of a C-node c and an
  arbitrary node a and all its B-nodes referenced by its t-reference.  Clearly,
  this is not performant.

  Also note that for implementation reasons, pattern inheritance is not
  available for alternative patterns, i.e., the alternative pattern specs must
  not contain an :extends clause.

  Nested Patterns
  ===============

  A pattern can include nested pattern using :nested clauses.  Each :nested
  clause contains arbitrary many symbol-pattern pairs.

    [a<A> -<:d>-> d<D>
     :nested [f1 [a -<:t>-> a1
                  :nested [f2 [a1 -<:t>-> a2]]]]]

  So in the example above, the outer pattern has one inner pattern whose
  matches are bound to f1.  The nested pattern has another nested pattern whose
  matches are bound to f2.  Each pattern has access to the preceeding variables
  in the outer patterns, i.e., the first nested pattern can see a and d (and
  actually uses a), and the innermost nested pattern can see a, d, and a1 (and
  actually uses a1).

  The matches of the pattern above have the following structure.

    {:a a
     :d d
     :f1 ({:a1 a1
           :f2 ({:a2 a2}
                ...)}
          ...)}

  As can be seen, by default the elements already matched by an outer pattern
  are omitted in the matches of inner patterns, e.g., the :f1 matches don't
  include :a, and the :f2 matches don't include :a1.

  The :f1 and :f2 values are lazy sequences of nested pattern matches.

  Match Representation
  ====================

  By default, the matches of a pattern are represented as maps from keywords
  named according to the pattern symbol identifiers to the matched elements.

    [v --> w
     :when-let [u (foobar w)]]

  So the example above is equivalent to the following pattern with an :as form.

    [v --> w
     :when-let [u (foobar w)]
     :as {:u u, :v v, :w w}]

  To represent matches as vectors, one could write :as [v w u].  There is also
  the shorthand :as :vector which represents matches as vectors where the
  values are ordered according their occurence in the pattern.
  Thus, :as :vector is equivalent to :as [v w u] here.  For reasons of
  symmetry, there is also the shorthand :as :map which is equivalent to
  omitting the :as clause, i.e., matches are represented as maps.

  Distinct Matches
  ================

  Finally, a pattern may contain a :distinct modifier.  If there is one, the
  lazy seq of matches which is the result of a applying the pattern won't
  contain duplicates (where \"duplicates\" is defined by the :as clause).
  Let's clarify that with an example.  Consider a model with only two nodes n1
  and n2.  There are the following four edges: n1 --> n1, n1 --> n2, n1 --> n2,
  and n2 --> n1.  Then the effects of :distinct (in combination with :as) are
  as follows:

    [x --> y]    => 4 matches: [n1 n1], [n1 n2], [n1 n2], [n2 n1]

    [x --> y     => 3 matches: [n1 n1], [n1 n2], [n2 n1]
     :distinct]

    [x --> y     => 2 matches: #{n1 n1}, #{n1 n2}
     :as #{x y}
     :distinct]

  Available Options
  =================

  The :eager option
  -----------------

  If ^:eager metadata is attached to `name` (or the :eager option is set to
  true in the `attr-map`), then the pattern is evaluated eagerly giving rise to
  parallel evaluation.  Here, the matches of the first node in the `pattern` is
  computed sequentially and put into a vector.  This vector is then partitioned
  and the remaining pattern matching process is performed in parallel on the
  individual partitions.  By default, the vector of matches of the first node
  in the pattern is partitioned so that there are at most 32 partitions to be
  processed per CPU (keep CPUs busy!), however, every partition has at least 16
  elements (not too much contention!).  This seems to be a good heuristic, but
  of course it cannot be optimal in every situation.

  Therefore, the :eager option may also be set to a vector of the form

    [MIN-PARTITION-SIZE MAX-PARTITIONS-PER-CPU]

  where MIN-PARTITION-SIZE denotes the minimal number of elements in a
  partition, and MAX-PARTITIONS-PER-CPU denotes the maximum number of
  partitions to be processed per CPU.

  The :sequential option
  ----------------------

  The parallel evaluation induced by :eager may be suppressed using
  ^:sequential metadata (or by setting the :sequential option to true in
  `attr-map`).

  The :pattern-expansion-context option
  -------------------------------------

  The expansion of a pattern, i.e., if it expands into a generic query, a query
  on TGraphs, or a query on EMF models (or something else), is controlled by
  the option `:pattern-expansion-context` with possible values
  `:generic` (default), `:tg` or `:emf` which can be specified in the
  `attr-map` given to `defpattern` or as metadata.

  Instead of using that option for every rule, you can also set
  `:pattern-expansion-context` metadata to the namespace defining patterns, in
  which case that expansion context is used for all patterns in that namespace.
  Finally, it is also possible to bind `*pattern-expansion-context*` otherwise.
  Note that this binding has to be available at compile-time."

  {:arglists '([name doc-string? attr-map? [args] [pattern]]
               [name doc-string? attr-map? ([args] [pattern])+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        name (vary-meta name assoc ::pattern-specs `'~(if (vector? (first more))
                                                        [(fnext more)]
                                                        (vec (map fnext more))))]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              (:pattern-expansion-context (meta *ns*))
                                              *pattern-expansion-context*)]
      `(defn ~name ~(meta name)
         ~@(if (vector? (first more))
             (convert-spec name more)
             (mapv (partial convert-spec name) more))))))

(defmacro letpattern
  "Establishes local patterns just like `letfn` establishes local functions.
  Every pattern in the `patterns` vector is specified as one of:

    (pattern-name attr-map? [args] [pattern])
    (pattern-name attr-map? ([args] [pattern])+)

  For the syntax and semantics of patterns, see the `defpattern` docs.

  Following the patterns vector, an `attr-map` may be given for specifying the
  `*pattern-expansion-context*` in case it's not bound otherwise (see that
  var's documentation and `defpattern`).  The attr-maps of the individual
  patterns may override entries of the letpattern's `attr-map`."
  {:arglists '([[patterns] attr-map? & body])}
  [patterns & attr-map-body]
  (when-not (vector? patterns)
    (u/errorf "No patterns vector in letpattern!"))
  (let [[attr-map body] (if (map? (first attr-map-body))
                          [(first attr-map-body) (next attr-map-body)]
                          [{} attr-map-body])
        names-with-specs (apply hash-map (mapcat (fn [[n & more]]
                                                   (let [more (if (map? (first more))
                                                                (next more)
                                                                more)]
                                                     [n (if (vector? (first more))
                                                          [(fnext more)]
                                                          (vec (map fnext more)))]))
                                                 patterns))]
    (binding [*letpattern-pattern-specs* (merge *letpattern-pattern-specs*
                                                names-with-specs)]
      `(letfn [~@(map (fn [[n & more]]
                        (let [[n more] (if (map? (first more))
                                         [(vary-meta n merge attr-map (first more)) (next more)]
                                         [n more])]
                          (binding [*pattern-expansion-context*
                                    (or (:pattern-expansion-context (meta n))
                                        (:pattern-expansion-context attr-map)
                                        (:pattern-expansion-context (meta *ns*))
                                        *pattern-expansion-context*)]
                            `(~n
                              ~@(if (vector? (first more))
                                  (convert-spec n more)
                                  (mapv (partial convert-spec n) more))))))
                   patterns)]
         ~@body))))

(defmacro pattern
  "Creates an anonymous patterns just like `fn` creates an anonymous functions.
  The syntax is

    (pattern pattern-name? attr-map? [args] [pattern])
    (pattern pattern-name? attr-map? ([args] [pattern])+)

  For the syntax and semantics of patterns, see the `defpattern` docs.

  The `*pattern-expansion-context*` may be given as metadata to the pattern
  name in case it's not bound otherwise (see that var's documentation and
  `defpattern`)."

  {:arglists '([name? attr-map? [args] [pattern]]
               [name? attr-map? ([args] [pattern])+])}
  [& more]
  (let [[name more] (if (symbol? (first more))
                      [(first more) (next more)]
                      [nil more])
        [attr-map more] (if (map? (first more))
                          [(first more) (next more)]
                          [nil more])
        [name more] (if name
                      (m/name-with-attributes name more)
                      [name more])
        name        (if name
                      (vary-meta name merge attr-map)
                      (with-meta (gensym "anon-pattern-") attr-map))]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              (:pattern-expansion-context (meta *ns*))
                                              *pattern-expansion-context*)]
      `(fn ~@(when name [name])
         ~@(if (vector? (first more))
             (convert-spec name more)
             (mapv (partial convert-spec name) more))))))
