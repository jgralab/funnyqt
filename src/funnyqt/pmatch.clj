(ns funnyqt.pmatch
  "Pattern Matching."
  (:use [funnyqt.utils :only [errorf pr-identity]])
  (:use [funnyqt.query :only [the member?]])
  (:use funnyqt.protocols)
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.query.tg :as tgq]
            [funnyqt.emf :as emf]
            [funnyqt.query.emf :as emfq])
  (:require clojure.set)
  (:require [clojure.tools.macro :as m]))

;; TODO: Patterns and rules should support ^:perf-stat metadata which records
;; the number of nodes of the types occuring in the pattern in the host graph.
;; Then users can check if their pattern is anchored at the right node, or if
;; they should reformulate it to speed up things.

;; TODO: There are several has-type? checks in the resulting comprehensions.
;; In order to speed things up, it would be better to create all needed type
;; matchers in advance, bind them to variables, and then use them in the
;; pattern.

;;# Pattern to pattern graph

(defn ^:private vertex-sym? [sym]
  (and (symbol? sym)
       (re-matches #"[a-zA-Z0-9_]*(<[a-zA-Z0-9._!]*>)?" (name sym))))

(defn ^:private edge-sym? [sym]
  (and
   (symbol? sym)
   (re-matches #"<?-[!a-zA-Z0-9_]*(<[a-zA-Z0-9._!]*>)?->?" (name sym))
   (or (re-matches #"<-.*-" (name sym))
       (re-matches #"-.*->" (name sym)))))

(defn ^:private name-and-type [sym]
  (if (or (vertex-sym? sym) (edge-sym? sym))
    (let [[_ s t] (re-matches #"(?:<-|-)?([!a-zA-Z0-9_]+)?(?:<([.a-zA-Z0-9_!]*)>)?(?:-|->)?"
                              (name sym))]
      [(and (seq s) (symbol s)) (and (seq t) (symbol t))])
    (errorf "No valid pattern symbol: %s" sym)))

(defn ^:private neg-edge-sym? [sym]
  (and (edge-sym? sym)
       (= '! (first (name-and-type sym)))))

(defn ^:private edge-dir [esym]
  (if (edge-sym? esym)
    (if (re-matches #"<-.*" (name esym))
      :in
      :out)
    (errorf "%s is not edge symbol." esym)))

(defonce ^:private pattern-schema
  (tg/load-schema (clojure.java.io/resource "pattern-schema.tg")))

(defn call-binding-vars
  "Returns the symbols bound by a :call in pattern p."
  [p]
  (loop [p p, r []]
    (if (seq p)
      (if (= :call (first p))
        (recur (nnext p) (into r (flatten (map first (partition 2 (fnext p))))))
        (recur (next p) r))
      r)))

(defn pattern-to-pattern-graph [pname argvec pattern]
  (let [callbounds (into #{} (call-binding-vars pattern))
        argset (into #{} argvec)
        pg (let [g (tg/create-graph pattern-schema)]
             (tg/set-value! g :patternName (if pname (name pname) "--anonymous--"))
             g)
        get-by-name (fn [n]
                      (first (filter #(= (name n) (tg/value % :name))
                                     (concat (tg/vseq pg 'APatternVertex)
                                             (tg/eseq pg '[PatternEdge ArgumentEdge])))))
        check-unique (fn [n t]
                       (when (and n t (get-by-name n))
                         (errorf "A pattern element with name %s is already declared!" n))
                       (when (and t (argset n))
                         (errorf "The pattern declares %s although that's an argument already!" n)))
        get-or-make-v (fn [n t]
                        (if-let [v (and n (get-by-name n))]
                          v
                          (let [v (tg/create-vertex! pg (cond
                                                         (argset n)     'ArgumentVertex
                                                         (callbounds n) 'CallBoundVertex
                                                         :else          'PatternVertex))]
                            (when n (tg/set-value! v :name (name n)))
                            (when t (tg/set-value! v :type (name t)))
                            v)))]
    (loop [pattern pattern, lv (tg/create-vertex! pg 'Anchor)]
      (when (seq pattern)
        (cond
         ;; Constraints and non-pattern binding forms ;;
         (#{:when :let :when-let :while :call} (first pattern))
         (let [v (tg/create-vertex! pg 'ConstraintOrBinding)]
           (tg/set-value! v :form
                          (if (= :call (first pattern))
                            (str (pr-str (fnext pattern)) "]")
                            (str "[" (str (pr-str (first pattern))  " ")
                                 (pr-str (fnext pattern)) "]")))
           (tg/create-edge! pg 'Precedes lv v)
           (recur (nnext pattern) v))
         ;; Edge symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (edge-sym? (first pattern)) (let [sym (first pattern)
                                           [n t] (name-and-type sym)
                                           nsym (second pattern)
                                           [nvn nvt] (name-and-type nsym)
                                           _ (check-unique nvn nvt)
                                           nv (get-or-make-v nvn nvt)]
                                       (let [e (apply tg/create-edge!
                                                      pg (cond
                                                          (= '! n)   'NegPatternEdge
                                                          (argset n) 'ArgumentEdge
                                                          :else      'PatternEdge)
                                                      (if (= :out (edge-dir sym))
                                                        [lv nv]
                                                        [nv lv]))]
                                         (when (and n (not (has-type? e 'NegPatternEdge)))
                                           (tg/set-value! e :name (name n)))
                                         (when t (tg/set-value! e :type (name t))))
                                       (recur (nnext pattern) nv))
         ;; Vertex symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (vertex-sym? (first pattern)) (let [sym (first pattern)
                                             [n t] (name-and-type sym)
                                             v (get-or-make-v n t)]
                                         (when (= 0 (tg/ecount pg 'HasStartPatternVertex))
                                           (tg/create-edge! pg 'HasStartPatternVertex
                                                            (the (tg/vseq pg 'Anchor)) v))
                                         (recur (rest pattern) v))
         :else (errorf "Don't know how to handle pattern part: %s" (first pattern)))))
    ;; Anchor disconnected components at the anchor.
    (let [vset (funnyqt.utils/oset (tg/vseq pg))
          a (the (tg/vseq pg 'Anchor))]
      (loop [disc (clojure.set/difference vset (tgq/reachables a [q/p-* tgq/<->]))]
        (when (seq disc)
          (tg/create-edge! pg 'HasStartPatternVertex a (first disc))
          (recur (clojure.set/difference vset (tgq/reachables a [q/p-* tgq/<->]))))))
    (when-let [argv (seq (tg/vseq pg 'ArgumentVertex))]
      (let [hfpv (the (tg/eseq pg 'HasStartPatternVertex))]
        (when (has-type? (tg/omega hfpv) '!ArgumentVertex)
          (println
           (format "The pattern %s could perform better by anchoring it at an argument node."
                   (tg/value pg :patternName))))))
    pg))

;;# Pattern comprehension

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
  "Converts :when-let [x (foo), y (bar)] to :let [x (foo)] :when x :let [y (bar)] :when y."
  [bindings]
  (loop [p bindings, nb []]
    (if (seq p)
      (if (= :when-let (first p))
        (recur (rest (rest p))
               (vec (concat nb (shortcut-when-let-vector (fnext p)))))
        (recur (rest (rest p)) (conj (conj nb (first p)) (second p))))
      (vec nb))))

(defmacro pattern-for
  [seq-exprs body-expr]
  (let [seq-exprs (shortcut-when-let-bindings seq-exprs)
        [bind exp] seq-exprs]
    (condp = bind
      :let `(let ~exp
              (pattern-for ~(vec (rest (rest seq-exprs)))
                ~body-expr))
      :when `(when ~exp
               (pattern-for ~(vec (rest (rest seq-exprs)))
                ~body-expr))
      ;; default
      (if (seq seq-exprs)
        `(for ~seq-exprs
           ~body-expr)
        (sequence nil)))))

;;# Patter graph to pattern comprehension

(defn ^:private enqueue-incs
  ([cur stack done]
     (enqueue-incs cur stack done false))
  ([cur stack done only-out]
     (into stack (remove done
                         (tg/riseq cur nil (when only-out :out))))))

(defn ^:private conj-done [done & elems]
  (into done (mapcat #(if (tg/edge? %)
                        (vector % (tg/inverse-edge %))
                        (vector %))
                     elems)))

(defn ^:private get-name [elem]
  (when-let [n (tg/value elem :name)]
    (symbol n)))

(defn ^:private anon? [elem]
  (or (has-type? elem 'NegPatternEdge)
      (not (get-name elem))))

(defn ^:private get-type [elem]
  (when (has-type? elem '[PatternVertex PatternEdge NegPatternEdge])
    (when-let [t (tg/value elem :type)]
      (symbol t))))

(defn ^:private anon-vec [startv done]
  (loop [cur startv, done done, vec []]
    (if (and cur (anon? cur))
      (cond
       (tg/edge? cur)   (recur (tg/that cur)
                               (conj-done done cur)
                               (conj vec cur))
       (tg/vertex? cur) (recur (let [ns (remove done (tg/iseq cur 'PatternEdge))]
                                 (if (> (count ns) 1)
                                   (errorf "Must not happen!")
                                   (first ns)))
                               (conj-done done cur)
                               (conj vec cur))
       :else (errorf "Unexpected %s." cur))
      (if cur
        (conj vec cur)
        vec))))

(defn ^:private validate-bf [bf done pg]
  (when-let [missing (seq (remove done (concat (tg/vseq pg) (tg/eseq pg))))]
    ;;(tg/show-graph pg)
    (errorf "Some pattern elements were not reached: %s" missing))
  bf)

(defn ^:private do-anons [anon-vec-transformer-fn startsym av done]
  (let [target-node (last av)]
    (cond
     (anon? target-node)
     [:when `(seq ~(anon-vec-transformer-fn startsym av))]
     ;;---
     (done target-node)
     [:when `(q/member? ~(get-name target-node)
                        ~(anon-vec-transformer-fn startsym av))]
     ;;---
     ;; Not already done ArgumentVertex, so declare it!
     (has-type? target-node 'ArgumentVertex)
     [:when-let `[~(get-name target-node) ~(get-name target-node)]
      :when `(q/member? ~(get-name target-node)
                        ~(anon-vec-transformer-fn startsym av))]
     ;;---
     (has-type? target-node 'CallBoundVertex)
     [:when `(q/member? ~(get-name target-node)
                        ~(anon-vec-transformer-fn startsym av))]
     ;;---
     :normal-v
     [(get-name target-node)
      `(distinct ~(anon-vec-transformer-fn startsym av))])))

(defn ^:private make-type-matchers [pg gsym also-edges?]
  (let [types (distinct (remove nil? (map #(tg/value % :type)
                                          (concat (tg/vseq pg 'PatternVertex)
                                                  (when also-edges?
                                                    (tg/eseq pg 'PatternEdge))))))
        tm-map (apply hash-map (mapcat (fn [t]
                                         [(symbol t) (gensym (str "tm-" t))])
                                       types))]
    [tm-map
     `[~@(mapcat (fn [[t t-gensym]]
                   [t-gensym `(type-matcher ~gsym '~t)])
                 tm-map)]]))

(defn pattern-graph-to-pattern-for-bindings-tg [argvec pg]
  (let [gsym (first argvec)
        [tm-map tm-vec] (make-type-matchers pg gsym true)
        anon-vec-to-for (fn [start-sym av]
                          (let [[v r]
                                (loop [cs start-sym, av av, r []]
                                  (if (seq av)
                                    (let [el (first av)
                                          ncs (gensym)]
                                      (recur ncs
                                             (rest av)
                                             (if (tg/vertex? el)
                                               (into r `[:let [~ncs (tg/that ~cs)]
                                                         ~@(when-let [t (get-type el)]
                                                             [:when `(~(tm-map t) ~ncs)])])
                                               (into r `[~ncs (tg/iseq ~cs ~(tm-map (get-type el))
                                                                       ~(if (tg/normal-edge? el)
                                                                          :out :in))]))))
                                    [cs r]))]
                            `(for ~r ~v)))]
    (loop [stack [(the (tg/vseq pg 'Anchor))]
           done #{}
           bf []]
      (if (seq stack)
        (let [cur (peek stack)]
          (if (done cur)
            (recur (pop stack) done bf)
            (case (qname cur)
              Anchor
              (recur (enqueue-incs cur (pop stack) done)
                     (conj-done done cur)
                     bf)
              HasStartPatternVertex
              (recur (conj (pop stack) (tg/that cur))
                     (conj-done done cur)
                     bf)
              PatternVertex
              (recur (enqueue-incs cur (pop stack) done)
                     (conj-done done cur)
                     (into bf `[~(get-name cur) (tg/vseq ~gsym ~(tm-map (get-type cur)))]))
              ArgumentVertex
              (recur (enqueue-incs cur (pop stack) done)
                     (conj-done done cur)
                     (if (done cur) bf (into bf `[:when-let [~(get-name cur) ~(get-name cur)]])))
              CallBoundVertex  ;; They're bound by ConstraintOrBinding/Preceedes
              (recur (enqueue-incs cur (pop stack) done)
                     (conj-done done cur)
                     bf)
              PatternEdge
              (if (anon? cur)
                (let [av (anon-vec cur done)
                      target-node (last av)
                      done (conj-done done cur)]
                  ;;(println av)
                  (recur (enqueue-incs target-node (pop stack) done)
                         (apply conj-done done av)
                         (into bf (do-anons anon-vec-to-for (get-name (tg/this cur)) av done))))
                (let [trg (tg/that cur)
                      done (conj-done done cur)]
                  (recur (enqueue-incs trg (pop stack) done)
                         (conj-done done trg)
                         (apply conj bf `~(get-name cur)
                                `(tg/iseq ~(get-name (tg/this cur)) ~(tm-map (get-type cur))
                                           ~(if (tg/normal-edge? cur) :out :in))
                                (cond
                                 (done trg) [:when `(= ~(get-name trg) (tg/that ~(get-name cur)))]
                                 (anon? trg) (do-anons anon-vec-to-for
                                                       `(tg/that ~(get-name cur))
                                                       (anon-vec trg done) done)
                                 ;;---
                                 (has-type? trg 'ArgumentVertex)
                                 [:when-let [(get-name trg) (get-name trg)]
                                  :when `(= ~(get-name trg) (tg/that ~(get-name cur)))]
                                 ;;---
                                 :else (concat
                                        [:let `[~(get-name trg) (tg/that ~(get-name cur))]]
                                        (when-let [t (get-type trg)]
                                          `[:when (~(tm-map t) ~(get-name trg))])))))))
              ArgumentEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)]
                (recur (enqueue-incs trg (pop stack) done)
                       (conj-done done cur trg)
                       (apply conj bf :when `(= ~(get-name src) (tg/this ~(get-name cur)))
                              (cond
                               (done trg) [:when `(= ~(get-name trg) (tg/that ~(get-name cur)))]
                               ;;---
                               (has-type? trg 'ArgumentVertex)
                               [:when-let [(get-name trg) (get-name trg)]
                                :when `(= ~(get-name trg) (tg/that ~(get-name cur)))]
                               ;;---
                               :else (concat
                                      [:let `[~(get-name trg) (tg/that ~(get-name cur))]]
                                      (when-let [t (get-type trg)]
                                        `[:when (~(tm-map t) ~(get-name trg))]))))))
              NegPatternEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)
                    done (conj-done done cur)]
                (if (done trg)
                  (recur (enqueue-incs trg (pop stack) done)
                         (conj-done done trg)
                         (into bf `[:when (empty? (filter
                                                   #(= ~(get-name trg) (tg/that %))
                                                   (tg/iseq ~(get-name src) ~(tm-map (get-type cur))
                                                            ~(if (tg/normal-edge? cur) :out :in))))]))
                  (recur (enqueue-incs trg (pop stack) done)
                         (conj-done done trg)
                         (into bf `[~@(when-not (anon? trg)
                                        `[~(get-name trg) (tg/vseq ~gsym ~(tm-map (get-type trg)))])
                                    :when (empty? (tg/iseq ~(get-name src) ~(tm-map (get-type cur))
                                                           ~(if (tg/normal-edge? cur) :out :in)))]))))
              Precedes
              (let [cob (tg/that cur)
                    allcobs (tgq/reachables cob [q/p-* [tgq/--> 'Precedes]])
                    forms (mapcat #(read-string (tg/value % :form)) allcobs)
                    allprecs (mapcat #(tg/iseq % 'Precedes) allcobs)]
                (recur (pop stack)
                       (apply conj-done done cur (concat allcobs allprecs))
                       (into bf forms))))))
        [(validate-bf bf done pg) tm-vec]))))

(defn pattern-graph-to-pattern-for-bindings-emf [argvec pg]
  (let [gsym (first argvec)
        [tm-map tm-vec] (make-type-matchers pg gsym false)
        get-edge-type (fn [e]
                        (keyword (get-type e)))
        anon-vec-to-for (fn [start-sym av]
                          (let [[v r]
                                (loop [cs start-sym, av av, r []]
                                  (if (seq av)
                                    (let [el (first av)
                                          ncs (if (tg/vertex? el) cs (gensym))]
                                      (recur ncs
                                             (rest av)
                                             (if (tg/vertex? el)
                                               (into r (when-let [t (get-type el)]
                                                         [:when `(~(tm-map t) ~ncs)]))
                                               (into r `[~ncs ~(if-let [t (get-edge-type el)]
                                                                 `(q/adjs* ~cs ~t)
                                                                 `(emf/erefs ~cs))]))))
                                    [cs r]))]
                            `(for ~r ~v)))]
    ;; Check there are only anonymous edges.
    (when-not (every? anon? (tg/eseq pg 'APatternEdge))
      (errorf "Edges mustn't be named for EMF: %s"
              (mapv describe (remove anon? (tg/eseq pg 'APatternEdge)))))
    (loop [stack [(the (tg/vseq pg 'Anchor))]
           done #{}
           bf []]
      (if (seq stack)
        (let [cur (peek stack)]
          (if (done cur)
            (recur (pop stack) done bf)
            (case (qname cur)
              Anchor
              (recur (enqueue-incs cur (pop stack) done true)
                     (conj-done done cur)
                     bf)
              HasStartPatternVertex
              (recur (conj (pop stack) (tg/that cur))
                     (conj-done done cur)
                     bf)
              PatternVertex
              (recur (enqueue-incs cur (pop stack) done true)
                     (conj-done done cur)
                     (into bf `[~(get-name cur) (emf/eallobjects ~gsym ~(tm-map (get-type cur)))]))
              ArgumentVertex
              (recur (enqueue-incs cur (pop stack) done true)
                     (conj-done done cur)
                     (if (done cur) bf (into bf `[:when-let [~(get-name cur) ~(get-name cur)]])))
              CallBoundVertex  ;; Actually bound by ConstraintOrBinding/Precedes
              (recur (enqueue-incs cur (pop stack) done true)
                     (conj-done done cur)
                     bf)
              PatternEdge
              (if (anon? cur)
                (let [av (anon-vec cur done)
                      target-node (last av)
                      done (conj-done done cur)]
                  (recur (enqueue-incs target-node (pop stack) (apply conj-done done av) true)
                         (apply conj-done done cur av)
                         (into bf (do-anons anon-vec-to-for
                                            (get-name (tg/this cur)) av done))))
                (errorf "Edges mustn't be named for EMF: %s" (describe cur)))
              NegPatternEdge
              (let [src (tg/this cur)
                    trg (tg/that cur)
                    done (conj-done done cur)]
                (if (done trg)
                  (recur (enqueue-incs trg (pop stack) done)
                         (conj-done done trg)
                         (into bf `[:when (not (member? ~(get-name trg)
                                                        ~(if-let [t (get-edge-type cur)]
                                                                 `(q/adjs* ~(get-name src) ~t)
                                                                 `(emf/erefs ~(get-name src)))))]))
                  (recur (enqueue-incs trg (pop stack) done)
                         (conj-done done trg)
                         (into bf `[~@(when-not (anon? trg)
                                        `[~(get-name trg) (emf/eallobjects
                                                           ~gsym ~(tm-map (get-type trg)))])
                                    :when (empty? ~(if-let [t (get-edge-type cur)]
                                                                 `(q/adjs* ~(get-name src) ~t)
                                                                 `(emf/erefs ~(get-name src))))]))))
              ArgumentEdge
              (errorf "There mustn't be argument edges for EMF: %s" (describe cur))
              Precedes
              (let [cob (tg/that cur)
                    allcobs (tgq/reachables cob [q/p-* [tgq/--> 'Precedes]])
                    forms (mapcat #(read-string (tg/value % :form)) allcobs)
                    allprecs (mapcat #(tg/iseq % 'Precedes) allcobs)]
                (recur (pop stack)
                       (apply conj-done done cur (concat allcobs allprecs))
                       (into bf forms))))))
        [(validate-bf bf done pg) tm-vec]))))

(defn bindings-to-arglist
  "Rips out the symbols declared in `bindings`.
  `bindings` is a binding vector with the syntax of `for`."
  [bindings]
  (loop [p bindings, l []]
    (if (seq p)
      (cond
       ;; Handle :let [x y, [u v] z]
       (or (= :let (first p))
           (= :call (first p))
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
       ;; Ignore :when (exp ...)
       (keyword? (first p)) (recur (rest (rest p)) l)
       ;; A vector destructuring form
       (vector? (first p)) (recur (rest (rest p)) (vec (concat l (first p))))
       ;; Another destructuring form
       (coll? (first p))
       (errorf "Only vector destructuring is permitted outside :let, got: %s"
               (first p))
       ;; That's a normal binding
       :default (recur (rest (rest p)) (conj l (first p))))
      (vec l))))

(defn ^:private verify-pattern-vector
  "Ensure that the pattern vector doesn't declare bindings twice, which would
  be a bug."
  [pattern args]
  (let [blist (bindings-to-arglist pattern)]
    (if-let [double-syms (seq (mapcat (fn [[sym freq]]
                                        (when (> freq 1)
                                          (str "- " sym " is declared " freq " times\n")))
                                      (frequencies blist)))]
      (errorf "These symbols are declared multiple times:\n%s"
              (apply str double-syms))
      pattern)))

(def ^:dynamic *pattern-expansion-context*
  "Defines the expansion context of a pattern, i.e., if a pattern expands into
  a query on a TGraph or an EMF model.  The possible values are :tg or :emf.

  Usually, you won't bind this variable directly (using `binding`) but instead
  you specify the expansion context for a given pattern using the `attr-map` of
  a `defpattern` or `letpattern` form, or you declare the expansion context for
  a complete namespace using `:pattern-expansion-context` metadata for the
  namespace."
  nil)

(def pattern-graph-transform-function-map
  "A map from techspace to pattern graph transformers."
  {:emf pattern-graph-to-pattern-for-bindings-emf
   :tg  pattern-graph-to-pattern-for-bindings-tg})

(defn transform-pattern-vector
  "Transforms patterns like [a<X> -<role>-> b<Y>] to a binding for
  supported by `pattern-for`.  (Only for internal use.)"
  [name pattern args]
  (let [pgraph (pattern-to-pattern-graph name args pattern)
        transform-fn (pattern-graph-transform-function-map *pattern-expansion-context*)]
    (if transform-fn
      (transform-fn args pgraph)
      (errorf "The pattern expansion context is not set.\n%s"
              "See `*pattern-expansion-context*` in the pmatch namespace."))))

(defn ^:private convert-spec [name [args pattern resultform]]
  (when-not (and (vector? args) (vector? pattern))
    (errorf "Pattern %s is missing the args or pattern vector." name))
  (let [[bf tms] (transform-pattern-vector name pattern args)]
    (verify-pattern-vector bf args)
    `(~args
      (let ~tms
        (pattern-for ~bf
                     ~(or resultform (bindings-to-arglist bf)))))))

(defmacro defpattern
  "Defines a pattern with `name`, optional `doc-string`, optional `attr-map`,
  an `args` vector, and a `pattern` vector.  When invoked, it returns a lazy seq
  of all matches of `pattern`.

  `pattern` is a vector of symbols for nodes and edges.

    v<V>:            A node of type V identified as v
    v<V> -e<E>-> v:  An edge of type E starting and ending at node v of type V

  Both the identifier (v and e above) and the type enclosed in angle brackets
  are optional.  So this is a valid pattern, too.

    [v --> <V> -<E>-> <> --> x<X>]: An arbitrary node that is connected to an
                                    X-node x via some arbitrary forward edge
                                    leading to some V-node from which an E-edge
                                    leads some arbitrary other node from which
                                    another arbitrary edge leads to x.

  Such sequences of anonymous paths, i.e., edges and nodes without identifier,
  must be anchored at named nodes like above (v and x).

  Patterns may also include the arguments given to the defpattern, in which
  case those are assumed to be bound to one single node or edge, depending on
  their usage in the pattern, e.g., arg must be a node and -arg-> must be an
  edge.

  Patters may further include arbitrary constraints that must hold for a valid
  match using the following syntax:

    [v --> w
     :when (pred1? v)
     :when (not (pred2? w))]

  Patterns may contain negative edges indicated by edge symbols with name !.
  Those must not exist for a match to succeed.  For example, the following
  declares that there must be a Foo edge from v to w, but w has no outgoing
  edges at all, and v and w must not be connected with a forward Bar edge.

    [v -<Foo>-> w -!-> <>
     v -!<Bar>-> w]

  Moreover, a pattern may bind further variables using :let and :when-let.

    [v --> w
     :let [a (foo v), b (bar v)]
     :when-let [c (baz w)]]

  Hereby, the variables bound by :let (a and b) are taken as is whereas the
  variables bound by :when-let must be logically true in order to match.

  Finally, patterns may also include calls to other patterns and usual
  comprehension binding forms using :call, i.e., pairs of variables and
  expressions.

    [v --> w
     :call [u (reachables w [p-seq [p-+ [p-alt <>-- [<--- 'SomeEdgeType]]]])]]

  The result of a pattern is a lazy sequence of matches.  Each match is either
  defined by `result-spec` if that's given, or it defaults to a vector of
  matched elements in the order of declaration.  For example, the pattern

    (defpattern foo [a d]
      [a -e<E>-> b<B> <-f<F>- c<C>
       b <-- d])

  results in a lazy seq of [a e b f c d] vectors.

  The expansion of a pattern, i.e., if it expands to a query on TGraphs or EMF
  models, is controlled by the option `:pattern-expansion-context` with
  possible values `:tg` or `:emf` which can be specified in the `attr-map`
  given to `defpattern`.  Instead of using that option for every rule, you can
  also set `:pattern-expansion-context` metadata to the namespace defining
  patterns, in which case that expansion context is used.  Finally, it is also
  possible to bind `*pattern-expansion-context*` to `:tg` or `:emf` otherwise.
  Note that this binding has to be available at compile-time."

  {:arglists '([name doc-string? attr-map? [args] [pattern] result-spec?]
                 [name doc-string? attr-map? ([args] [pattern] result-spec?)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              *pattern-expansion-context*
                                              (:pattern-expansion-context (meta *ns*)))]
      `(defn ~name ~(meta name)
         ~@(if (seq? (first more))
             (mapv (partial convert-spec name) more)
             (convert-spec name more))))))

(defmacro letpattern
  "Establishes local patterns just like `letfn` establishes local functions.
  Every pattern in the `patterns` vector is specified as:

    (pattern-name [args] [pattern-spec] result-form)

  The result form is optional.

  Following the patterns vector, an `attr-map` may be given for specifying the
  `*pattern-expansion-context*` in case it's not bound otherwise (see that
  var's documentation and `defpattern`)."
  {:arglists '([[patterns] attr-map? & body])}
  [patterns attr-map & body]
  (when-not (vector? patterns)
    (errorf "No patterns vector in letpattern!"))
  (let [body (if (map? attr-map) body (cons attr-map body))]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context attr-map)
                                              *pattern-expansion-context*
                                              (:pattern-expansion-context (meta *ns*)))]
      `(letfn [~@(map (fn [[n & more]]
                        `(~n ~@(if (vector? (first more))
                                 (convert-spec n more)
                                 (mapv (partial convert-spec n) more))))
                   patterns)]
         ~@body))))

(defmacro pattern
  "Creates an anonymous patterns just like `fn` creates an anonymous functions.
  The syntax is

    (pattern pattern-name? attr-map? [args] [pattern-spec] result-form?)
    (pattern pattern-name? attr-map? ([args] [pattern-spec] result-form?)+)

  The `result-form` is optional.

  The `*pattern-expansion-context*` may be given as metadata to the pattern
  name in case it's not bound otherwise (see that var's documentation and
  `defpattern`)."

  {:arglists '([name attr-map? [args] [pattern] result-spec?]
                 [name attr-map? ([args] [pattern] result-spec?)+])}
  [& more]
  (let [[name more] (if (symbol? (first more))
                      [(first more) (next more)]
                      [nil more])
        [attr-map more] (if (map? (first more))
                          [(first more) (next more)]
                          [nil more])
        [name more] (if name
                      (m/name-with-attributes name more)
                      [name more])]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context attr-map)
                                              *pattern-expansion-context*
                                              (:pattern-expansion-context (meta *ns*)))]
      `(fn ~@(when name [name])
         ~@(if (vector? (first more))
             (convert-spec name more)
             (mapv (partial convert-spec name) more))))))
