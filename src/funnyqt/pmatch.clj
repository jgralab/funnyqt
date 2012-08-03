(ns funnyqt.pmatch
  "Pattern Matching."
  (:use [funnyqt.utils :only [errorf pr-identity]])
  (:use [funnyqt.query :only [the for* member?]])
  (:use funnyqt.macro-utils)
  (:use funnyqt.protocols)
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.query.tg :as tgq]
            [funnyqt.emf :as emf])
  (:require clojure.set)
  (:require [clojure.tools.macro :as m]))


(defn- vertex-sym? [sym]
  (and (symbol? sym)
       (re-matches #"[a-zA-Z0-9_]*(<[a-zA-Z0-9._!]*>)?" (name sym))))

(defn- edge-sym? [sym]
  (and
   (symbol? sym)
   (re-matches #"<?-[a-zA-Z0-9_]*(<[a-zA-Z0-9._!]*>)?->?" (name sym))
   (or (re-matches #"<-.*-" (name sym))
       (re-matches #"-.*->" (name sym)))))

(defn- name-and-type [sym]
  (if (or (vertex-sym? sym) (edge-sym? sym))
    (let [[_ s t] (re-matches #"(?:<-|-)?([a-zA-Z0-9_]+)?(?:<([.a-zA-Z0-9_!]*)>)?(?:-|->)?"
                              (name sym))]
      [(and (seq s) (symbol s)) (and (seq t) (symbol t))])
    (errorf "No valid pattern symbol: %s" sym)))

(defn- edge-dir [esym]
  (if (edge-sym? esym)
    (if (re-matches #"<-.*" (name esym))
      :in
      :out)
    (errorf "%s is not edge symbol." esym)))

(def ^:private pattern-schema
  (tg/load-schema "resources/pattern-schema.tg"))

(defn- pattern-to-pattern-graph [argvec pattern]
  (let [argset (into #{} argvec)
        pg (tg/create-graph pattern-schema)
        get-by-name (fn [n]
                      (first (filter #(= (name n) (tg/value % :name))
                                     (concat (tgq/vseq pg 'APatternVertex)
                                             (tgq/eseq pg 'APatternEdge)))))
        check-unique (fn [n t]
                       (when (and n t (get-by-name n))
                         (errorf "A pattern element with name %s is already declared!" n))
                       (when (and t (argset n))
                         (errorf "The pattern declares %s although that's an argument already!" n)))
        get-or-make-v (fn [n t]
                        (if-let [v (and n (get-by-name n))]
                          v
                          (let [v (tg/create-vertex! pg (if (argset n)
                                                          'ArgumentVertex
                                                          'PatternVertex))]
                            (when n (tg/set-value! v :name (name n)))
                            (when t (argset n) (tg/set-value! v :type (name t)))
                            v)))]
    (loop [pattern pattern, lv (tg/create-vertex! pg 'Anchor)]
      (when (seq pattern)
        (cond
         ;; Constraints and non-pattern binding forms ;;
         (or (#{:when :let :while} (first pattern))
             (coll? (fnext pattern)))
         (do
           (let [v (tg/create-vertex! pg 'ConstraintOrBinding)]
             (tg/set-value! v :form
                            (str "[" (pr-str (first pattern)) " "
                                 (pr-str (fnext pattern)) "]"))
             (tg/create-edge! pg 'Precedes lv v)
             (recur (nnext pattern) v)))
         ;; Edge symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (edge-sym? (first pattern)) (let [sym (first pattern)
                                           [n t] (name-and-type sym)
                                           nsym (second pattern)
                                           [nvn nvt] (name-and-type nsym)
                                           _ (check-unique nvn nvt)
                                           nv (get-or-make-v nvn nvt)]
                                       (let [e (if (= :out (edge-dir sym))
                                                 (tg/create-edge! pg (if (argset n)
                                                                       'ArgumentEdge
                                                                       'PatternEdge)
                                                                  lv nv)
                                                 (tg/create-edge! pg (if (argset n)
                                                                       'ArgumentEdge
                                                                       'PatternEdge)
                                                                  nv lv))]
                                         (when n (tg/set-value! e :name (name n)))
                                         (when t (tg/set-value! e :type (name t))))
                                       (recur (nnext pattern) nv))
         ;; Vertex symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (vertex-sym? (first pattern)) (let [sym (first pattern)
                                             [n t] (name-and-type sym)
                                             v (get-or-make-v n t)]
                                         (when (= 1 (tgq/vcount pg 'APatternVertex))
                                           (tg/create-edge! pg 'HasFirstPatternVertex
                                                            (the (tgq/vseq pg 'Anchor)) v))
                                         (recur (rest pattern) v))
         :else (errorf "Don't know how to handle pattern part: %s" (first pattern)))))
    pg))

;; TODO:
;;
;; - There must not be symbols for anonymous elements, else we get too many
;;   matches.
;;   + probably do it with path expressions
;;   + variants:
;;     - [a<A> -<B>-> <C> -<D>-> e<E>]
;;     - [a<A> -b<B>-> <C> -<D>-> e<E>]
;;     - [a<A> -<B>-> <C> -<D>-> <E>]
;;     - [a<A> -b<B>-> <C> -<D>-> <E>]
;;     - [<A> -<B>-> <C> -<D>-> e<E>]
;;     - [<A> -b<B>-> <C> -<D>-> e<E>]
;;     - [<A> -<B>-> <C> -<D>-> <E>]
;;     - [<A> -b<B>-> <C> -<D>-> <E>]
;;
;; - also allow consecutive anons: x<X> -<Y>-> <Z> -<A>-> b<B>
(defn pattern-graph-to-for*-bindings-tg [argvec pg]
  (let [gsym (first argvec)
        name #(when-let [n (tg/value % :name)]
                (symbol n))
        anon? (complement name)
        conj-done (fn [done & elems]
                    (into done (mapcat #(if (tg/edge? %)
                                          (vector % (tg/inverse-edge %))
                                          (vector %))
                                       elems)))
        anon-vec (fn [startv done]
                   (loop [cur startv, done done, vec []]
                     (if (and cur (anon? cur))
                       (cond
                        (tg/edge? cur)   (recur (tg/that cur)
                                                (conj-done done cur)
                                                (conj vec cur))
                        (tg/vertex? cur) (recur (let [ns (remove done (tgq/iseq cur))]
                                                  (if (> (count ns) 1)
                                                    (errorf "Must not happen!")
                                                    (first ns)))
                                                (conj-done done cur)
                                                (conj vec cur))
                        :else (errorf "Unexpected %s." cur))
                       (if cur
                         (conj vec cur)
                         vec))))
        type (fn [elem] (when-let [t (tg/value elem :type)]
                         `'~(symbol t)))
        anon-vec-to-rpd (fn [av]
                          `[q/p-seq ~@(mapcat (fn [el]
                                                (if (tg/vertex? el)
                                                  (if (has-type? el 'ArgumentVertex)
                                                    []
                                                    [[`q/p-restr (type el)]])
                                                  [[(if (tg/normal-edge? el)
                                                      `tgq/--> `tgq/<--)
                                                    (type el)]]))
                                          av)])
        enqueue-incs (fn [cur stack done]
                       (into stack (remove done (tgq/riseq cur))))]
    (loop [stack [(the (tgq/vseq pg 'Anchor))]
           done #{}
           bf []]
      (if (seq stack)
        (let [cur (peek stack)]
          (if (done cur)
            (recur (pop stack) done bf)
            (case (qname cur)
              Anchor (recur (enqueue-incs cur (pop stack) done)
                            (conj-done done cur)
                            bf)
              HasFirstPatternVertex (recur (conj (pop stack) (tg/that cur))
                                           (conj-done done cur)
                                           bf)
              PatternVertex (recur (enqueue-incs cur (pop stack) done)
                                   (conj-done done cur)
                                   (into bf `[~(name cur) (tgq/vseq ~gsym ~(type cur))]))
              ArgumentVertex (recur (enqueue-incs cur (pop stack) done)
                                    (conj-done done cur)
                                    bf)
              PatternEdge (if (anon? cur)
                            (let [av (anon-vec cur done)
                                  target-node (last av)
                                  done (apply conj-done done av)]
                              ;;(println av)
                              (cond
                               (anon? target-node)
                               (recur (enqueue-incs target-node (pop stack) done)
                                      done
                                      (conj bf :when
                                            `(seq (tgq/reachables
                                                   ~(name (tg/this cur))
                                                   ~(anon-vec-to-rpd av)))))
                               ;;;;;;;;;;;;;;;
                               (done target-node)
                               (recur (enqueue-incs target-node (pop stack) done)
                                      done
                                      (conj bf :when
                                            `(q/member?  ~(name target-node)
                                                         (tgq/reachables
                                                          ~(name (tg/this cur))
                                                          ~(anon-vec-to-rpd av)))))
                               ;;;;;;;;;;;;;;;
                               :normal-v
                               (recur (enqueue-incs target-node (pop stack) done)
                                      done
                                      (conj bf (name target-node)
                                            `(tgq/reachables ~(name (tg/this cur))
                                                             ~(anon-vec-to-rpd av))))))
                            (let [trg (tg/that cur)]
                              (recur (enqueue-incs trg (pop stack) done)
                                     (conj-done done cur trg)
                                     (apply conj bf `~(name cur)
                                            `(tgq/iseq ~(name (tg/this cur)) ~(type cur)
                                                       ~(if (tg/normal-edge? cur) :out :in))
                                            (cond
                                             (done trg) [:when `(= ~(name trg) (tg/that ~(name cur)))]
                                             (anon? trg) (errorf "Anon vertices not yet implemented!")
                                             :else (concat
                                                    [:let `[~(name trg) (tg/that ~(name cur))]]
                                                    (when-let [t (type trg)]
                                                      `[:when (has-type? ~(name trg) ~(type trg))])))))))
              ArgumentEdge (let [src (tg/this cur)
                                 trg (tg/that cur)]
                             (recur (enqueue-incs trg (pop stack) done)
                                    (conj-done done cur trg)
                                    (apply conj bf :when `(= ~(name src) (tg/this ~(name cur)))
                                           (if (done trg)
                                             [:when `(= ~(name trg) (tg/that ~(name cur)))]
                                             (concat
                                              [:let `[~(name trg) (tg/that ~(name cur))]]
                                              (when-let [t (type trg)]
                                                `[:when (has-type? ~(name trg) ~(type trg))]))))))
              Precedes (let [cob (tg/that cur)
                             allcobs (tgq/reachables cob [q/p-* [tgq/--> 'Precedes]])
                             forms (mapcat #(read-string (tg/value % :form)) allcobs)]
                         (recur (pop stack)
                                (conj-done done cur)
                                (into bf forms))))))
        bf))))

(defn- shortcut-let-vector [lv]
  (mapcat (fn [[s v]]
            [:let [s v] :when s])
          (partition 2 lv)))

(defn- shortcut-bindings
  "Converts :let [x (foo), y (bar)] to :let [x (foo)] :when x :let [y (bar)] :when y."
  [bindings]
  (loop [p bindings, nb []]
    (if (seq p)
      (if (= :let (first p))
        (recur (rest (rest p))
               (vec (concat nb (shortcut-let-vector (fnext p)))))
        (recur (rest (rest p)) (conj (conj nb (first p)) (second p))))
      (vec nb))))

(defn- verify-pattern-vector
  "Ensure that the match vector `match` and the arg vector `args` are disjoint.
  Throws an exception if they overlap, else returns `match`."
  [pattern args]
  (let [blist (bindings-to-arglist pattern)]
    (if (seq (clojure.set/intersection
              (set blist)
              (set args)))
      (errorf "Arglist and pattern vector overlap!")
      (if-let [double-syms (seq (mapcat (fn [[sym freq]]
                                          (when (> freq 1)
                                            (str "- " sym " is declared " freq " times\n")))
                                        (frequencies blist)))]
        (errorf "These symbols are declared multiple times:\n%s"
                (apply str double-syms))
        pattern))))

(def ^:dynamic *pattern-expansion-context*
  "Defines the expansion context of a pattern, i.e., if a pattern expands into
  a query on a TGraph or an EMF model.  The possible values are :tg or :emf.

  Usually, you won't bind this variable directly (using `binding`) but instead
  you specify the expansion context for a given pattern using the `arg-map` of
  a `defpattern` form, or you declare the expansion context for a complete
  namespace using `:pattern-expansion-context` metadata for the namespace."
  nil)

(defn transform-pattern-vector
  "Transforms patterns like [a<X> -<role>-> b<Y>] to a binding for
  supported by `for*`.  (Only for internal use.)"
  [pattern args]
  ;; TODO: Handle emf depending on *pattern-expansion-context*
  (case *pattern-expansion-context*
    :emf (errorf "Pattern compilation currently not supported for EMF!")
    :tg  (verify-pattern-vector
          (shortcut-bindings
           (pattern-graph-to-for*-bindings-tg
            args (pattern-to-pattern-graph args pattern)))
          args)
    (errorf "The pattern expansion context is not set.\n%s"
            "See `*pattern-expansion-context*` in the pmatch namespace.")))

(defn- convert-spec [[a p r]]
  (let [bf (transform-pattern-vector p a)]
    `(~a
      (for ~bf
        ~(or r (bindings-to-arglist bf))))))

(defmacro defpattern
  "Defines a pattern with `name`, optional `doc-string`, optional `attr-map`,
  an `args` vector, and a `pattern` vector.  When invoked, it returns a lazy seq
  of all matches of `pattern`.

  Usually, you use this to specify a pattern that occurs in the patterns of
  many rules.  So instead of writing a pattern vector like

    [a (vseq g), b (iseq a) :let [c (that b)], ...]

  in several rules, you do

    (defpattern abc [g] [a (vseq g), b (iseq a) :let [c (that b)]])

  and then

    [[a b c] (abc g), ...]

  in the rules.

  The expansion of a pattern, i.e., if it expands to a query on TGraphs or EMF
  models, is controlled by the option `:pattern-expansion-context` with
  possible values `:tg` or `:emf` has to be specified in the `attr-map` given
  to `defpattern`.  Instead of using that option for every rule, you can also
  set `:pattern-expansion-context` metadata to the namespace defining patterns,
  in which case that expansion context is used.  Finally, it is also possible
  to bind `*pattern-expansion-context*` to `:tg` or `:emf` otherwise.  Note
  that this binding has to be available at compile-time."

  {:arglists '([name doc-string? attr-map? [args] [pattern] result-spec?]
                 [name doc-string? attr-map? ([args] [pattern] result-spec?)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              (:pattern-expansion-context (meta *ns*))
                                              *pattern-expansion-context*)]
      `(defn ~name ~(meta name)
         ~@(if (seq? (first more))
             (doall (map convert-spec more))
             (convert-spec more))))))

