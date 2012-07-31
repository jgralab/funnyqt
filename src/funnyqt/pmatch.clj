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


(defn- name-and-type [sym]
  (if-let [[_ s t] (re-matches #"(?:<-|-)?([a-zA-Z0-9]+)?(?::([a-zA-Z_0-9]+))?(?:-|->)?"
                               (name sym))]
    [(and s (symbol s)) (and t (symbol t))]
    (errorf "No valid pattern symbol: %s" sym)))

(defn- edge-sym? [sym]
  (or (re-matches #"<-[a-zA-Z:_0-9]*-" (name sym))
      (re-matches #"-[a-zA-Z:_0-9]*->" (name sym))))

(defn- edge-dir [esym]
  (if (edge-sym? esym)
    (if (re-matches #"<-.*" (name esym))
      :in
      :out)
    (errorf "%s is not edge symbol." esym)))

(defn- normal-binding-form? [sym form]
  (coll? form))

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
    (loop [pattern pattern, lv nil]
      (when (seq pattern)
        (let [sym (first pattern)
              [n t] (name-and-type sym)]
          (check-unique n t)
          (cond
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (or (#{:when :let :while} sym)
               (normal-binding-form? sym (fnext pattern)))
           (do
             (check-unique n 'SomeType)
             (when (check-unique n 'SomeType))
             (let [v (tg/create-vertex! pg 'ConstraintOrBinding)]
               (tg/set-value! v :form
                              (str "[" (pr-str sym) " "
                                   (pr-str (fnext pattern)) "]"))
               (when lv
                 (tg/create-edge! pg 'Precedes lv v))
               (recur (nnext pattern) v)))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (edge-sym? sym) (let [nsym (second pattern)
                                 [nvn nvt] (name-and-type nsym)
                                 _ (check-unique nvn nvt)
                                 nv (get-or-make-v nvn nvt)]
                             (let [e (if (= :out (edge-dir sym))
                                       (tg/create-edge! pg (if (argset n) 'ArgumentEdge 'PatternEdge) lv nv)
                                       (tg/create-edge! pg (if (argset n) 'ArgumentEdge 'PatternEdge) nv lv))]
                               (when n (tg/set-value! e :name (name n)))
                               (when t (tg/set-value! e :type (name t))))
                             (recur (nnext pattern) nv))
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           :vertex (let [v (get-or-make-v n t)]
                     (recur (rest pattern) v))))))
    pg))

(defn pattern-graph-to-comprehension-tg [argvec pg]
  (let [gsym (first argvec)
        anons (atom {})
        name #(if-let [n (tg/value % :name)]
                (symbol n)
                (if-let [n (@anons %)]
                  n
                  (do
                    (swap! anons assoc % (gensym))
                    (@anons %))))
        add-non-anons (fn [rf done & elems]
                        (if-let [es (seq (map name (remove #(or (keyword? %) (done %) (@anons %)) elems)))]
                          (apply conj rf es)
                          rf))
        type (fn [elem] (when-let [t (tg/value elem :type)]
                         `'~(symbol t)))
        enqueue-incs (fn [cur queue done]
                       (if-let [incs (seq (remove done (tgq/iseq cur)))]
                         (apply conj (pop queue) incs)
                         (pop queue)))]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY
                       (tg/first-vertex pg (tg/type-matcher pg 'APatternVertex)))
           done #{}
           bf []
           rf []]
      (if (seq queue)
        (let [cur (peek queue)]
          (println (describe cur) ", done? " (done cur))
          (if (done cur)
            (recur (pop queue) done bf rf)
            (case (qname cur)
              PatternVertex (recur (enqueue-incs cur queue done)
                                   (conj done cur)
                                   (apply conj bf `[~(name cur) (tgq/vseq ~gsym ~(type cur))])
                                   (add-non-anons rf done cur))
              ArgumentVertex (recur (enqueue-incs cur queue done)
                                    (conj done cur)
                                    bf
                                    (add-non-anons rf cur))
              PatternEdge (let [trg (tg/that cur)]
                            (recur (pop queue)
                                   (conj done (tg/inverse-edge cur) trg)
                                   (apply conj bf `~(name cur) `(tgq/iseq ~(name (tg/this cur)) ~(type cur))
                                          (if (done trg)
                                            [:when `(= ~(name trg) (tg/that ~(name cur)))]
                                            (concat
                                             [:let `[~(name trg) (tg/that ~(name cur))]]
                                             (when-let [t (type trg)]
                                               `[:when (has-type? ~(name trg) ~(type trg))]))))
                                   (add-non-anons rf done cur trg)))
              ArgumentEdge (let [src (tg/this cur)
                                 trg (tg/that cur)]
                             (recur (pop queue)
                                    (conj done cur (tg/inverse-edge cur) trg)
                                    (apply conj bf :when `(= ~(name src) (tg/this ~(name cur)))
                                           (if (done trg)
                                             [:when `(= ~(name trg) (tg/that ~(name cur)))]
                                             (concat
                                              [:let `[~(name trg) (tg/that ~(name cur))]]
                                              (when-let [t (type trg)]
                                                `[:when (has-type? ~(name trg) ~(type trg))]))))
                                    (add-non-anons rf done cur trg)))
              Precedes (let [cob (tg/that cur)
                             allcobs (tgq/reachables cob [q/p-* [tgq/--> 'Precedes]])
                             forms (mapcat #(read-string (tg/value % :form)) allcobs)]
                         (recur (pop queue)
                                (conj done cur)
                                (apply conj bf forms)
                                (apply add-non-anons rf done (map first (partition 2 forms))))))))
        [bf rf]))))

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

(defn- verify-match-vector
  "Ensure that the match vector `match` and the arg vector `args` are disjoint.
  Throws an exception if they overlap, else returns `match`."
  [match args]
  (let [blist (bindings-to-arglist match)]
    (if (seq (clojure.set/intersection
              (set blist)
              (set args)))
      (errorf "Arglist and match vector overlap!")
      (if-let [double-syms (seq (mapcat (fn [[sym freq]]
                                          (when (> freq 1)
                                            (str "- " sym " is declared " freq " times\n")))
                                        (frequencies blist)))]
        (errorf "These symbols are declared multiple times:\n%s"
                (apply str double-syms))
        match))))

(def ^:dynamic *pattern-match-context*
  nil)

(defn transform-match-vector
  "Transforms patterns like a:X -:role-> b:Y to `for` syntax.
  (Only used internally)"
  [match args]
  ;; NOTE: the first element in args must be the graph/model symbol...
  (verify-match-vector
   (shortcut-bindings
    (case *pattern-match-context*
      ;; TODO: Implement me!
      :tgraph (errorf "Not yet implemented.")
      :emf    (errorf "Not yet implemented.")
      match))
   args))

(defn- convert-spec [[a m]]
  (let [tm (transform-match-vector m a)]
    `(~a
      (for ~tm
        ~(bindings-to-arglist tm)))))

(defmacro defpattern
  "Defines a pattern with `name`, optional `doc-string`, optional `attr-map`,
  an `args` vector, and a `match` vector.  When invoked, it returns a lazy seq
  of all matches of `match`.

  Usually, you use this to specify a pattern that occurs in the match pattern
  of many rules.  So instead of writing a match vector like

    [a (vseq g), b (iseq a) :let [c (that b)], ...]

  in several rules, you do

    (defpattern abc [g] [a (vseq g), b (iseq a) :let [c (that b)]])

  and then

    [[a b c] (abc g), ...]

  in the rules."
  {:arglists '([name doc-string? attr-map? [args] [match]]
                 [name doc-string? attr-map? ([args] [match])+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)]
    `(defn ~name ~(meta name)
      ~@(if (seq? (first more))
          (map convert-spec more)
          (convert-spec more)))))
