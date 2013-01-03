(ns funnyqt.query.tg
  "Functional TGraph querying.

Adjacences
==========

To traverse the neighboring vertices of some given vertex by role names, there
is the adjacences function `adjs`.  Here are some examples:

    ;; Returns all Localities contained in the County my-county-vertex.
    (adjs my-county-vertex :localities)

    ;; Returns all Crossroad vertices that are contained in Localities that are
    ;; contained in the County my-county-vertex
    (adjs my-county-vertex :localities :crossroads)

Regular Path Expressions
========================

Just like GReQL, FunQL has path expressions.  In contrast to GReQL, there're no
path systems or slices, yet, e.g., any path expression simply calculates the
ordered set of reachable vertices.

The basic path functions traverse one edge at a time given a start vertex or a
collection of start vertices.

    -->, --->, <--, <---, <->, <-->         ;; restrict by edge direction
    <>--, <_>--, <*>--, --<>, --<_>, --<*>  ;; restrict by aggregation kind

All of them can be restricted by type (see `funnyqt.protocols/type-matcher`)
and an arbitrary predicate on the edges.  These basic path functions can then
be combined using these generic, regular path expression functions, which are
provided by the `funnyqt.query` namespace:

    p-seq   ;; sequence
    p-opt   ;; option
    p-alt   ;; alternative
    p-exp   ;; iteration by exponent
    p-*     ;; zero-or-many iteration
    p-+     ;; one-or-many iteration
    p-restr ;; filters vertices by class and predicate

For decoupling what is the \"path description\" from the function application,
there is the function `reachables` which accepts a start vertex or a collection of
start vertices and a path description as nested vector and chains the start or
reachable vertices thru.  Here's an example:

    ;; From a Class `c`, calculates all coupled classes.
    (reachables c [p-seq
                    [<-- 'IsClassBlockOf]
                    [<-- 'IsMemberOf]
                    [p-alt
                      [<-- 'IsCalledByMethod]
                      [p-seq
                        [<-- ['IsBodyOfMethod 'IsFieldCreationOf]]
                        [p-* [<-- 'IsStatementOf]]
                        [<-- 'IsDeclarationOfAccessedField]
                        [--> 'IsFieldCreationOf]]]
                    [--> 'IsMemberOf]
                    [--> 'IsClassBlockOf]
                    [p-restr nil #(not (= c %1))]])

Traversal Contexts
==================

The graph traversal (using the sequence functions above, or the basic accessors
from core) can be restricted by traversal contexts.  There are the functions
`vsubgraph` and `esubgraph` for creating traversal contexts corresponding to
vertex or edge induced subgraphs.

To set the traversal contexts, use the macro `on-subgraph`.  In its body, the
traversal context is set, and when leaving the body, the traversal context is
set to what it was before automatically, even if the body is left because an
exception is thrown.  Here's an example (taken from the tests):

    (on-subgraph [my-route-graph
                  (vsubgraph my-route-graph 'junctions.Airport)]
      (is (== 3 (vcount my-route-graph)))
      (is (== 3 (ecount my-route-graph))))

Aggregating Attribute Values
============================

Using the function `reduce-values`, it's easy to aggregate attribute values.
`It`s similar to clojure's `reduce` function with a mandatory starting value,
but allows for specifying a chain of accessors for composite attributes.  For
example, to calculate the average founding year of all localities, where the
founding date is a record with components \"day\", \"month\", and \"year\" we
can compute that like so:

    (let [locs (vseq (rg) 'localities.Locality)]
      (/ (reduce-values + 0 locs :foundingDate :year)
         (count locs)))"
  (:use funnyqt.tg)
  (:use funnyqt.protocols)
  (:use funnyqt.utils)
  (:use ordered.set)
  (:use funnyqt.query)
  (:require clojure.string
            [clojure.core.reducers :as r])
  (:import
   (de.uni_koblenz.jgralab.algolib.algorithms.search IterativeDepthFirstSearch)
   (de.uni_koblenz.jgralab.algolib.functions.entries PermutationEntry)
   (de.uni_koblenz.jgralab.algolib.problems TopologicalOrderSolver AcyclicitySolver)
   (de.uni_koblenz.jgralab.algolib.algorithms.topological_order
    KahnKnuthAlgorithm TopologicalOrderWithDFS)
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement TraversalContext
                           EdgeDirection)
   (de.uni_koblenz.jgralab.schema Attribute RecordDomain GraphClass
                                  GraphElementClass AttributedElementClass VertexClass
                                  EdgeClass AggregationKind)
   (de.uni_koblenz.jgralab.schema.impl DirectedSchemaEdgeClass)
   (de.uni_koblenz.jgralab.graphmarker SubGraphMarker)))

(def ^:private iseq-internal (ns-resolve (find-ns 'funnyqt.tg) 'iseq-internal))

;;# Funlib

(defn reduce-values
  "Reduces `f` thru the `a`-attribute values of the elements in `coll`.
  `f` must be a function of 2 args (see `clojure.core/reduce').  `val` is a
  starting value.  `s` may be additional accessors, if `a` is a composite
  attribute.  Each additional accessor may be either a function (which is
  simply applied) or another keyword, string, or symbol denoting a record
  component."
  [f val coll a & as]
  (reduce f val (map (if as
                       #(loop [v (value % a), r as]
                          (if (seq r)
                            (let [acc (first r)]
                              (recur (if (fn? acc)
                                       (acc v)
                                       (value v acc))
                                     (rest r)))
                            v))
                       #(value % a))
                     coll)))

;;# Adjancencies

(defn- maybe-traverse [^Vertex v role allow-unknown-ref single-valued]
  (let [role (name role)]
    (if-let [^DirectedSchemaEdgeClass dec
             (.getDirectedEdgeClassForFarEndRole
              ^VertexClass (attributed-element-class v)
              role)]
      (if single-valued
        (let [^EdgeClass ec (.getEdgeClass dec)
              dir (.getDirection dec)
              ub (if (= dir EdgeDirection/OUT)
                   (-> ec .getTo .getMax)
                   (-> ec .getFrom .getMax))]
          (if (= ub 1)
            (seq (.adjacences v role))
            (errorf "Must not call adj on role '%s' (EdgeClass %s) with upper bound %s."
                    role ec ub)))
        (seq (.adjacences v role)))
      (when-not allow-unknown-ref
        (errorf "No %s role at vertex %s" role v)))))

(defn- zero-or-one [s]
  (if (next s)
    (errorf "More than one adjacent vertex found: %s" s)
    (first s)))

(extend-protocol Adjacencies
  Vertex
  (adj-internal [this roles]
    (if (seq roles)
      (when-let [target (zero-or-one (maybe-traverse this (first roles) false true))]
        (recur target (rest roles)))
      this))
  (adj*-internal [this roles]
    (if (seq roles)
      (when-let [target (zero-or-one (maybe-traverse this (first roles) true true))]
        (recur target (rest roles)))
      this))
  (adjs-internal
    ([this]
       (map that (iseq this)))
    ([this roles]
       (loop [r [this], roles roles]
         (if (and (seq roles) (seq r))
           (recur (mapcat #(maybe-traverse % (first roles) false false) r)
                  (rest roles))
           r))))
  (adjs*-internal [this roles]
    (loop [r [this], roles roles]
      (if (and (seq roles) (seq r))
        (recur (mapcat #(maybe-traverse % (first roles) true false) r)
               (rest roles))
        r))))


;;# Traversal Context

(defmacro on-graph
  "Disables the graph `g`s current traversal context for the execution of
  `body`.  Guaranteed to restore the old traversal context after `body`
  finished (even if it errored).

  Also see `on-subgraph`, and `on-subgraph-intersection`."
  [[g] & body]
  `(on-subgraph [~g nil]
     ~@body))

(defmacro on-subgraph
  "Sets the TraversalContext of `g` to `tc` and then executes `body`.
  Guaranteed to restore the old TraversalContext after `body` finished (even if
  it errored).

  Also see `vsubgraph`, `esubgraph`, and `on-subgraph-intersection`."
  [[g tc] & body]
  `(let [^Graph g# ~g
         ^TraversalContext old-tc# (.getTraversalContext g#)]
     (try
       (.setTraversalContext g# ~tc)
       ~@body
       (finally (.setTraversalContext g# old-tc#)))))

(defn merge-traversal-contexts
  "Returns a TraversalContext that accepts only elements that are accepted by
  both `tc1' and `tc2'.
  (Don't use this direcly, but use `on-subgraph-intersection`.)"
  [^TraversalContext tc1 ^TraversalContext tc2]
  (cond
   (nil? tc1) tc2
   (nil? tc2) tc1
   :else (reify TraversalContext
           (containsVertex [_ v]
             (and (.containsVertex tc1 v)
                  (.containsVertex tc2 v)))
           (containsEdge [_ e]
             (and (.containsEdge tc1 e)
                  (.containsEdge tc2 e))))))

(defmacro on-subgraph-intersection
  "Sets the TraversalContext of `g` to a new TraversalContext that accepts only
  elements which both `tc` and `g`s current TraversalContext accept and then
  executes `body`.  Guaranteed to restore the old TraversalContext.

  Also see `vsubgraph`, `esubgraph`, and `on-subgraph`."
  [[g tc] & body]
  `(let [^Graph g# ~g
         ^TraversalContext old-tc# (.getTraversalContext g#)]
     (try
       (.setTraversalContext g# (merge-traversal-contexts old-tc# ~tc))
       ~@body
       (finally (.setTraversalContext g# old-tc#)))))

(defn- vsubgraph-tc
  "Returns a TraversalContext of a vertex induced subgraph restricted by `pred`
  on the vertices.  All vertices satisfying `pred` are accepted plus all edges
  between accepted vertices."
  [g pred precalc]
  (let [vp #(boolean (pred %))
        ep #(boolean (and (vp (alpha %))
                          (vp (omega %))))]
    (if precalc
      (let [^SubGraphMarker sgm (SubGraphMarker. g)]
        (doseq [^Vertex v (filter vp (vseq g))]
          (.mark sgm v))
        (doseq [^Edge e (filter #(and (.containsVertex sgm (alpha %))
                                      (.containsVertex sgm (omega %)))
                                (eseq g))]
          (.mark sgm e))
        sgm)
      (reify TraversalContext
        (containsVertex [_ v]
          (vp v))
        (containsEdge [_ e]
          (ep e))))))

(defn vsubgraph
  "Returns a vertex induced subgraph of `g` restricted by `pred` in terms of a
  TraversalContext.  `pred` may be a predicate that is used to filter the
  vertices of `g`, a type specification (see `type-spec`) or a collection of
  vertices.  The subgraph contains all vertices matching the predicate, and all
  edges/incidences that are connected to vertices that are both in the
  subgraph.  If `precalc` is true (the default), then pre-calculate the
  accepted elements beforehand as a SubGraphMarker.  That speeds up the
  procedure enormously, but doesn't allow for using the returned traversal
  context on a graph that changes in between.

  Also see `esubgraph` and `on-subgraph`."
  ([g pred]
     (vsubgraph g pred true))
  ([g pred precalc]
     (cond
      (fn? pred)        (vsubgraph-tc g pred precalc)
      (type-spec? pred) (vsubgraph-tc g (type-matcher g pred) precalc)
      (coll? pred)      (vsubgraph-tc g #(member? % pred) precalc)
      :default          (error (str "Don't know how to handle predicate " pred)))))

(defn- esubgraph-tc
  "Returns a TraversalContext of an edge induced subgraph restricted by `pred`
  on the edges.  All edges satisfying `pred` are accepted plus all vertices
  that are connected to at least one accepted edge.  If `precalc` is non-nil,
  precalculate accepted elements before, which is much faster."
  [g pred precalc]
  (let [ep #(boolean (pred %))
        vp #(boolean (some ep (iseq %)))]
    (if precalc
      (let [^SubGraphMarker sgm (SubGraphMarker. g)]
        (doseq [^Edge e (filter ep (eseq g))]
          (.mark sgm e))
        sgm)
      (reify TraversalContext
        (containsVertex [_ v]
          (vp v))
        (containsEdge [_ e]
          (ep e))))))

(defn esubgraph
  "Returns an edge induced subgraph of `g` restricted by `pred` in terms of a
  TraversalContext.  `pred` may be a predicate that is used to filter the edges
  of `g`, a type specification (see `type-spec`) or a collection of edges.  The
  subgraph contains all edges matching the predicate including their start and
  end vertices.  If `precalc` is true (the default), then pre-calculate the
  accepted elements beforehand as a SubGraphMarker.  That speeds up the
  procedure enormously, but doesn't allow for using the returned traversal
  context on a graph that changes in between.

  Also see `vsubgraph` and `on-subgraph`."
  ([g pred]
     (esubgraph g pred true))
  ([g pred precalc]
     (cond
      (fn? pred)        (esubgraph-tc g pred precalc)
      (type-spec? pred) (esubgraph-tc g (type-matcher g pred) precalc)
      (coll? pred)      (esubgraph-tc g #(member? % pred) precalc)
      :default          (error (str "Don't know how to handle predicate " pred)))))


;;# Path Functions

(defn- p-apply-tg
  [v p]
  (cond
   ;; funs: -->
   (fn? p) (p v)
   ;; funs with params: [--> 'Foo], [p-alt --> <>--]
   (coll? p) (apply (first p) v (rest p))
   ;; adjacences / that-role names
   (prop-name? p) (into (ordered.set/ordered-set)
                        (r/mapcat #(adjs* % p) v))
   :else (errorf "Don't know how to apply %s." p)))

(defn- p-restr-tg
  "Vertex restriction concerning `ts` and `pred` on each vertex in `vs`.
  ts is a type specification (see `funnyqt.protocols/type-matcher`)."
  ([vs ts]
     (p-restr-tg vs ts identity))
  ([vs ts pred]
     (let [vs (oset vs)]
       (oset
        (if (seq vs)
          (let [tm (type-matcher (first vs) ts)]
            (into (ordered.set/ordered-set)
                  (r/filter (every-pred tm pred) vs)))
          vs)))))

(defn reachables
  "Returns the ordered set of vertices reachable from `v` by via the path
  description `p`.
  `v` may be a vertex or a seq of vertices."
  [v p]
  (binding [*p-apply* p-apply-tg
            *p-restr* p-restr-tg]
    (*p-apply* v p)))

(defn- ---
  "Returns the vertices reachable from `v` via incidences with direction `dir`
  and aggregation kinds, restricted by `ts`, and `pred` (on the edges)."
  [v dir this-aks that-aks ts pred]
  (let [vs (oset v)]
    (if (seq vs)
      (let [complete-pred (every-pred
                           (or pred identity)
                           (if (seq this-aks)
                             #(member? (.getThisAggregationKind ^Edge %) this-aks)
                             identity)
                           (if (seq that-aks)
                             #(member? (.getThatAggregationKind ^Edge %) that-aks)
                             identity))
            tm (type-matcher (first vs) ts)
            dm (direction-matcher dir)]
        (into
         (ordered-set)
         (r/mapcat (fn [sv]
                   (r/map that
                          (r/filter complete-pred
                                    (iseq-internal sv tm dm))))
                 vs)))
      (ordered-set))))

(defn -->
  "Returns the vertices reachable from `v` via outgoing incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (--> v nil nil))
  ([v ts]
     (--> v ts nil))
  ([v ts pred]
     (--- v :out nil nil ts pred)))

(defn <--
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<-- v nil nil))
  ([v ts]
     (<-- v ts nil))
  ([v ts pred]
     (--- v :in nil nil ts pred)))

(defn <->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<-> v nil nil))
  ([v ts]
     (<-> v ts nil))
  ([v ts pred]
     (--- v :inout nil nil ts pred)))

(defn --->
  "Returns the vertices reachable from `v` via outgoing incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `-->`, traversal of edges with aggregation semantics is forbidden."
  ([v]
     (---> v nil nil))
  ([v ts]
     (---> v ts nil))
  ([v ts pred]
     (--- v :out [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <---
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<--', traversal of edges with aggregation semantics is forbidden."
  ([v]
     (<--- v nil nil))
  ([v ts]
     (<--- v ts nil))
  ([v ts pred]
     (--- v :in [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <-->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<->', traversal of edges with aggregation semantics is forbidden."
  ([v]
     (<--> v nil nil))
  ([v ts]
     (<--> v ts nil))
  ([v ts pred]
     (--- v :inout [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <>--
  "Aggregation path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (<>-- v nil nil))
  ([v ts]
     (<>-- v ts nil))
  ([v ts pred]
     (--- v :inout
          nil [AggregationKind/SHARED AggregationKind/COMPOSITE]
          ts pred)))

(defn --<>
  "Aggregation path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (--<> v nil nil))
  ([v ts]
     (--<> v ts nil))
  ([v ts pred]
     (--- v :inout
          [AggregationKind/SHARED AggregationKind/COMPOSITE] nil
          ts pred)))

(defn <_>--
  "Aggregation-only path expression starting at whole `v`, optionally
  restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<_>-- v nil nil))
  ([v ts]
     (<_>-- v ts nil))
  ([v ts pred]
     (--- v :inout
          nil [AggregationKind/SHARED]
          ts pred)))

(defn --<_>
  "Aggregation-only path expression starting at part `v`, optionally restricted
  by `ts` and `pred` (on the edges)."
  ([v]
     (--<_> v nil nil))
  ([v ts]
     (--<_> v ts nil))
  ([v ts pred]
     (--- v :inout
          [AggregationKind/SHARED] nil
          ts pred)))

(defn <*>--
  "Composition path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (<*>-- v nil nil))
  ([v ts]
     (<*>-- v ts nil))
  ([v ts pred]
     (--- v :inout
          nil [AggregationKind/COMPOSITE]
          ts pred)))

(defn --<*>
  "Composition path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (--<*> v nil nil))
  ([v ts]
     (--<*> v ts nil))
  ([v ts pred]
     (--- v :inout
          [AggregationKind/COMPOSITE] nil
          ts pred)))

;;# Describe Schema and Graph Elements

(defn- attr-desc
  "Returns a map of aec's own attributes as name-domain pairs."
  [^AttributedElementClass aec]
  (into (sorted-map)
        (for [^Attribute attr (.getOwnAttributeList aec)]
          [(keyword (.getName attr)) (describe (.getDomain attr))])))

(defn- slot-desc
  [^AttributedElement e]
  (let [aec (.getAttributedElementClass e)]
    (into (sorted-map)
          (for [^Attribute attr (.getAttributeList aec)]
            (let [n (.getName attr)]
              [(keyword n) (value e n)])))))

(defn- super-classes
  [^GraphElementClass gec]
  (set (map #(symbol (.getQualifiedName ^GraphElementClass %))
            (.getDirectSuperClasses gec))))

(defn- sub-classes
  [^GraphElementClass gec]
  (set (map #(symbol (.getQualifiedName ^GraphElementClass %))
            (.getDirectSubClasses gec))))

(extend-protocol Describable
  Graph
  (describe [this]
    {:type 'Graph
     :qname (symbol (qname this))
     :slots (slot-desc this)})
  Vertex
  (describe [this]
    {:type 'Vertex
     :qname (symbol (qname this))
     :slots (slot-desc this)})
  Edge
  (describe [this]
    {:type 'Edge
     :qname (symbol (qname this))
     :slots (slot-desc this)
     :alpha (.getAlpha this)
     :omega (.getOmega this)})
  GraphClass
  (describe [this]
    {:type 'GraphClass
     :qname (symbol (.getQualifiedName this))
     :attributes (attr-desc this)})
  VertexClass
  (describe [this]
    {:type 'VertexClass
     :qname (symbol (.getQualifiedName this))
     :attributes (attr-desc this)
     :super-classes (super-classes this)
     :sub-classes (sub-classes this)})
  EdgeClass
  (describe [this]
    {:type 'EdgeClass
     :qname (symbol (.getQualifiedName this))
     :attributes (attr-desc this)
     :from-vc (-> this .getFrom .getVertexClass .getQualifiedName symbol)
     :to-vc (-> this .getTo .getVertexClass .getQualifiedName symbol)
     :super-classes (super-classes this)
     :sub-classes (sub-classes this)})
  de.uni_koblenz.jgralab.schema.BasicDomain
  (describe [this]
    (-> this .getQualifiedName symbol))
  RecordDomain
  (describe [this]
    {:type 'Record
     :qname (symbol (.getQualifiedName this))
     :components (into (sorted-map)
                       (for [^de.uni_koblenz.jgralab.schema.RecordDomain$RecordComponent
                             c (.getComponents this)]
                         [(keyword (.getName c)) (describe (.getDomain c))]))})
  de.uni_koblenz.jgralab.schema.EnumDomain
  (describe [this]
    {:type 'Enum
     :qname (symbol (.getQualifiedName this))
     :constants (vec (.getConsts this))})
  de.uni_koblenz.jgralab.schema.CollectionDomain
  (describe [this]
    (symbol (.getQualifiedName this)))
  de.uni_koblenz.jgralab.schema.MapDomain
  (describe [this]
    (symbol (-> this
                .getQualifiedName
                (clojure.string/replace #"\s" "")
                (clojure.string/replace "," "=>")))))

;;# Funlib

(defn degree
  "Returns the degree of vertex `v`, optionally restricted by `ts` and `dir`."
  ([^Vertex v]         (.getDegree v))
  ([^Vertex v ts]     (count (iseq v ts)))
  ([^Vertex v ts dir] (count (iseq v ts dir))))

(defn- topological-sort-clj
  "Returns a vector of `g`s vertices in topological order.
  Returns false, iff the graph is cyclic."
  [g]
  (loop [rem (vseq g), es  #{}, sorted []]
    (if (seq rem)
      (let [gs (group-by (fn [v]
                           (if (seq (remove es (map normal-edge (iseq v nil :in))))
                             false
                             true))
                         rem)
            good (gs true)
            bad (gs false)]
        ;;(println (count rem) ": good" (count good) "bad" (count bad))
        (if (seq good)
          (recur bad
                 (into es (mapcat #(iseq % nil :out) good))
                 (into sorted good))
          false))
      sorted)))

(defn topological-sort
  "Returns a seq of `g`s vertices in topological order.
  Returns false, iff the graph is cyclic.  The actual algorithm `alg` may be
  chosen between :dfs (a depth-first variant, the default), :kahn-knuth,
  and :plain (a purely functional clojure implementation, which is nice but
  slow)."
  ([g]
     (topological-sort g :dfs))
  ([g alg]
     (if (= alg :plain)
       (topological-sort-clj g)
       (let [^TopologicalOrderSolver a
             (case alg
               :kahn-knuth (KahnKnuthAlgorithm. g)
               :dfs        (TopologicalOrderWithDFS. g (IterativeDepthFirstSearch. g))
               (error (str "Unknown topo-sort algorithm" alg)))]
         (.execute a)
         (if (.isAcyclic ^AcyclicitySolver a)
           (map #(.getSecond ^PermutationEntry %)
                (seq (.getTopologicalOrder a)))
           false)))))
