(ns funnyqt.query.tg
  "Functional TGraph querying.

Sequence Functions
==================

The central elements of this namespace are the functions `vseq`, `eseq`, and
`iseq`, which return lazy seqs of a graph's vertices or edges, or a lazy seq of
a vertex's incidences.  On that lazy seqs, we can simply use all the powerful
clojure sequence functions like `filter`, `map`, `reduce`, and friends.  For
example, that's how we can pick out all the Locality vertices whose number of
inhabitants is greater than 1000:

    (filter #(> (value % :inhabitants) 1000)
            (vseq my-route-graph 'localities.Locality))

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

All of them can be restricted by type (see `type-matcher` in core) and an
arbitrary predicate on the edges.  These basic path functions can then be
combined using these generic, regular path expression functions, which are
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
  (:require clojure.string)
  (:import
   (de.uni_koblenz.jgralab.algolib.algorithms.search IterativeDepthFirstSearch)
   (de.uni_koblenz.jgralab.algolib.functions.entries PermutationEntry)
   (de.uni_koblenz.jgralab.algolib.problems TopologicalOrderSolver AcyclicitySolver)
   (de.uni_koblenz.jgralab.algolib.algorithms.topological_order KahnKnuthAlgorithm TopologicalOrderWithDFS)
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement TraversalContext)
   (de.uni_koblenz.jgralab.schema Attribute RecordDomain GraphClass
                                  GraphElementClass AttributedElementClass VertexClass EdgeClass
                                  AggregationKind)
   (de.uni_koblenz.jgralab.graphmarker SubGraphMarker)))

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

;;# Lazy Vertex, Edge, Incidence Seqs

(defprotocol VSeq
  "Protocol for types supporting vseq."
  (vseq-internal [this tm]
        "Returns a lazy seq of the graphs vertices restricted by type matcher `tm`."))

(extend-protocol VSeq
  Graph
  (vseq-internal
    [g tm]
    (lazy-seq
     (let [f (first-vertex g tm)]
       (and f (cons f (vseq-internal f tm))))))
  Vertex
  (vseq-internal
    [v tm]
    (lazy-seq
     (let [n (next-vertex v tm)]
       (and n (cons n (vseq-internal n tm)))))))

(alter-meta! (var vseq-internal) assoc :private true)

(defn vseq
  "Returns the lazy seq of vertices of `g` restricted by the type spec `ts`.
  `g` may be a graph or a vertex.  In the latter case, returns all vertices
  following `g` in the vertex sequence."
  ([g]
     (vseq-internal g identity))
  ([g ts]
     (vseq-internal g (type-matcher g ts))))

(defprotocol ESeq
  "Protocol for types supporting eseq."
  (eseq-internal [this tm]
        "Returns a lazy seq of the graph's edges restricted by tm."))

(extend-protocol ESeq
  Graph
  (eseq-internal
    [g tm]
    (lazy-seq
     (let [f (first-edge g tm)]
       (and f (cons f (eseq-internal f tm))))))

  Edge
  (eseq-internal
    [e tm]
    (lazy-seq
     (let [n (next-edge e tm)]
       (and n (cons n (eseq-internal n tm)))))))

(alter-meta! (var eseq-internal) assoc :private true)

(defn eseq
  "Returns the lazy seq of edges of `e` restricted by `ts`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  following `g` in the edge sequence."
  ([g]
     (eseq-internal g identity))
  ([g ts]
     (eseq-internal g (type-matcher g ts))))

(defprotocol ISeq
  "Protocol for types supporting iseq."
  (iseq-internal [this tm dm]
    "Returns a lazy seq of incident edges restricted by tm and dm."))

(extend-protocol ISeq
  Vertex
  (iseq-internal
    [v tm dm]
    (lazy-seq
     (let [f (first-inc v tm dm)]
       (and f (cons f (iseq-internal f tm dm))))))

  Edge
  (iseq-internal
    [e tm dm]
    (lazy-seq
     (let [n (next-inc e tm dm)]
       (and n (cons n (iseq-internal n tm dm)))))))

(alter-meta! (var iseq-internal) assoc :private true)

(defn iseq
  "Returns the lazy seq of incidences of `v` restricted by `ts` and `dir`.
  `v` may be a vertex or an edge.  In the latter case, returns all incidences
  following `v` in the current vertex's incidence sequence."
  ([v]
     (iseq-internal v identity identity))
  ([v ts]
     (iseq-internal v (type-matcher v ts) identity))
  ([v ts dir]
     (iseq-internal v (type-matcher v ts) (direction-matcher dir))))

;;# Adjancencies

(defn- maybe-traverse [^Vertex v role]
  (let [role (name role)]
    (when (.getDirectedEdgeClassForFarEndRole
           ^VertexClass (attributed-element-class v)
           role)
      (seq (.adjacences v role)))))

(defn- zero-or-one [s]
  (if (next s)
    (error (format "More than one adjacent vertex found: %s" s))
    (first s)))

(extend-protocol Adjacencies
  Vertex
  (adj-internal [this roles]
    (if (seq roles)
      (recur (zero-or-one (.adjacences this (name (first roles)))) (rest roles))
      this))
  (adjs-internal [this roles]
    (loop [r [this], roles roles]
      (if (and (seq roles) (seq r))
        (recur (mapcat (fn [^Vertex v]
                         (seq (.adjacences v (name (first roles))))) r)
               (rest roles))
        r)))
  (adj*-internal [this roles]
    (zero-or-one (adjs*-internal this roles)))
  (adjs*-internal [this roles]
    (loop [r [this], roles roles]
      (if (and (seq roles) (seq r))
        (recur (mapcat #(maybe-traverse % (first roles)) r)
               (rest roles))
        r))))

;;# Vertex, edge counts

(defprotocol GraphCounts
  "Protocol for getting vertex and edge counts, possibly restricted by a type
  spec."
  (vcount [g] [g ts]
    "Returns the vertex count of `g` restricted by `ts`.")
  (ecount [g] [g ts]
    "Returns the edge count of `g` restricted by `ts`."))

(extend-protocol GraphCounts
  Graph
  (vcount
    ([g]     (.getVCount g))
    ([g ts] (count (vseq g ts))))
  (ecount
    ([g]     (.getECount g))
    ([g ts] (count (eseq g ts)))))


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
   (qname? p) (to-oset (mapcat #(adjs* % p) (to-oset v)))
   :else (error (format "Don't know how to apply %s." p))))

(defn- p-restr-tg
  "Vertex restriction concerning `ts` and `pred` on each vertex in `vs`.
  ts is a type specification (see `type-matcher`)."
  ([vs ts]
     (p-restr-tg vs ts identity))
  ([vs ts pred]
     (let [vs (to-oset vs)]
       (to-oset
        (if (seq vs)
          (let [tm (type-matcher (first vs) ts)]
            (filter (every-pred tm pred)
                    vs))
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
  (let [vs (to-oset v)]
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
        (to-oset
         (mapcat (fn [sv]
                   (map that
                        (filter complete-pred
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
