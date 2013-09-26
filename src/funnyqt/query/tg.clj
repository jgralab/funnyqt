(ns funnyqt.query.tg
  "Functional TGraph querying.

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
  (:require clojure.string
            [clojure.core.reducers :as r]
            [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]
            [funnyqt.utils :as u]
            [flatland.ordered.set :as os]
            [funnyqt.query :as q])
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

;;# Path Functions

(defn- p-apply-tg
  [v p]
  (cond
   ;; funs: -->
   (fn? p) (p v)
   ;; funs with params: [--> 'Foo], [p-alt --> <>--]
   (coll? p) (apply (first p) v (rest p))
   ;; adjacences / that-role names
   (u/prop-name? p) (into (os/ordered-set)
                          (r/mapcat #(q/adjs* % p) (u/oset v)))
   :else (u/errorf "Don't know how to apply %s." p)))

(defn- p-restr-tg
  "Vertex restriction concerning `ts` and `pred` on each vertex in `vs`.
  ts is a type specification (see `funnyqt.protocols/type-matcher`)."
  ([vs ts]
     (p-restr-tg vs ts identity))
  ([vs ts pred]
     (let [vs (u/oset vs)]
       (u/oset
        (if (seq vs)
          (let [tm (p/type-matcher (first vs) ts)]
            (into (os/ordered-set)
                  (r/filter (every-pred tm pred) vs)))
          vs)))))

(defn reachables
  "Returns the ordered set of vertices reachable from `v` by via the path
  description `p`.
  `v` may be a vertex or a seq of vertices."
  [v p]
  (binding [q/*p-apply* p-apply-tg
            q/*p-restr* p-restr-tg]
    (q/*p-apply* v p)))

(defn- ---
  "Returns the vertices reachable from `v` via incidences with direction `dir`
  and aggregation kinds, restricted by `ts`, and `pred` (on the edges)."
  [v dir this-aks that-aks ts pred]
  (let [vs (u/oset v)]
    (if (seq vs)
      (let [complete-pred (every-pred
                           (or pred identity)
                           (if (seq this-aks)
                             #(q/member? (.getThisAggregationKind ^Edge %) this-aks)
                             identity)
                           (if (seq that-aks)
                             #(q/member? (.getThatAggregationKind ^Edge %) that-aks)
                             identity))
            tm (p/type-matcher (first vs) ts)
            dm (tg/direction-matcher dir)]
        (into
         (os/ordered-set)
         (r/mapcat (fn [sv]
                   (r/map tg/that
                          (r/filter complete-pred
                                    (iseq-internal sv tm dm))))
                 vs)))
      (os/ordered-set))))

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
                       #(loop [v (tg/value % a), r as]
                          (if (seq r)
                            (let [acc (first r)]
                              (recur (if (fn? acc)
                                       (acc v)
                                       (tg/value v acc))
                                     (rest r)))
                            v))
                       #(tg/value % a))
                     coll)))

(defn topological-sort
  "Returns a seq of `g`s vertices in topological order.
  Returns false, iff the graph is cyclic.  The actual algorithm `alg` may be
  chosen between :dfs (a depth-first variant, the default), and :kahn-knuth."
  ([g]
     (topological-sort g :dfs))
  ([g alg]
     (let [^TopologicalOrderSolver a
           (case alg
             :kahn-knuth (KahnKnuthAlgorithm. g)
             :dfs        (TopologicalOrderWithDFS. g (IterativeDepthFirstSearch. g))
             (u/error (str "Unknown topo-sort algorithm" alg)))]
       (.execute a)
       (if (.isAcyclic ^AcyclicitySolver a)
         (map #(.getSecond ^PermutationEntry %)
              (seq (.getTopologicalOrder a)))
         false))))
