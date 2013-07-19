(ns funnyqt.tg
  "Core functions for accessing and manipulating TGraphs.

Loading/Saving
==============

See `load-graph`, `save-graph`, and `load-schema`.

Graph Elements
==============

For accessing elements, see `vertex`, `edge`, `first-vertex`, `next-vertex`,
`first-edge`, `next-edge`, `first-inc`, and `next-inc`.

For creating graphs and elements, see `create-graph`, `create-vertex!`, and
`create-edge!`.

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

Attributes
==========

To access attribute values or record components, use the function `value`.  To
set them, use `set-value!`.  All clojure collections and maps are automatically
converted to JGraLab's pcollections.  In case of RecordDomains and EnumDomains,
whose values are instances of generated classes, there are the constructor
functions `record` and `enum`."
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.utils)
  (:require [clojure.core.cache :as cache]
            [clojure.core.reducers :as r])
  (:import
   (java.awt.event KeyEvent KeyListener)
   (java.util Collection)
   (java.lang.reflect Method)
   (javax.swing JFrame JScrollPane JLabel ImageIcon JOptionPane WindowConstants)
   (de.uni_koblenz.jgralab AttributedElement Graph GraphElement Vertex Edge
			   EdgeDirection GraphIO Record TraversalContext
                           ImplementationType ProgressFunction)
   (de.uni_koblenz.jgralab.graphmarker SubGraphMarker)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain EnumDomain
                                  ListDomain SetDomain MapDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass)
   (de.uni_koblenz.jgralab.schema.impl DirectedSchemaEdgeClass)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.utilities.tg2dot Tg2Dot)
   (de.uni_koblenz.jgralab.utilities.tg2dot.dot GraphVizProgram GraphVizOutputFormat)
   (de.uni_koblenz.jgralab.impl ConsoleProgressFunction)
   (org.pcollections ArrayPMap ArrayPSet ArrayPVector)))

;;# Caches

(def type-matcher-cache
  "A cache from type-specs to type-matchers."
  (cache/soft-cache-factory (hash-map)))

(defn reset-all-tg-caches
  "Resets all EMF specific caches:

    1. the type-matcher-cache"
  []
  (alter-var-root #'type-matcher-cache
                  (constantly (cache/soft-cache-factory (hash-map)))))


;;# Utility Functions and Macros

(defn ^:private impl-type
  [kw]
  (or
   (when (nil? kw) ImplementationType/GENERIC)
   (when (instance? ImplementationType kw) kw)
   (case kw
     :generic     ImplementationType/GENERIC
     :standard    ImplementationType/STANDARD
     ;; Don't error with "no matching clause", but use the better error message
     ;; below.
     nil)
   (errorf "No such implementation type %s" kw)))

;;## Graph utilities

(defn tgtree
  "Shows a simple Swing tree view representation of the graph `g`."
  [g]
  (.setVisible (de.uni_koblenz.jgralab.utilities.tgtree.TGTree. g) true))

(defn load-schema
  "Loads a schema from `file`, and possibly compile it for implementation type
  `impl` (default :generic, i.e., don't compile).  Supported impl types
  are :generic and :standard."
  ([file]
     (load-schema file ImplementationType/GENERIC))
  ([file impl]
     (let [^Schema s (with-open [is (clojure.java.io/input-stream file)]
                       (GraphIO/loadSchemaFromStream
                        (if (and (string? file)
                                 (.endsWith ^String file ".gz"))
                          (java.util.zip.GZIPInputStream. is)
                          is)))
           it (impl-type impl)]
       (.finish s)
       (if (and it (not= it ImplementationType/GENERIC))
         (do
           #_(println
              (format "Loading schema %s, and compiling for implementation type %s."
                      file it))
           (.compile s CodeGeneratorConfiguration/MINIMAL)
           (let [qn  (name (qname s))
                 scm (SchemaClassManager/instance qn)
                 sc  (Class/forName qn true scm)
                 im  (.getMethod sc "instance" (into-array Class []))]
             (.invoke im nil (to-array []))))
         s))))

(defn load-graph
  "Loads a graph from `file` using ImplementationType `impl`,
  defauling to :generic.  The schema will be compiled automagically if needed.
  Supported impl types are :generic and :standard."
  ([file]
     (load-graph file ImplementationType/GENERIC))
  ([file impl]
     (let [^String filename (if (instance? java.io.File file)
                              (.getPath ^java.io.File file)
                              file)
           ^ImplementationType impl (impl-type impl)
           ^ProgressFunction pg (ConsoleProgressFunction. "Loading")]
       (GraphIO/loadGraphFromFile filename impl pg))))

(defn save-graph
  "Saves `g` to `file`."
  [^Graph g ^String file]
  (GraphIO/saveGraphToFile g file (ConsoleProgressFunction. "Saving")))

;;# Schema Access

(extend-protocol QualifiedName
  AttributedElementClass
  (qname [aec]
    (symbol (.getQualifiedName aec)))

  AttributedElement
  (qname [ae]
    (qname (.getAttributedElementClass ae)))

  Schema
  (qname [s]
    (symbol (.getQualifiedName s)))

  ListDomain
  (qname [cd]
    (vector 'List
            (qname (.getBaseDomain cd))))

  SetDomain
  (qname [cd]
    (vector 'Set
            (qname (.getBaseDomain cd))))

  MapDomain
  (qname [cd]
    (vector 'Map
            (qname (.getKeyDomain cd))
            (qname (.getValueDomain cd))))

  Domain
  (qname [d]
    (symbol (.getQualifiedName d))))

(extend-protocol Abstractness
  GraphElementClass
  (abstract? [this]
    (.isAbstract this)))

(defn domain-qname
  "Transforms a domain qname given as symbol, keyword, string, or vector to a
  canonical string representation:"
  [qn]
  (letfn [(domain-vector-qname [v]
            (when (seq v)
              (let [s (map domain-qname (rest v))
                    l (last s)
                    f (butlast s)]
                (apply str (first v) "<"
                       (concat (interleave f (iterate (constantly ", ") ", "))
                               [l ">"])))))]
    (if (coll? qn)
      (domain-vector-qname qn)
      (name qn))))

(def ^:private aec-simple-to-qname-map
  (memoize
   (fn [^Schema s]
     (let [m (atom {})
           gc (.getGraphClass s)]
       (doseq [^GraphElementClass gec (concat (.getVertexClasses gc)
                                              (.getEdgeClasses gc))]
         (swap! m update-in [(.getSimpleName gec)] conj (.getQualifiedName gec)))
       (apply hash-map (mapcat (fn [[k v]]
                                 (if (> (count v) 1)
                                   []
                                   [k (first v)]))
                               @m))))))

(defn attributed-element-class
  "Returns `ae`s AttributedElementClass or the AttributedElementClass with the
  given `qname` in the schema of `ae`.  In the arity 2 version, `ae` may be an
  attributed element, an attributed element class, or a schema."
  (^de.uni_koblenz.jgralab.schema.AttributedElementClass [^AttributedElement ae]
     (.getAttributedElementClass ae))
  (^de.uni_koblenz.jgralab.schema.AttributedElementClass [elem qname]
     (condp instance? elem
       AttributedElement
       (let [^AttributedElement ae elem]
         (or (-> ae .getSchema (.getAttributedElementClass (name qname)))
             (let [qn ((aec-simple-to-qname-map (.getSchema ae)) (name qname))]
               (-> ae .getSchema (.getAttributedElementClass qn)))
             (errorf "No such attributed element class %s" (name qname))))
       AttributedElementClass
       (let [^AttributedElementClass aec elem]
         (or (-> aec .getSchema (.getAttributedElementClass (name qname)))
             (let [qn ((aec-simple-to-qname-map (.getSchema aec)) (name qname))]
               (-> aec .getSchema (.getAttributedElementClass qn)))
             (errorf "No such attributed element class %s" (name qname))))
       Schema
       (let [^Schema s elem]
         (or (.getAttributedElementClass s (name qname))
             (let [qn ((aec-simple-to-qname-map s) (name qname))]
               (.getAttributedElementClass s qn))
             (errorf "No such attributed element class %s" (name qname)))))))

(defn schema
  "Returns the schema of `elem` (an AttributedElement, AttributedElementClass,
  or a Domain)."
  [elem]
  (condp instance? elem
    AttributedElement      (.getSchema ^AttributedElement elem)
    AttributedElementClass (.getSchema ^AttributedElementClass elem)
    Domain                 (.getSchema ^Domain elem)))

(defn domain
  "Returns the Domain `qname` in the schema of `elem`.  `elem` may be an
  AttributedElement, AttributedElementClass, or a Schema."
  [elem qname]
  (if (or (instance? AttributedElement elem)
          (instance? AttributedElementClass elem))
    (or (.getDomain ^Schema (schema elem) (domain-qname qname))
        (errorf "No such domain %s" (domain-qname qname)))
    (or (.getDomain ^Schema elem (domain-qname qname))
        (errorf "No such domain %s" (domain-qname qname)))))

(defn schema-graph
  "Returns the SchemaGraph of `g`."
  [^Graph g]
  (.convert2SchemaGraph
   (de.uni_koblenz.jgralab.utilities.tg2schemagraph.Schema2SchemaGraph.)
   ^Schema (schema g)))

;;# Generic Metamodel Access

(extend-protocol MMClasses
  GraphElementClass
  (mm-classes [aec]
    (let [^GraphClass gc (.getGraphClass aec)]
      (concat (.getVertexClasses gc)
	      (.getEdgeClasses gc)))))

(extend-protocol MMClass
  AttributedElement
  (mm-class [this]
    (.getAttributedElementClass this)))

(extend-protocol MMDirectSuperClasses
  GraphElementClass
  (mm-direct-super-classes [this]
    (seq (.getDirectSuperClasses this))))

;;# Graph Access

(defn graph
  "Returns the graph containing the graph element `ge`."
  [^GraphElement ge]
  (.getGraph ge))

;;## General type predicates

(definline graph?
  "Returns logical true if `g` is a Graph."
  [g]
  `(instance? Graph ~g))

(definline schema?
  "Returns logical true if `s` is a Schema."
  [g]
  `(instance? Schema ~g))

(definline vertex?
  "Returns logical true if `v` is a Vertex."
  [v]
  `(instance? Vertex ~v))

(definline edge?
  "Returns logical true if `e` is an Edge."
  [e]
  `(instance? Edge ~e))

(definline graph-element?
  "Returns logical true if `ge` is a GraphElement."
  [ge]
  `(instance? GraphElement ~ge))

(definline attributed-element?
  "Returns logical true if `ae` is an AttributedElement."
  [ae]
  `(instance? AttributedElement ~ae))

(definline graph-class?
  "Returns logical true if `g` is a GraphClass."
  [g]
  `(instance? GraphClass ~g))

(definline vertex-class?
  "Returns logical true if `v` is a VertexClass."
  [v]
  `(instance? VertexClass ~v))

(definline edge-class?
  "Returns logical true if `e` is an EdgeClass."
  [e]
  `(instance? EdgeClass ~e))

(definline graph-element-class?
  "Returns logical true if `ge` is a GraphElementClass."
  [ge]
  `(instance? GraphElementClass ~ge))

(definline attributed-element-class?
  "Returns logical true if `ae` is an AttributedElementClass."
  [ae]
  `(instance? AttributedElementClass ~ae))

;;## Type Checks

(defn ^:private type-matcher-tg-2
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [g c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        type  (attributed-element-class g qname)]
    (if neg
      (if exact
        (fn [x] (not (identical? type (attributed-element-class x))))
        (fn [x] (not (is-instance? x type))))
      (if exact
        (fn [x] (identical? type (attributed-element-class x)))
        (fn [x] (is-instance? x type))))))

(defn ^:private type-matcher-tg-1
  [g ts]
  (cond
   (nil? ts)   identity
   (fn? ts)    ts
   (qname? ts) (type-matcher-tg-2 g ts)
   (attributed-element-class? ts) (fn [e] (.isInstanceOf ^AttributedElement e ts))
   (coll? ts)  (if (seq ts)
                 (let [f (first ts)
                       [op r] (case f
                                :and  [and-fn  (next ts)]
                                :nand [nand-fn (next ts)]
                                :or   [or-fn   (next ts)]
                                :nor  [nor-fn  (next ts)]
                                :xor  [xor-fn  (next ts)]
                                [or-fn ts])
                       t-matchers (map #(type-matcher-tg-1 g %) r)]
                   (apply op t-matchers))
                 ;; Empty collection given: (), [], that's also ok
                 identity)
   :else (errorf "Don't know how to create a TG type-matcher for %s" ts)))

(defn ^:private type-matcher-tg [^Graph g ts]
  (let [^Schema s (schema g)
        gh (hash g)]
    (if (.isFinished s)
      (if-let [tm (cache/lookup type-matcher-cache [s gh ts])]
        (do (cache/hit type-matcher-cache [s gh ts]) tm)
        (let [tm (type-matcher-tg-1 g ts)]
          (cache/miss type-matcher-cache [s gh ts] tm)
          tm))
      (type-matcher-tg-1 g ts))))

(extend-protocol TypeMatcher
  GraphElement
  (type-matcher [ge ts]
    (type-matcher-tg (graph ge) ts))
  Graph
  (type-matcher [g ts]
    (type-matcher-tg g ts)))

(extend-protocol InstanceOf
  Graph
  (is-instance? [object class]
    (and (instance? GraphClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    ((type-matcher obj spec) obj))
  Vertex
  (is-instance? [object class]
    (and (instance? VertexClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    ((type-matcher obj spec) obj))
  Edge
  (is-instance? [object class]
    (and (instance? EdgeClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    ((type-matcher obj spec) obj)))


;;## Containment

(defn contains-vertex?
  "Returns true if graph `g` contains vertex `v`."
  [^Graph g ^Vertex v]
  (.containsVertex g v))

(defn contains-edge?
  "Returns true if graph `g` contains edge `e`."
  [^Graph g ^Edge e]
  (.containsEdge g e))

;;## Access by ID

(defn id
  "Returns this element's ID."
  [elem]
  (condp instance? elem
    GraphElement (.getId ^GraphElement elem)
    Graph        (.getId ^Graph elem)))

(defn vertex
  "Returns `g`s vertex with the given ID."
  [^Graph g id]
  (.getVertex g id))

(defn edge
  "Returns `g`s edge with the given ID."
  [^Graph g id]
  (.getEdge g id))

;;## Edge functions

(defn alpha
  "Returns the start vertex of edge `e`."
  [^Edge e]
  (.getAlpha e))

(defn omega
  "Returns the end vertex of edge `e`."
  [^Edge e]
  (.getOmega e))

(defn this
  "Returns `e`s this-vertex."
  [^Edge e]
  (.getThis e))

(defn that
  "Returns `e`s that-vertex."
  [^Edge e]
  (.getThat e))

(defn normal-edge
  "Returns `e`s normal (forward-oriented) edge."
  [^Edge e]
  (.getNormalEdge e))

(defn reversed-edge
  "Returns `e`s reversed (backward-oriented) edge."
  [^Edge e]
  (.getReversedEdge e))

(defn normal-edge?
  "Returns true, iff `e` is normal (forward-oriented)."
  [^Edge e]
  (.isNormal e))

(defn inverse-edge
  "Returns the normal edge of a reversed edge, or the reversed edge of a normal edge."
  [e]
  (if (normal-edge? e)
    (reversed-edge e)
    (normal-edge e)))

;;## First, next elements

(defn first-vertex
  "Returns the first vertex of graph `g` accepted by type matcher `tm`."
  ([^Graph g]
     (first-vertex g identity))
  ([^Graph g tm]
     (loop [v (.getFirstVertex g)]
       (if (or (nil? v) (tm v))
         v
         (recur (.getNextVertex v))))))

(defn last-vertex
  "Returns the last vertex of graph `g` accepted by type matcher `tm`."
  ([^Graph g]
     (last-vertex g identity))
  ([^Graph g tm]
     (loop [v (.getLastVertex g)]
       (if (or (nil? v) (tm v))
         v
         (recur (.getPrevVertex v))))))

(defn next-vertex
  "Returns the vertex following `v` in vseq accepted by type matcher `tm`."
  ([^Vertex v]
     (next-vertex v identity))
  ([^Vertex v tm]
     (loop [n (.getNextVertex v)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getNextVertex n))))))

(defn prev-vertex
  "Returns the vertex preceding `v` in vseq accepted by type matcher `tm`."
  ([^Vertex v]
     (prev-vertex v identity))
  ([^Vertex v tm]
     (loop [n (.getPrevVertex v)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getPrevVertex n))))))

(defn first-edge
  "Returns the first edge of graph `g` accepted by type matcher `tm`."
  ([^Graph g]
     (first-edge g identity))
  ([^Graph g tm]
     (loop [e (.getFirstEdge g)]
       (if (or (nil? e) (tm e))
         e
         (recur (.getNextEdge e))))))

(defn last-edge
  "Returns the last edge of graph `g` accepted by type matcher `tm`."
  ([^Graph g]
     (last-edge g identity))
  ([^Graph g tm]
     (loop [e (.getLastEdge g)]
       (if (or (nil? e) (tm e))
         e
         (recur (.getPrevEdge e))))))

(defn next-edge
  "Returns the edge following `e` in eseq accepted by type matcher `tm`."
  ([^Edge e]
     (next-edge e identity))
  ([^Edge e tm]
     (loop [n (.getNextEdge e)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getNextEdge n))))))

(defn prev-edge
  "Returns the edge preceding `e` in eseq accepted by type matcher `tm`."
  ([^Edge e]
     (prev-edge e identity))
  ([^Edge e tm]
     (loop [n (.getPrevEdge e)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getPrevEdge n))))))

(defn direction-matcher
  "Returns a matcher function that accepts edges of direction `dir`.
  `dir` may be an EdgeDirection enum literal or one of the keywords :in, :out,
  or :inout.
  If `dir` is nil, then any direction is allowed (aka :inout)."
  [dir]
  ;; case does a constant time dispatch, so only use cond if the dir was not
  ;; given as keyword (or is nil).
  (case dir
    :out           normal-edge?
    :in            (complement normal-edge?)
    (:inout nil)   identity
    ;; too bad, not nil and not keyword...
    (cond
     (= dir EdgeDirection/OUT)   normal-edge?
     (= dir EdgeDirection/IN)    (complement normal-edge?)
     (= dir EdgeDirection/INOUT) identity
     :default (errorf "Unknown direction %s" dir))))

(defn first-inc
  "Returns the first incidence in iseq of `v` accepted by the type matcher `tm`
  and direction matcher `dm.'"
  ([^Vertex v]
     (first-inc v identity identity))
  ([^Vertex v tm]
     (first-inc v tm identity))
  ([^Vertex v tm dm]
     (loop [i (.getFirstIncidence v)]
       (if (or (nil? i) (and (dm i) (tm i)))
         i
         (recur (.getNextIncidence i))))))

(defn last-inc
  "Returns the last incidence in iseq of `v` accepted by the type matcher `tm`
  and direction matcher `dm.'"
  ([^Vertex v]
     (last-inc v identity identity))
  ([^Vertex v tm]
     (last-inc v tm identity))
  ([^Vertex v tm dm]
     (loop [i (.getLastIncidence v)]
       (if (or (nil? i) (and (dm i) (tm i)))
         i
         (recur (.getPrevIncidence i))))))

(defn next-inc
  "Returns the incidence following `e` in the current vertex's iseq accepted by
  type matcher `tm` and direction matcher `dm`."
  ([^Edge e]
     (next-inc e identity identity))
  ([^Edge e tm]
     (next-inc e tm identity))
  ([^Edge e tm dm]
     (loop [i (.getNextIncidence e)]
       (if (or (nil? i) (and (dm i) (tm i)))
         i
         (recur (.getNextIncidence i))))))

(defn prev-inc
  "Returns the incidence preceding `e` in the current vertex's iseq accepted by
  type matcher `tm` and direction matcher `dm`."
  ([^Edge e]
     (prev-inc e identity identity))
  ([^Edge e tm]
     (prev-inc e tm identity))
  ([^Edge e tm dm]
     (loop [i (.getPrevIncidence e)]
       (if (or (nil? i) (and (dm i) (tm i)))
         i
         (recur (.getPrevIncidence i))))))

;;## Value access (including attribute setting)

(defprotocol ^:private ClojureValues2JGraLabValues
  "Protocol for transforming clojure persistent collections/maps into
  equivalent pcollections and ratios to doubles."
  (clj2jgval [coll]))

(extend-protocol ClojureValues2JGraLabValues
  clojure.lang.ISeq
  (clj2jgval [coll]
    (.plusAll (ArrayPVector/empty) ^Collection (map clj2jgval coll)))

  clojure.lang.IPersistentVector
  (clj2jgval [coll]
    (.plusAll (ArrayPVector/empty) ^Collection (map clj2jgval coll)))

  clojure.lang.IPersistentMap
  (clj2jgval [coll]
    (reduce (fn [m [k v]] (.plus ^org.pcollections.PMap m k v))
            (ArrayPMap/empty)
            (map (fn [k v] [k v])
                 (map clj2jgval (keys coll))
                 (map clj2jgval (vals coll)))))

  clojure.lang.IPersistentSet
  (clj2jgval [coll]
    (.plusAll (ArrayPSet/empty) ^Collection (map clj2jgval coll)))

  Record (clj2jgval [r] r)

  clojure.lang.Ratio (clj2jgval [r] (double r))

  java.lang.Number (clj2jgval [n] n)
  java.lang.String (clj2jgval [s] s)
  java.lang.Boolean (clj2jgval [b] b)

  java.lang.Enum (clj2jgval [e] e)

  ;; PCollections stay as is
  org.pcollections.PCollection (clj2jgval [coll] coll)
  org.pcollections.PMap (clj2jgval [m] m)

  ;; nil stays nil/null
  nil (clj2jgval [_] nil))

(defn value
  "Returns `ae-or-rec`s (an attributed element or record) `attr-or-comp` value."
  [ae-or-rec attr-or-comp]
  (condp instance? ae-or-rec
    AttributedElement (.getAttribute ^AttributedElement ae-or-rec (name attr-or-comp))
    Record            (.getComponent ^Record ae-or-rec (name attr-or-comp))))

(defn set-value!
  "Sets `ae`s (an attributed element) `attr` value to `val` and returns `ae`."
  [^AttributedElement ae attr val]
  (doto ae
    (.setAttribute (name attr) (clj2jgval val))))

(defn record
  "Creates a record of type `t` in the schema of element `e` with component
  values as specified by map `m`, a map from keywords to values.  The map `m`
  must specify all components, and be sure that if a component is of type
  Integer, then use `Integer/valueOf'."
  [e t m]
  (let [^Graph g (if (instance? Graph e) e (graph e))]
    (.createRecord g
                   ^RecordDomain (domain e t)
                   ^java.util.Map (zipmap (map name (keys m))
                                          (vals m)))))

(defn enum-constant
  "Returns the enum constant `c` in the schema of element `e`.
  `c` is the qualified name of the constant, e.g., my.Enum.CONSTANT."
  [^AttributedElement e c]
  (let [^Graph g (if (instance? Graph e) e (graph e))
        [enum constant _] (split-qname c)]
    (.getEnumConstant g
                      ^EnumDomain (domain e enum)
                      ^String constant)))

;;### Generic attribute access

(extend-protocol AttributeValueAccess
  AttributedElement
  (aval [this attr]
    (value this attr))
  (set-aval! [this attr val]
    (set-value! this attr val))
  Record
  (aval [this attr]
    (value this attr)))

;;## Element Order

(defn before?
  "Returns true iff `a` is defined before `b` in the global vertex/edge
  sequence."
  [a b]
  (condp instance? a
    Vertex (.isBefore ^Vertex a b)
    Edge   (.isBeforeEdge ^Edge a b)))

(defn put-before!
  "Puts `a` directly before `b` in the graph's vertex/edge sequence."
  [a b]
  (condp instance? a
    Vertex (.putBefore ^Vertex a b)
    Edge   (.putBeforeEdge ^Edge a b)))

(defn after?
  "Returns true iff `a` is defined after `b` in the global vertex/edge
  sequence."
  [a b]
  (condp instance? a
    Vertex (.isAfter ^Vertex a b)
    Edge   (.isAfterEdge ^Edge a b)))

(defn put-after!
  "Puts `a` directly after `b` in the graph's vertex/edge sequence."
  [a b]
  (condp instance? a
    Vertex (.putAfter ^Vertex a b)
    Edge   (.putAfterEdge ^Edge a b)))

(defn before-inc?
  "Returns true iff `a` is defined before `b` in the incidence sequence of the
  current vertex."
  [^Edge a b]
  (.isBeforeIncidence a b))

(defn put-before-inc!
  "Puts `a` directly before `b` in the current vertex's incidence sequence."
  [^Edge a b]
  (.putIncidenceBefore a b))

(defn after-inc?
  "Returns true iff `a` is defined after `b` in the incidence sequence of the
  current vertex."
  [^Edge a b]
  (.isAfterIncidence a b))

(defn put-after-inc!
  "Puts `a` directly after `b` in the current vertex's incidence sequence."
  [^Edge a b]
  (.putIncidenceAfter a b))

;;## Lazy Vertex, Edge, Incidence Seqs

(defn ^:private vseq-internal-1 [v tm]
  (lazy-seq
   (let [n (next-vertex v tm)]
     (and n (cons n (vseq-internal-1 n tm))))))

(defn ^:private vseq-internal [g tm]
  (condp instance? g
    Vertex (vseq-internal-1 g tm)
    Graph (lazy-seq
           (let [f (first-vertex g tm)]
             (and f (cons f (vseq-internal-1 f tm)))))))

(defn ^:private rvseq-internal-1 [v tm]
  (lazy-seq
   (let [n (prev-vertex v tm)]
     (and n (cons n (rvseq-internal-1 n tm))))))

(defn ^:private rvseq-internal [g tm]
  (condp instance? g
    Vertex (rvseq-internal-1 g tm)
    Graph (lazy-seq
           (let [f (last-vertex g tm)]
             (and f (cons f (rvseq-internal-1 f tm)))))))

(defn vseq
  "Returns the lazy seq of vertices of `g` restricted by the type spec `ts`.
  `g` may be a graph or a vertex.  In the latter case, returns all vertices
  following `g` in the vertex sequence."
  ([g]
     (vseq-internal g identity))
  ([g ts]
     (vseq-internal g (type-matcher g ts))))

(defn rvseq
  "Returns the lazy reversed seq of vertices of `g` restricted by the type spec `ts`.
  `g` may be a graph or a vertex.  In the latter case, returns all vertices
  preceding `g` in the vertex sequence."
  ([g]
     (rvseq-internal g identity))
  ([g ts]
     (rvseq-internal g (type-matcher g ts))))

(defn ^:private eseq-internal-1 [e tm]
  (lazy-seq
   (let [n (next-edge e tm)]
     (and n (cons n (eseq-internal-1 n tm))))))

(defn ^:private eseq-internal [g tm]
  (condp instance? g
    Edge (eseq-internal-1 g tm)
    Graph (lazy-seq
           (let [f (first-edge g tm)]
             (and f (cons f (eseq-internal-1 f tm)))))))

(defn ^:private reseq-internal-1 [e tm]
  (lazy-seq
   (let [n (prev-edge e tm)]
     (and n (cons n (reseq-internal-1 n tm))))))

(defn ^:private reseq-internal [g tm]
  (condp instance? g
    Edge (reseq-internal-1 g tm)
    Graph (lazy-seq
           (let [f (last-edge g tm)]
             (and f (cons f (reseq-internal-1 f tm)))))))

(defn eseq
  "Returns the lazy seq of edges of `e` restricted by `ts`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  following `g` in the edge sequence."
  ([g]
     (eseq-internal g identity))
  ([g ts]
     (eseq-internal g (type-matcher g ts))))

(defn reseq
  "Returns the lazy reversed seq of edges of `e` restricted by `ts`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  preceding `g` in the edge sequence."
  ([g]
     (reseq-internal g identity))
  ([g ts]
     (reseq-internal g (type-matcher g ts))))

(defn ^:private iseq-internal-1 [e tm dm]
  (lazy-seq
   (let [n (next-inc e tm dm)]
     (and n (cons n (iseq-internal-1 n tm dm))))))

(defn ^:private iseq-internal [v tm dm]
  (condp instance? v
    Edge (iseq-internal-1 v tm dm)
    Vertex (lazy-seq
            (let [f (first-inc v tm dm)]
              (and f (cons f (iseq-internal-1 f tm dm)))))))

(defn ^:private riseq-internal-1 [e tm dm]
  (lazy-seq
   (let [n (prev-inc e tm dm)]
     (and n (cons n (riseq-internal-1 n tm dm))))))

(defn ^:private riseq-internal [v tm dm]
  (condp instance? v
    Edge (riseq-internal-1 v tm dm)
    Vertex (lazy-seq
            (let [f (last-inc v tm dm)]
              (and f (cons f (riseq-internal-1 f tm dm)))))))

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

(defn riseq
  "Returns the lazy reversed seq of incidences of `v` restricted by `ts` and `dir`.
  `v` may be a vertex or an edge.  In the latter case, returns all incidences
  preceding `v` in the current vertex's incidence sequence."
  ([v]
     (riseq-internal v identity identity))
  ([v ts]
     (riseq-internal v (type-matcher v ts) identity))
  ([v ts dir]
     (riseq-internal v (type-matcher v ts) (direction-matcher dir))))

(extend-protocol Elements
  Graph
  (elements
    ([this]
       (vseq this))
    ([this ts]
       (vseq this ts))))

(defn container
  "Returns the vertex containing `v`.
  That is, returns the vertex x where there is a composition edge x <*>-- v."
  [v]
  (loop [^Edge inc (first-inc v)]
    (when inc
      (if (= AggregationKind/COMPOSITE (.getThisAggregationKind inc))
        (that inc)
        (recur (next-inc inc))))))

;;## Vertex, edge counts, degree

(defn vcount
  "Returns the vertex count of `g` restricted by `ts`."
  ([^Graph g]    (.getVCount g))
  ([^Graph g ts] (count (vseq g ts))))

(defn ecount
  "Returns the edge count of `g` restricted by `ts`."
  ([^Graph g]    (.getECount g))
  ([^Graph g ts] (count (eseq g ts))))

(defn degree
  "Returns the degree of vertex `v`, optionally restricted by `ts` and `dir`."
  ([^Vertex v]         (.getDegree v))
  ([^Vertex v ts]     (count (iseq v ts)))
  ([^Vertex v ts dir] (count (iseq v ts dir))))

;;# Modifications

;;## Creations

;; TODO: Basically, the impl should be determined by the schema.  Ask Volker!
(defn create-graph
  "Creates a graph with id `gid` of the given `schema` using implementation type `impl`.
  Supported impl types are :generic and :standard.  The graph id defaults to a
  creation timestamp, and the impl type to GENERIC."
  ([schema]
     (create-graph schema (format "Created: %s" (str (java.util.Date.)))))
  ([schema gid]
     (create-graph schema gid ImplementationType/GENERIC))
  ([^Schema schema ^String gid impl]
     (.createGraph schema (impl-type impl) gid
                   (Integer/valueOf 500)
                   (Integer/valueOf 500))))

(defn create-vertex!
  "Creates a new vertex of type `cls` in `g`.
  `cls` is a VertexClass or a qualified name given as string, symbol, or keyword."
  [^Graph g cls]
  (.createVertex g (if (vertex-class? cls)
                     cls
                     (attributed-element-class g cls))))

(extend-protocol CreateElement
  Graph
  (create-element! [g cls]
    (create-vertex! g cls)))

(defn create-edge!
  "Creates a new edge of type `cls` in `g` starting at `from` and ending at `to`.
  `cls` is an EdgeClass or a qualified name given as string, symbol, or keyword."
  [^Graph g cls ^Vertex from ^Vertex to]
  (.createEdge g (if (edge-class? cls)
                   cls
                   (attributed-element-class g cls))
               from to))

(defn set-alpha!
  "Sets the start vertex of `e` to `v` and returns `e`."
  [^Edge e ^Vertex v]
  (doto e (.setAlpha v)))

(defn set-omega!
  "Sets the end vertex of `e` to `v` and returns `e`."
  [^Edge e ^Vertex v]
  (doto e (.setOmega v)))

(defn set-this!
  "Sets the this vertex of `i` to `v` and returns `i`."
  [^Edge i ^Vertex v]
  (doto i (.setThis v)))

(defn set-that!
  "Sets the that vertex of `i` to `v` and returns `i`."
  [^Edge i ^Vertex v]
  (doto i (.setThat v)))

(defn unlink!
  "Unlinks the given vertex, i.e., deletes all incident edges matching `ts` and
  `ds`."
  ([^Vertex v]
     (unlink! v identity identity))
  ([^Vertex v ts]
     (unlink! v ts identity))
  ([^Vertex v ts ds]
     (let [tm (type-matcher v ts)
           dm (direction-matcher ds)]
       (while (when-let [e (first-inc v tm dm)]
                (delete! e))))))

(extend-protocol ModifyAdjacencies
  Vertex
  (set-adjs! [v role vs]
    (let [^de.uni_koblenz.jgralab.schema.impl.DirectedSchemaEdgeClass
          dec (.getDirectedEdgeClassForFarEndRole
               ^VertexClass (attributed-element-class v)
               (name role))
          _  (when-not dec (errorf "No %s role at vertex %s." role v))
          ec (.getEdgeClass dec)
          ed (.getDirection dec)]
      (unlink! v #(is-instance? % ec) ed))
    (add-adjs! v role vs))
  (set-adj! [v1 role v2]
    (set-adjs! v1 role [v2]))
  (add-adjs! [v role vs]
    (doseq [av vs]
      (add-adj! v role av)))
  (add-adj! [v1 role v2]
    (.addAdjacence v1 (name role) v2)))

;;## Deletions

(extend-protocol Deletable
  Vertex
  (delete!
    ([v] (.delete v) v)
    ([v recursive]
       ;; Not recursive, so delete all incidences first.
       (when-not recursive
         (unlink! v))
       (delete! v)))
  Edge
  (delete!
    ([e]   (.delete e) e)
    ([e _] (.delete e) e)))

;;## Relinking edges

(defn relink!
  "Relinks all incidences of vertex `from` to vertex `to` and returns `from`.
  The incidences can be restricted by type spec `ts` and `dir` (see
  `type-matcher` and `direction-matcher`)."
  ([from to]
     (relink! from to identity identity))
  ([from to ts]
     (relink! from to ts identity))
  ([from to ts dir]
     (let [tm (type-matcher from ts)
           dm (direction-matcher dir)]
       (loop [inc (first-inc from tm dm)]
         (when inc
           (set-this! inc to)
           (recur (first-inc from tm dm)))))
     from))

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
  (adjs-internal [this roles]
    (if (seq roles)
      (when-let [a (maybe-traverse this (first roles) false false)]
        (r/mapcat #(adjs-internal % (rest roles))
                  (if (instance? java.util.Collection a) a [a])))
      [this]))
  (adjs*-internal [this roles]
    (if (seq roles)
      (when-let [a (maybe-traverse this (first roles) true false)]
        (r/mapcat #(adjs-internal % (rest roles))
                  (if (instance? java.util.Collection a) a [a])))
      [this])))


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

;;# toString

(defmethod print-method Vertex
  [v out]
  (.write ^java.io.Writer out
          (str "#<v" (id v) ": " (qname v) ">")))

(defmethod print-method Edge
  [e out]
  (.write ^java.io.Writer out
          (str "#<" (if (normal-edge? e) "+" "-") "e"
               (Math/abs ^int (id e)) ": " (qname e) ">")))

(defmethod print-method Graph
  [^Graph g out]
  (.write ^java.io.Writer out
          (str "#<Graph " (id g)
               " (" (.getGraphVersion g)
               "): " (qname g) ">")))

(defmethod print-method VertexClass
  [vc out]
  (.write ^java.io.Writer out
          (str "#<VertexClass " (qname vc) ">")))

(defmethod print-method EdgeClass
  [ec out]
  (.write ^java.io.Writer out
          (str "#<EdgeClass " (qname ec) ">")))

(defmethod print-method GraphClass
  [gc out]
  (.write ^java.io.Writer out
          (str "#<GraphClass " (qname gc) ">")))

