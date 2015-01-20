(ns funnyqt.tg
  "Core functions for accessing and manipulating TGraphs.

Loading/Saving
==============

See `load-graph`, `save-graph`, and `load-schema`.

Graph Elements
==============

For accessing elements, see `vertex`, `edge`, `first-vertex`, `next-vertex`,
`first-edge`, `next-edge`, `first-inc`, and `next-inc`.

For creating graphs and elements, see `new-graph`, `create-vertex!`, and
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
functions `record` and `enum-constant`."
  (:require [clojure.core.cache         :as cache]
            [clojure.core.reducers      :as r]
            [clojure.string             :as str]
            [flatland.ordered.set       :as os]
            [funnyqt.query              :as q]
            [funnyqt.generic            :as g]
            [funnyqt.internal           :as i]
            [funnyqt.utils              :as u])
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
                                  ListDomain SetDomain MapDomain CollectionDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass IncidenceClass)
   (de.uni_koblenz.jgralab.schema.impl DirectedSchemaEdgeClass)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.utilities.tg2dot Tg2Dot)
   (de.uni_koblenz.jgralab.utilities.tg2dot.dot GraphVizProgram GraphVizOutputFormat)
   (de.uni_koblenz.jgralab.impl ConsoleProgressFunction)
   (org.pcollections ArrayPMap ArrayPSet ArrayPVector)))

;;# Caches

(def ^:private +type-matcher-cache+
  "A cache from type-specs to type-matchers."
  (cache/soft-cache-factory (hash-map)))

(defn reset-all-tg-caches
  "Resets all TG specific caches:

    1. the +type-matcher-cache+"
  []
  (alter-var-root #'+type-matcher-cache+
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
   (u/errorf "No such implementation type %s" kw)))

;;## Graph utilities

(defn tgtree
  "Shows a simple Swing tree view representation of the graph `g`."
  [g]
  (.setVisible (de.uni_koblenz.jgralab.utilities.tgtree.TGTree. g) true))

(defn ^:private get-file-name [file]
  (cond
    (instance? java.io.File file)
    (.getPath ^java.io.File file)

    (instance? java.net.URL file)
    (.toString ^java.net.URL file)

    (and (string? file)
         (.exists (clojure.java.io/file file)))
    file

    (and (string? file)
         (clojure.java.io/resource file))
    (.toString (clojure.java.io/resource file))

    :else (u/errorf "Cannot handle file %s." file)))

(defn load-schema
  "Loads a schema from `file`, and possibly compile it for implementation type
  `impl` (default :generic, i.e., don't compile).  Supported impl types
  are :generic and :standard."
  ([file]
   (load-schema file ImplementationType/GENERIC))
  ([file impl]
   (let [file-name (get-file-name file)
         ^Schema s (with-open [is (clojure.java.io/input-stream file-name)]
                     (GraphIO/loadSchemaFromStream
                      (if (and (string? file-name)
                               (.endsWith ^String file-name ".gz"))
                        (java.util.zip.GZIPInputStream. is)
                        is)))
         it (impl-type impl)]
     (.finish s)
     (if (and it (not= it ImplementationType/GENERIC))
       (do
         #_(println
            (format "Loading schema %s, and compiling for implementation type %s."
                    file-name it))
         (.compile s CodeGeneratorConfiguration/MINIMAL)
         (let [qn  (name (g/qname s))
               scm (SchemaClassManager/instance qn)
               sc  (Class/forName qn true scm)
               im  (.getMethod sc "instance" (into-array Class []))]
           (.invoke im nil (to-array []))))
       s))))

(alter-var-root #'g/mm-load-handlers assoc #".*\.tg(z|\.gz)?$" load-schema)

(defn save-schema
  "Saves schema `s` to `file`."
  [^Schema g ^String file]
  (GraphIO/saveSchemaToFile g file))

(defn load-graph
  "Loads a graph from `file` using ImplementationType `impl`,
  defauling to :generic.  The schema will be compiled automagically if needed.
  Supported impl types are :generic and :standard."
  ([file]
   (load-graph file ImplementationType/GENERIC))
  ([file impl]
   (let [^String filename (get-file-name file)
         ^ImplementationType impl (impl-type impl)
         ^ProgressFunction pg (ConsoleProgressFunction. "Loading")]
     (GraphIO/loadGraphFromFile filename impl pg))))

(defn save-graph
  "Saves graph `g` to `file`."
  [^Graph g ^String file]
  (GraphIO/saveGraphToFile g file (ConsoleProgressFunction. "Saving")))

;;# General type predicates

(defn graph?
  "Returns logical true if `g` is a Graph."
  {:inline (fn [x] `(instance? Graph ~x))}
  [g]
  (instance? Graph g))

(defn schema?
  "Returns logical true if `s` is a Schema."
  {:inline (fn [x] `(instance? Schema ~x))}
  [s]
  (instance? Schema s))

(defn vertex?
  "Returns logical true if `v` is a Vertex."
  {:inline (fn [x] `(instance? Vertex ~x))}
  [v]
  (instance? Vertex v))

(defn edge?
  "Returns logical true if `e` is an Edge."
  {:inline (fn [x] `(instance? Edge ~x))}
  [e]
  (instance? Edge e))

(defn graph-element?
  "Returns logical true if `ge` is a GraphElement."
  {:inline (fn [x] `(instance? GraphElement ~x))}
  [ge]
  (instance? GraphElement ge))

(defn attributed-element?
  "Returns logical true if `ae` is an AttributedElement."
  {:inline (fn [x] `(instance? AttributedElement ~x))}
  [ae]
  (instance? AttributedElement ae))

(defn graph-class?
  "Returns logical true if `gc` is a GraphClass."
  {:inline (fn [x] `(instance? GraphClass ~x))}
  [gc]
  (instance? GraphClass gc))

(defn vertex-class?
  "Returns logical true if `vc` is a VertexClass."
  {:inline (fn [x] `(instance? VertexClass ~x))}
  [vc]
  (instance? VertexClass vc))

(defn edge-class?
  "Returns logical true if `ec` is an EdgeClass."
  {:inline (fn [x] `(instance? EdgeClass ~x))}
  [ec]
  (instance? EdgeClass ec))

(defn graph-element-class?
  "Returns logical true if `gec` is a GraphElementClass."
  {:inline (fn [x] `(instance? GraphElementClass ~x))}
  [gec]
  (instance? GraphElementClass gec))

(defn attributed-element-class?
  "Returns logical true if `aec` is an AttributedElementClass."
  {:inline (fn [x] `(instance? AttributedElementClass ~x))}
  [aec]
  (instance? AttributedElementClass aec))

;;# Schema Access

(extend-protocol g/IQualifiedName
  AttributedElementClass
  (qname [aec]
    (symbol (.getQualifiedName aec)))

  AttributedElement
  (qname [ae]
    (g/qname (.getAttributedElementClass ae)))

  Schema
  (qname [s]
    (symbol (.getQualifiedName s)))

  ListDomain
  (qname [cd]
    (vector 'List
            (g/qname (.getBaseDomain cd))))

  SetDomain
  (qname [cd]
    (vector 'Set
            (g/qname (.getBaseDomain cd))))

  MapDomain
  (qname [cd]
    (vector 'Map
            (g/qname (.getKeyDomain cd))
            (g/qname (.getValueDomain cd))))

  Domain
  (qname [d]
    (symbol (.getQualifiedName d))))

(extend-protocol g/IUniqueName
  NamedElement
  (uname [nec]
    (symbol (.getUniqueName nec)))
  AttributedElement
  (uname [ae]
    (g/uname (.getAttributedElementClass ae))))

(extend-protocol g/IMMAbstract
  GraphElementClass
  (mm-abstract? [this]
    (.isAbstract this)))

(extend-protocol g/IUnset
  AttributedElement
  (unset? [this attr]
    (.isUnsetAttribute this (name attr))))

(defn ^:private domain-qname
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

(def ^:private named-element-simple-to-qname-map
  (memoize
   (fn [^Schema s]
     (let [m (atom {})
           gc (.getGraphClass s)]
       (doseq [^NamedElement ne (concat (.getVertexClasses gc)
                                        (.getEdgeClasses gc)
                                        (.getDomains s))]
         (swap! m update-in [(.getSimpleName ne)] conj (.getQualifiedName ne)))
       (apply hash-map (mapcat (fn [[k v]]
                                 (if (> (count v) 1)
                                   []
                                   [k (first v)]))
                               @m))))))

(defn attributed-element-class
  "Returns `ae`s AttributedElementClass or the AttributedElementClass
  with the given `qname` in the schema of `elem`.  In the arity 2
  version, `elem` may be an attributed element, an attributed element
  class, or a schema."
  (^de.uni_koblenz.jgralab.schema.AttributedElementClass
   [^AttributedElement ae]
   (.getAttributedElementClass ae))
  (^de.uni_koblenz.jgralab.schema.AttributedElementClass
   [elem qname]
   (condp instance? elem
     AttributedElement
     (let [^AttributedElement ae elem]
       (or (-> ae .getSchema (.getAttributedElementClass (name qname)))
           (let [qn ((named-element-simple-to-qname-map (.getSchema ae)) (name qname))]
             (-> ae .getSchema (.getAttributedElementClass qn)))
           (u/errorf "No such attributed element class %s" (name qname))))
     AttributedElementClass
     (let [^AttributedElementClass aec elem]
       (or (-> aec .getSchema (.getAttributedElementClass (name qname)))
           (let [qn ((named-element-simple-to-qname-map (.getSchema aec)) (name qname))]
             (-> aec .getSchema (.getAttributedElementClass qn)))
           (u/errorf "No such attributed element class %s" (name qname))))
     Schema
     (let [^Schema s elem]
       (or (.getAttributedElementClass s (name qname))
           (let [qn ((named-element-simple-to-qname-map s) (name qname))]
             (.getAttributedElementClass s qn))
           (u/errorf "No such attributed element class %s" (name qname)))))))

(defn schema
  "Returns the schema of `elem` (an AttributedElement, AttributedElementClass,
  or a Domain)."
  ^de.uni_koblenz.jgralab.schema.Schema [elem]
  (condp instance? elem
    AttributedElement      (.getSchema ^AttributedElement elem)
    AttributedElementClass (.getSchema ^AttributedElementClass elem)
    Domain                 (.getSchema ^Domain elem)))

(defn ^de.uni_koblenz.jgralab.schema.Domain domain
  "Returns the attribute `attr`s domain.
  In the arity 2 variant, returns the Domain with qualified name `qname` in the
  schema of `elem`.  `elem` may be an AttributedElement,
  AttributedElementClass, or a Schema."
  ([^Attribute attr]
   (.getDomain attr))
  ([elem qname]
   (let [^Schema s (if (schema? elem) elem (schema elem))]
     (.getDomain s ((named-element-simple-to-qname-map s)
                    (domain-qname qname))))))

(defn schema-graph
  "Returns the SchemaGraph of schema `s`."
  [^Schema s]
  (.convert2SchemaGraph
   (de.uni_koblenz.jgralab.utilities.tg2schemagraph.Schema2SchemaGraph.)
   s))

;;# Generic Metamodel Access

(extend-protocol g/IMetaModelObject
  AttributedElementClass
  (meta-model-object? [this] true))

(extend-protocol g/IMMElementClasses
  GraphElementClass
  (mm-element-classes [aec]
    (let [^GraphClass gc (.getGraphClass aec)]
      (.getVertexClasses gc)))
  Schema
  (mm-element-classes [schema]
    (let [^GraphClass gc (.getGraphClass schema)]
      (.getVertexClasses gc))))

(extend-protocol g/IMMRelationshipClasses
  GraphElementClass
  (mm-relationship-classes [aec]
    (let [^GraphClass gc (.getGraphClass aec)]
      (.getEdgeClasses gc)))
  Schema
  (mm-relationship-classes [schema]
    (let [^GraphClass gc (.getGraphClass schema)]
      (.getEdgeClasses gc))))

(extend-protocol g/IMMClass
  AttributedElement
  (mm-class
    ([this]
       (.getAttributedElementClass this))
    ([this qn]
       (if-let [cls (.getAttributedElementClass (.getSchema this) (name qn))]
         cls
         (u/errorf "No such AttributedElementClass: %s." qn))))
  Schema
  (mm-class
    ([this qn]
       (if-let [cls (.getAttributedElementClass this (name qn))]
         cls
         (u/errorf "No such AttributedElementClass: %s." qn)))))

(extend-protocol g/IMMDirectSuperClasses
  GraphElementClass
  (mm-direct-super-classes [this]
    (seq (.getDirectSuperClasses this))))

(extend-protocol g/IMMAllSubClasses
  GraphElementClass
  (mm-all-subclasses [this]
    (seq (.getAllSubClasses this))))

(extend-protocol g/IMMSuperClassOf
  GraphElementClass
  (mm-super-class? [this sub]
    (.isSuperClassOf this sub)))

(extend-protocol g/IMMAttributes
  AttributedElementClass
  (mm-attributes [aec]
    (map (fn [^Attribute attr]
           (keyword (.getName attr)))
         (.getOwnAttributeList aec))))

(extend-protocol g/IMMReferences
  VertexClass
  (mm-references [vc]
    (mapcat (fn [^IncidenceClass ic]
              (let [role (.getRolename ic)]
                (when (seq role)
                  [(keyword role)])))
            (concat (.getValidToFarIncidenceClasses vc)
                    (.getValidFromFarIncidenceClasses vc)))))

(extend-protocol g/IMMBooleanAttribute
  AttributedElementClass
  (mm-boolean-attribute? [aec attr]
    (-> (.getAttribute aec (name attr))
        .getDomain
        .isBoolean)))

(extend-protocol g/IMMMultiValuedProperty
  AttributedElementClass
  (mm-multi-valued-property? [cls prop]
    (if-let [attr (.getAttribute cls (name prop))]
      (let [dom (domain attr)]
        (or (instance? CollectionDomain dom)
            (instance? MapDomain dom)))
      (let [dec (.getDirectedEdgeClassForFarEndRole ^VertexClass cls (name prop))
            _   (when-not dec
                  (u/errorf "No role %s at VertexClass %s."
                            (name prop) (g/qname cls)))
            ec  (.getEdgeClass dec)
            dir (.getDirection dec)]
        (> (.getMax (if (identical? dir EdgeDirection/OUT)
                      (.getTo ec)
                      (.getFrom ec)))
           1)))))

(extend-protocol g/IMMContainmentReference
  VertexClass
  (mm-containment-reference? [this ref-kw]
    (if-let [^DirectedSchemaEdgeClass
             dec (.getDirectedEdgeClassForFarEndRole this (name ref-kw))]
      (let [ec  (.getEdgeClass dec)
            dir (.getDirection dec)
            ic  (if (= dir de.uni_koblenz.jgralab.EdgeDirection/OUT)
                  (.getTo ec)
                  (.getFrom ec))]
        (= (.getAggregationKind ic)
           de.uni_koblenz.jgralab.schema.AggregationKind/COMPOSITE))
      (u/errorf "No such role %s at metamodel class %s." ref-kw this))))

;;# Graph Access

(defn graph
  "Returns the graph containing the graph element `ge`."
  [^GraphElement ge]
  (.getGraph ge))

;;## Type Checks

(defn ^:private type-matcher-tg-2
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [g c]
  (let [v     (u/type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        type  (attributed-element-class g qname)]
    (if neg
      (if exact
        (fn [x] (not (identical? type (attributed-element-class x))))
        (fn [x] (not (g/is-instance? x type))))
      (if exact
        (fn [x] (identical? type (attributed-element-class x)))
        (fn [x] (g/is-instance? x type))))))

(defn ^:private type-matcher-tg-1
  [g ts]
  (cond
    (nil? ts)    identity
    (fn? ts)     ts
    (u/qname? ts) (type-matcher-tg-2 g ts)
    (vector? ts) (if (seq ts)
                   (let [f (first ts)
                         [op r] (case f
                                  :and  [q/and-fn  (next ts)]
                                  :nand [q/nand-fn (next ts)]
                                  :or   [q/or-fn   (next ts)]
                                  :nor  [q/nor-fn  (next ts)]
                                  :xor  [q/xor-fn  (next ts)]
                                  [q/or-fn ts])
                         t-matchers (map #(type-matcher-tg-1 g %) r)]
                     (apply op t-matchers))
                   ;; Empty vector given, that's also ok
                   identity)
    (attributed-element-class? ts) (fn [e] (.isInstanceOf ^AttributedElement e ts))
    ;; {"http://my.nsuri/1.0" ts}, we can ignore the uri for tg, that's EMF specific
    (map? ts)    (type-matcher-tg-1 g (val (first ts)))
    :else (u/errorf "Don't know how to create a TG funnyqt.generic/type-matcher for %s" ts)))

(defn ^:private type-matcher-tg [^Graph g ts]
  (let [^Schema s (schema g)
        gh (hash g)]
    (if (.isFinished s)
      (if-let [tm (cache/lookup +type-matcher-cache+ [s gh ts])]
        (do (cache/hit +type-matcher-cache+ [s gh ts]) tm)
        (let [tm (type-matcher-tg-1 g ts)]
          (cache/miss +type-matcher-cache+ [s gh ts] tm)
          tm))
      (type-matcher-tg-1 g ts))))

(extend-protocol g/ITypeMatcher
  GraphElement
  (type-matcher [ge ts]
    (type-matcher-tg (graph ge) ts))
  Graph
  (type-matcher [g ts]
    (type-matcher-tg g ts)))

(extend-protocol g/IInstanceOf
  Graph
  (is-instance? [object class]
    (and (graph-class? class)
         (.isInstanceOf object class)))
  Vertex
  (is-instance? [object class]
    (and (vertex-class? class)
         (.isInstanceOf object class)))
  Edge
  (is-instance? [object class]
    (and (edge-class? class)
         (.isInstanceOf object class))))


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
  "Returns `g`s vertex with the given `id`."
  [^Graph g id]
  (.getVertex g id))

(defn edge
  "Returns `g`s edge with the given `id`.
  If `id` is negative, returns the reversed edge whose ID equals (abs id)."
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
  "Returns the first vertex of graph `g`.
  May be restricted to the first vertex for which `pred` returns true."
  ([^Graph g]
   (.getFirstVertex g))
  ([^Graph g pred]
   (loop [v (.getFirstVertex g)]
     (if (or (nil? v) (pred v))
       v
       (recur (.getNextVertex v))))))

(defn last-vertex
  "Returns the last vertex of graph `g`.
  May be restricted to the last vertex for which `pred` returns true."
  ([^Graph g]
   (.getLastVertex g))
  ([^Graph g pred]
   (loop [v (.getLastVertex g)]
     (if (or (nil? v) (pred v))
       v
       (recur (.getPrevVertex v))))))

(defn next-vertex
  "Returns the vertex following `v` in vseq.
  May be restricted to the next vertex for which `pred` returns true."
  ([^Vertex v]
   (.getNextVertex v))
  ([^Vertex v pred]
   (loop [n (.getNextVertex v)]
     (if (or (nil? n) (pred n))
       n
       (recur (.getNextVertex n))))))

(defn prev-vertex
  "Returns the vertex preceding `v` in vseq.
  May be restricted to the previous vertex for which `pred` returns true."
  ([^Vertex v]
   (.getPrevVertex v))
  ([^Vertex v pred]
   (loop [n (.getPrevVertex v)]
     (if (or (nil? n) (pred n))
       n
       (recur (.getPrevVertex n))))))

(defn first-edge
  "Returns the first edge of graph `g`.
  May be restricted to the first edge for which `pred` returns true."
  ([^Graph g]
   (.getFirstEdge g))
  ([^Graph g pred]
   (loop [e (.getFirstEdge g)]
     (if (or (nil? e) (pred e))
       e
       (recur (.getNextEdge e))))))

(defn last-edge
  "Returns the last edge of graph `g`.
  May be restricted to the last edge for which `pred` returns true."
  ([^Graph g]
   (.getLastEdge g))
  ([^Graph g pred]
   (loop [e (.getLastEdge g)]
     (if (or (nil? e) (pred e))
       e
       (recur (.getPrevEdge e))))))

(defn next-edge
  "Returns the edge following `e` in eseq.
  May be restricted to the next edge for which `pred` returns true."
  ([^Edge e]
   (.getNextEdge e))
  ([^Edge e pred]
   (loop [n (.getNextEdge e)]
     (if (or (nil? n) (pred n))
       n
       (recur (.getNextEdge n))))))

(defn prev-edge
  "Returns the edge preceding `e` in eseq.
  May be restricted to the previous edge for which `pred` returns true."
  ([^Edge e]
   (.getPrevEdge e))
  ([^Edge e pred]
   (loop [n (.getPrevEdge e)]
     (if (or (nil? n) (pred n))
       n
       (recur (.getPrevEdge n))))))

(defn ^:private direction-matcher [ds]
  ;; case does a constant time dispatch, so only use cond if the ds was not
  ;; given as keyword (or is nil).
  (case ds
    :out           normal-edge?
    :in            (complement normal-edge?)
    (:inout nil)   identity
    ;; too bad, not nil and not keyword...
    (cond
      (= ds EdgeDirection/OUT)   normal-edge?
      (= ds EdgeDirection/IN)    (complement normal-edge?)
      (= ds EdgeDirection/INOUT) identity
      :default (u/errorf "Unknown direction %s" ds))))

(defn first-inc
  "Returns the first incidence in iseq of `v`.
  May be restricted to the first incidence for which `pred` returns true."
  ([^Vertex v]
   (first-inc v identity))
  ([^Vertex v pred]
   (loop [i (.getFirstIncidence v)]
     (if (or (nil? i) (pred i))
       i
       (recur (.getNextIncidence i))))))

(defn last-inc
  "Returns the last incidence in iseq of `v`.
  May be restricted to the last incidence for which `pred` returns true."
  ([^Vertex v]
   (last-inc v identity))
  ([^Vertex v pred]
   (loop [i (.getLastIncidence v)]
     (if (or (nil? i) (pred i))
       i
       (recur (.getPrevIncidence i))))))

(defn next-inc
  "Returns the incidence following `e` in the current vertex's iseq.
  May be restricted to the next incidence for which `pred` returns true."
  ([^Edge e]
   (next-inc e identity))
  ([^Edge e pred]
   (loop [i (.getNextIncidence e)]
     (if (or (nil? i) (pred i))
       i
       (recur (.getNextIncidence i))))))

(defn prev-inc
  "Returns the incidence preceding `e` in the current vertex's iseq.
  May be restricted to the previous incidence for which `pred` returns true."
  ([^Edge e]
   (prev-inc e identity))
  ([^Edge e pred]
   (loop [i (.getPrevIncidence e)]
     (if (or (nil? i) (pred i))
       i
       (recur (.getPrevIncidence i))))))

;;## Value access (including attribute setting)

(defprotocol ^:private IClojureValues2JGraLabValues
             "Protocol for transforming clojure persistent collections/maps into
  equivalent pcollections and ratios to doubles."
             (^:private clj2jgval [coll]))

(extend-protocol IClojureValues2JGraLabValues
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
    Record            (.getComponent ^Record ae-or-rec (name attr-or-comp))
    (throw (IllegalArgumentException.
            ^String (format "Can't take %s value of %s." attr-or-comp ae-or-rec)))))

(defn set-value!
  "Sets `ae`s (an attributed element) `attr` value to `val` and returns `ae`."
  [^AttributedElement ae attr val]
  (let [an (name attr)]
    (if (and (instance? Long val)
             (-> ae
                 .getAttributedElementClass
                 (.getAttribute an)
                 .getDomain
                 .getQualifiedName
                 (= "Integer")))
      (.setAttribute ae an (int val))
      (.setAttribute ae an (clj2jgval val))))
  ae)

(defn record
  "Creates a record of type `qname` in the schema of attributed element
  `ae` with component values as specified by map `m`, a map from
  keywords to values.  The map `m` must specify all components.  Be sure
  to provide integer values as `(int val)` when a record component has
  domain Integer."
  [ae qname m]
  (let [^Graph g (if (graph? ae) ae (graph ae))]
    (.createRecord g
                   ^RecordDomain (domain g qname)
                   ^java.util.Map (zipmap (map name (keys m))
                                          (map clj2jgval (vals m))))))

(defn enum-constant
  "Returns the enum constant `qname` in the schema of attributed element `ae`.
  `qname` is the qualified name of the constant, e.g., my.Enum.CONSTANT."
  [^AttributedElement ae qname]
  (let [^Graph g (if (graph? ae) ae (graph ae))
        [enum constant _] (u/split-qname qname)]
    (.getEnumConstant g
                      ^EnumDomain (domain g enum)
                      ^String constant)))

(extend-protocol g/IEnumConstant
  AttributedElement
  (enum-constant [el const]
    (enum-constant el const)))

;;### Generic attribute access

(extend-protocol g/IAttributeValueAccess
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

(defn ^:private vseq-internal-1 [v pred]
  (lazy-seq
   (let [n (next-vertex v pred)]
     (and n (cons n (vseq-internal-1 n pred))))))

(defn ^:private vseq-internal [g pred]
  (condp instance? g
    Vertex (vseq-internal-1 g pred)
    Graph (lazy-seq
           (let [f (first-vertex g pred)]
             (and f (cons f (vseq-internal-1 f pred)))))))

(defn ^:private rvseq-internal-1 [v pred]
  (lazy-seq
   (let [n (prev-vertex v pred)]
     (and n (cons n (rvseq-internal-1 n pred))))))

(defn ^:private rvseq-internal [g pred]
  (condp instance? g
    Vertex (rvseq-internal-1 g pred)
    Graph (lazy-seq
           (let [f (last-vertex g pred)]
             (and f (cons f (rvseq-internal-1 f pred)))))))

(defn vseq
  "Returns the lazy seq of vertices of `g` restricted by the type spec `ts`.
  `g` may be a graph or a vertex.  In the latter case, returns all vertices
  following `g` in the vertex sequence."
  ([g]
   (vseq-internal g identity))
  ([g ts]
   (vseq-internal g (g/type-matcher g ts))))

(defn rvseq
  "Returns the lazy reversed seq of vertices of `g` restricted by the type spec `ts`.
  `g` may be a graph or a vertex.  In the latter case, returns all vertices
  preceding `g` in the vertex sequence."
  ([g]
   (rvseq-internal g identity))
  ([g ts]
   (rvseq-internal g (g/type-matcher g ts))))

(defn ^:private eseq-internal-1 [e pred]
  (lazy-seq
   (let [n (next-edge e pred)]
     (and n (cons n (eseq-internal-1 n pred))))))

(defn ^:private eseq-internal [g pred]
  (condp instance? g
    Edge (eseq-internal-1 g pred)
    Graph (lazy-seq
           (let [f (first-edge g pred)]
             (and f (cons f (eseq-internal-1 f pred)))))))

(defn ^:private reseq-internal-1 [e pred]
  (lazy-seq
   (let [n (prev-edge e pred)]
     (and n (cons n (reseq-internal-1 n pred))))))

(defn ^:private reseq-internal [g pred]
  (condp instance? g
    Edge (reseq-internal-1 g pred)
    Graph (lazy-seq
           (let [f (last-edge g pred)]
             (and f (cons f (reseq-internal-1 f pred)))))))

(defn eseq
  "Returns the lazy seq of edges of `e` restricted by `ts`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  following `g` in the edge sequence."
  ([g]
   (eseq-internal g identity))
  ([g ts]
   (eseq-internal g (g/type-matcher g ts))))

(defn reseq
  "Returns the lazy reversed seq of edges of `e` restricted by `ts`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  preceding `g` in the edge sequence."
  ([g]
   (reseq-internal g identity))
  ([g ts]
   (reseq-internal g (g/type-matcher g ts))))

(defn ^:private iseq-internal-1 [e pred]
  (lazy-seq
   (let [n (next-inc e pred)]
     (and n (cons n (iseq-internal-1 n pred))))))

(defn ^:private iseq-internal [v pred]
  (condp instance? v
    Edge (iseq-internal-1 v pred)
    Vertex (lazy-seq
            (let [f (first-inc v pred)]
              (and f (cons f (iseq-internal-1 f pred)))))))

(defn ^:private riseq-internal-1 [e pred]
  (lazy-seq
   (let [n (prev-inc e pred)]
     (and n (cons n (riseq-internal-1 n pred))))))

(defn ^:private riseq-internal [v pred]
  (condp instance? v
    Edge (riseq-internal-1 v pred)
    Vertex (lazy-seq
            (let [f (last-inc v pred)]
              (and f (cons f (riseq-internal-1 f pred)))))))

(defn ^:private type-matcher-from-ts-or-role [v ts]
  (if (keyword? ts)
    (if-let [^DirectedSchemaEdgeClass
             dec (.getDirectedEdgeClassForFarEndRole
                  ^VertexClass (attributed-element-class v)
                  (name ts))]
      (every-pred (direction-matcher (.getDirection dec))
                  (g/type-matcher v (.getEdgeClass dec)))
      (u/errorf "No such role %s at %s." ts v))
    (g/type-matcher v ts)))

(defn iseq
  "Returns the lazy seq of incidences of `v` restricted by `ts` and `ds`.
  `v` may be a vertex or an edge.  In the latter case, returns all incidences
  following `v` in the current vertex's incidence sequence.  `ts` is a type
  specification (see `funnyqt.generic/type-matcher`) and `ds` is a direction
  specification (see `funnyqt.generic/direction-matcher`).

  In fact, `ts` may also be a keyword naming a far-end role name, but this
  feature is only an implementation detail."
  ([v]
   (iseq-internal v identity))
  ([v ts]
   (iseq-internal v (type-matcher-from-ts-or-role v ts)))
  ([v ts ds]
   (iseq-internal v (every-pred (direction-matcher ds) (type-matcher-from-ts-or-role v ts)))))

(defn riseq
  "Returns the lazy reversed seq of incidences of `v` restricted by `ts` and `ds`.
  `v` may be a vertex or an edge.  In the latter case, returns all incidences
  preceding `v` in the current vertex's incidence sequence.  `ts` is a type
  specification (see `funnyqt.generic/type-matcher`) and `ds` is a direction
  specification (see `funnyqt.generic/direction-matcher`).

  In fact, `ts` may also be a keyword naming a far-end role name, but this
  feature is only an implementation detail."
  ([v]
   (riseq-internal v identity))
  ([v ts]
   (riseq-internal v (type-matcher-from-ts-or-role v ts)))
  ([v ts ds]
   (riseq-internal v (every-pred (direction-matcher ds) (type-matcher-from-ts-or-role v ts)))))

(extend-protocol g/IElements
  Graph
  (elements
    ([this]
     (vseq this))
    ([this ts]
     (vseq this ts))))

(extend-protocol g/IRelationships
  Graph
  (relationships
    ([this]
     (eseq this))
    ([this ts]
     (eseq this ts))))

(extend-protocol g/IIncidentRelationships
  Vertex
  (incident-relationships
    ([v]
     (iseq v nil nil))
    ([v ts]
     (iseq v ts nil))
    ([v ts ds]
     (iseq v ts ds))))

(extend-protocol g/IRelationshipSourceTarget
  Edge
  (source [e] (alpha e))
  (target [e] (omega e)))

(extend-protocol g/IContainer
  Vertex
  (container [v]
    (loop [^Edge inc (first-inc v)]
      (when inc
        (if (= AggregationKind/COMPOSITE (.getThisAggregationKind inc))
          (that inc)
          (recur (next-inc inc)))))))

(def ^:private contents-transducer
  (comp (filter (fn [^Edge inc]
                  (= AggregationKind/COMPOSITE (.getThatAggregationKind inc))))
        (map that)))

(extend-protocol g/IContents
  Vertex
  (contents
    ([this]
     (sequence contents-transducer (iseq this)))
    ([this ts]
     (sequence
      (comp contents-transducer (filter (type-matcher-tg this ts)))
      (iseq this)))))

(extend-protocol g/IModelObject
  GraphElement
  (model-object? [this] true))

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
  "Returns the degree of vertex `v`, optionally restricted by `ts` and `ds`."
  ([^Vertex v]       (.getDegree v))
  ([^Vertex v ts]    (count (iseq v ts)))
  ([^Vertex v ts ds] (count (iseq v ts ds))))

;;# Modifications

;;## Creations

;; TODO: Basically, the impl should be determined by the schema.  Ask Volker!
(defn new-graph
  "Creates a graph with id `gid` of the given `schema` using implementation type `impl`.
  Supported impl types are :generic and :standard.  The graph id defaults to a
  creation timestamp, and the impl type to GENERIC."
  ([schema]
   (new-graph schema (format "Created: %s" (str (java.util.Date.)))))
  ([schema gid]
   (new-graph schema gid ImplementationType/GENERIC))
  ([^Schema schema ^String gid impl]
   (.createGraph schema (impl-type impl) gid
                 (Integer/valueOf 500)
                 (Integer/valueOf 500))))

(defn create-vertex!
  "Creates a new vertex of type `cls` in `g`.
  `cls` is a VertexClass or a qualified name given as symbol.  `prop-map` is an
  optional map from property names given as keyword to values to be set."
  ([^Graph g cls]
   (.createVertex g (if (vertex-class? cls)
                      cls
                      (attributed-element-class g cls))))
  ([^Graph g cls prop-map]
   (let [v (create-vertex! g cls)]
     (doseq [[prop val] prop-map]
       (if (.getAttribute (attributed-element-class v) (name prop))
         (set-value! v prop val)
         (g/set-adjs! v prop (if (or (nil? val) (coll? val))
                               val
                               [val]))))
     v)))

(extend-protocol g/ICreateElement
  Graph
  (create-element!
    ([g cls]
     (create-vertex! g cls))
    ([g cls prop-map]
     (create-vertex! g cls prop-map))))

(defn create-edge!
  "Creates a new edge of type `cls` in `g` starting at `from` and ending at `to`.
  `cls` is an EdgeClass or a qualified name given as symbol.  `attr-map`
  is an optional map from attribute name given as keyword to value to be
  set for that attribute."
  ([^Graph g cls ^Vertex from ^Vertex to]
   (.createEdge g (if (edge-class? cls)
                    cls
                    (attributed-element-class g cls))
                from to))
  ([^Graph g cls ^Vertex from ^Vertex to attr-map]
   (let [e (create-edge! g cls from to)]
     (doseq [[attr val] attr-map]
       (set-value! e attr val))
     e)))

(extend-protocol g/ICreateRelationship
  Graph
  (create-relationship!
    ([this cls from to]
     (create-edge! this cls from to))
    ([this cls from to attr-map]
     (create-edge! this cls from to attr-map))))

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
   (unlink! v identity nil))
  ([^Vertex v ts]
   (unlink! v ts nil))
  ([^Vertex v ts ds]
   (let [pred (every-pred (direction-matcher ds) (g/type-matcher v ts))]
     (while (when-let [e (first-inc v pred)]
              (g/delete! e))))))

(extend-protocol g/IModifyAdjacencies
  Vertex
  (set-adjs! [v role vs]
    (let [^DirectedSchemaEdgeClass
          dec (.getDirectedEdgeClassForFarEndRole
               ^VertexClass (attributed-element-class v)
               (name role))
          _  (when-not dec (u/errorf "No %s role at vertex %s." role v))
          ec (.getEdgeClass dec)
          ed (.getDirection dec)]
      (unlink! v #(g/is-instance? % ec) ed))
    (doseq [av vs]
      (.addAdjacence v (name role) av)))
  (set-adj! [v1 role v2]
    (g/set-adjs! v1 role [v2]))
  (add-adjs! [v role vs]
    (if (g/mm-multi-valued-property? (attributed-element-class v) role)
      (doseq [av vs]
        (.addAdjacence v (name role) av))
      (u/errorf "Can't add to the single-value role %s of %s."
                (name role) v)))
  (add-adj! [v1 role v2]
    (if (g/mm-multi-valued-property? (attributed-element-class v1) role)
      (.addAdjacence v1 (name role) v2)
      (u/errorf "Can't add to the single-value role %s of %s."
                (name role) v1))))

;;## Deletions

(extend-protocol g/IDelete
  Vertex
  (delete!
    ([v] (.delete v) v)
    ([v recursive]
     ;; Not recursive, so delete all incidences first.
     (when-not recursive
       (unlink! v))
     (g/delete! v)))
  Edge
  (delete!
    ([e]   (.delete e) e)
    ([e _] (.delete e) e)))

;;## Relinking edges

(defn relink!
  "Relinks all incidences of vertex `from` to vertex `to` and returns `from`.
  The incidences can be restricted by type spec `ts` and `ds` (see
  `funnyqt.generic/type-matcher` and `funnyqt.generic/direction-matcher`)."
  ([from to]
   (relink! from to identity identity))
  ([from to ts]
   (relink! from to ts identity))
  ([from to ts ds]
   (let [pred (every-pred (direction-matcher ds) (g/type-matcher from ts))]
     (loop [inc (first-inc from pred)]
       (when inc
         (set-this! inc to)
         (recur (first-inc from pred)))))
   from))

;;# Adjancencies

(extend-protocol i/IAdjacenciesInternal
  Vertex
  (adjs-internal [this role allow-unknown-role single-valued]
    (let [role-name (name role)]
      (if-let [^DirectedSchemaEdgeClass dec
               (.getDirectedEdgeClassForFarEndRole
                ^VertexClass (attributed-element-class this)
                role-name)]
        (if single-valued
          (let [^EdgeClass ec (.getEdgeClass dec)
                dir (.getDirection dec)
                ub (if (= dir EdgeDirection/OUT)
                     (-> ec .getTo .getMax)
                     (-> ec .getFrom .getMax))]
            (if (== ub 1)
              (map that (iseq this role))
              (u/errorf "Must not call adj on role '%s' (EdgeClass %s) with upper bound %s."
                        role ec ub)))
          (map that (iseq this role)))
        (when-not allow-unknown-role
          (u/errorf "No %s role at vertex %s" role this))))))

;;# Neighbors

(extend-protocol g/INeighbors
  Vertex
  (neighbors [v]
    (map that (iseq v))))

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
  (combine-fn (tc1 el) (tc2 el)).
  (Don't use this direcly, but use `on-subgraph-intersection` and
  `on-subgraph-union'.)"
  [^TraversalContext tc1 ^TraversalContext tc2 combine-fn]
  (cond
    (nil? tc1) tc2
    (nil? tc2) tc1
    :else (reify TraversalContext
            (containsVertex [_ v]
              (combine-fn (.containsVertex tc1 v)
                          (.containsVertex tc2 v)))
            (containsEdge [_ e]
              (combine-fn (.containsEdge tc1 e)
                          (.containsEdge tc2 e))))))

(defmacro on-subgraph-intersection
  "Sets the TraversalContext of `g` to a new TraversalContext that accepts only
  elements which both `tc` and `g`s current TraversalContext accept and then
  executes `body`.  Guaranteed to restore the old TraversalContext.

  Also see `vsubgraph`, `esubgraph`, `on-subgraph`, and
  `on-subgraph-union'."
  [[g tc] & body]
  `(let [^Graph g# ~g
         ^TraversalContext old-tc# (.getTraversalContext g#)]
     (try
       (.setTraversalContext
        g# (merge-traversal-contexts old-tc# ~tc q/and*))
       ~@body
       (finally (.setTraversalContext g# old-tc#)))))

(defmacro on-subgraph-union
  "Sets the TraversalContext of `g` to a new TraversalContext that
  accepts only elements which `tc` or `g`s current TraversalContext
  accept and then executes `body`.  Guaranteed to restore the old
  TraversalContext.

  Also see `vsubgraph`, `esubgraph`, `on-subgraph`, and
  `on-subgraph-intersection'."
  [[g tc] & body]
  `(let [^Graph g# ~g
         ^TraversalContext old-tc# (.getTraversalContext g#)]
     (try
       (.setTraversalContext
        g# (merge-traversal-contexts old-tc# ~tc q/or*))
       ~@body
       (finally (.setTraversalContext g# old-tc#)))))

(defn ^:private vsubgraph-tc
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
     (fn? pred)          (vsubgraph-tc g pred precalc)
     (u/type-spec? pred) (vsubgraph-tc g (g/type-matcher g pred) precalc)
     (coll? pred)        (vsubgraph-tc g #(q/member? % pred) precalc)
     :default            (u/error (str "Don't know how to handle predicate " pred)))))

(defn ^:private esubgraph-tc
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
     (fn? pred)          (esubgraph-tc g pred precalc)
     (u/type-spec? pred) (esubgraph-tc g (g/type-matcher g pred) precalc)
     (coll? pred)        (esubgraph-tc g #(q/member? % pred) precalc)
     :default            (u/error (str "Don't know how to handle predicate " pred)))))

;;# Generic Regular Path Expressions

(defn ^:private ---
  "For internal use only.
  Returns the vertices reachable from `v` via incidences with direction `ds`
  and aggregation kinds, restricted by `ts`, and `pred` (on the edges)."
  [v ds this-aks that-aks ts pred]
  (let [vs (u/oset v)]
    (if (seq vs)
      (let [complete-pred (every-pred
                           (direction-matcher ds)
                           (g/type-matcher (first vs) ts)
                           (or pred identity)
                           (if (seq this-aks)
                             #(q/member? (.getThisAggregationKind ^Edge %) this-aks)
                             identity)
                           (if (seq that-aks)
                             #(q/member? (.getThatAggregationKind ^Edge %) that-aks)
                             identity))]
        (into (os/ordered-set)
              (r/mapcat (fn [sv]
                          (r/map that (iseq-internal sv complete-pred)))
                        vs)))
      (os/ordered-set))))

(extend-protocol q/ISimpleRegularPathExpression
  Vertex
  (-->
    ([n]         (--- n :out nil nil nil nil))
    ([n ts]      (--- n :out nil nil ts  nil))
    ([n ts pred] (--- n :out nil nil ts  pred)))
  (--->
    ([n]         (--- n :out
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] nil nil))
    ([n ts]      (--- n :out
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] ts  nil))
    ([n ts pred] (--- n :out
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] ts  pred)))
  (<--
    ([n]         (--- n :in nil nil nil nil))
    ([n ts]      (--- n :in nil nil ts  nil))
    ([n ts pred] (--- n :in nil nil ts  pred)))
  (<---
    ([n]         (--- n :in
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] nil nil))
    ([n ts]      (--- n :in
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] ts  nil))
    ([n ts pred] (--- n :in
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] ts  pred)))
  (<->
    ([n]         (--- n :inout nil nil nil nil))
    ([n ts]      (--- n :inout nil nil ts  nil))
    ([n ts pred] (--- n :inout nil nil ts  pred)))
  (<-->
    ([n]         (--- n :inout
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] nil nil))
    ([n ts]      (--- n :inout
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED]
                      ts  nil))
    ([n ts pred] (--- n :inout
                      [AggregationKind/NONE AggregationKind/SHARED]
                      [AggregationKind/NONE AggregationKind/SHARED] ts  pred)))
  (<>--
    ([n]         (--- n :inout nil [AggregationKind/COMPOSITE] nil nil))
    ([n ts]      (--- n :inout nil [AggregationKind/COMPOSITE] ts  nil))
    ([n ts pred] (--- n :inout nil [AggregationKind/COMPOSITE] ts  pred)))
  (--<>
    ([n]         (--- n :inout [AggregationKind/COMPOSITE] nil nil nil))
    ([n ts]      (--- n :inout [AggregationKind/COMPOSITE] nil ts  nil))
    ([n ts pred] (--- n :inout [AggregationKind/COMPOSITE] nil ts  pred))))

;;# Describe Schema and Graph Elements

(defn ^:private attr-desc
  "Returns a map of aec's own attributes as name-domain pairs."
  [^AttributedElementClass aec]
  (into (sorted-map)
        (for [^Attribute attr (.getOwnAttributeList aec)]
          [(keyword (.getName attr)) (g/describe (.getDomain attr))])))

(defn ^:private slot-desc
  [^AttributedElement e]
  (let [aec (.getAttributedElementClass e)]
    (into (sorted-map)
          (for [^Attribute attr (.getAttributeList aec)]
            (let [n (.getName attr)]
              [(keyword n) (value e n)])))))

(defn ^:private super-classes
  [^GraphElementClass gec]
  (set (map #(symbol (.getQualifiedName ^GraphElementClass %))
            (.getDirectSuperClasses gec))))

(defn ^:private sub-classes
  [^GraphElementClass gec]
  (set (map #(symbol (.getQualifiedName ^GraphElementClass %))
            (.getDirectSubClasses gec))))

(extend-protocol g/IDescribe
  Graph
  (describe [this]
    {:type 'Graph
     :qname (symbol (g/qname this))
     :slots (slot-desc this)})
  Vertex
  (describe [this]
    {:type 'Vertex
     :qname (symbol (g/qname this))
     :slots (slot-desc this)})
  Edge
  (describe [this]
    {:type 'Edge
     :qname (symbol (g/qname this))
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
                         [(keyword (.getName c)) (g/describe (.getDomain c))]))})
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
          (str "#<v" (id v) ": " (g/qname v) ">")))

(defmethod print-method Edge
  [e out]
  (.write ^java.io.Writer out
          (str "#<" (if (normal-edge? e) "+" "-") "e"
               (Math/abs ^int (id e)) ": " (g/qname e) ">")))

(defmethod print-method Graph
  [^Graph g out]
  (.write ^java.io.Writer out
          (str "#<Graph " (id g)
               " (" (.getGraphVersion g)
               "): " (g/qname g) ">")))

(defmethod print-method VertexClass
  [vc out]
  (.write ^java.io.Writer out
          (str "#<VertexClass " (g/qname vc) ">")))

(defmethod print-method EdgeClass
  [ec out]
  (.write ^java.io.Writer out
          (str "#<EdgeClass " (g/qname ec) ">")))

(defmethod print-method GraphClass
  [gc out]
  (.write ^java.io.Writer out
          (str "#<GraphClass " (g/qname gc) ">")))

;;# Schema-specific functional API

(defn ^:private create-vc-fns [^VertexClass vc prefix]
  `(do
     ;; CREATE FN
     ~(when-not (g/mm-abstract? vc)
        `(defn ~(symbol (str prefix "create-" (g/escaped-uname-str vc) "!"))
           ~(format "Create a new %s vertex in graph `g`.
  An additional `prop-map` may be supplied.
  Shorthand for (create-vertex! g '%s props)."
                    (.getQualifiedName vc)
                    (.getQualifiedName vc))
           ([~'g]
            (create-vertex! ~'g '~(g/qname vc)))
           ([~'g ~'prop-map]
            (create-vertex! ~'g '~(g/qname vc) ~'prop-map))))

     ;; VSEQ FN
     (defn ~(symbol (str prefix "vseq-" (g/escaped-uname-str vc)))
       ~(format "Returns the lazy sequence of %s vertices in `g`."
                (g/qname vc))
       [~'g]
       (vseq ~'g '~(g/qname vc)))

     ;; TYPE PRED
     (defn ~(symbol (str prefix "isa-" (g/escaped-uname-str vc) "?"))
       ~(format "Returns true if `v` is a %s-vertex."
                (g/qname vc))
       [~'v]
       (g/has-type? ~'v '~(g/qname vc)))))

(defn ^:private create-ec-fns [^EdgeClass ec prefix]
  `(do
     ~(when-not (g/mm-abstract? ec)
        ;; CREATE FN
        `(defn ~(symbol (str prefix "create-" (g/escaped-uname-str ec) "!"))
           ~(format "Create a new %s edge from `alpha` to `omega` in graph `g`.
  An additional `attr-map` may be supplied.
  Shorthand for (create-edge! g '%s alpha omega attr-map).

  [%s] --%s--> [%s]"
                    (g/qname ec)
                    (g/qname ec)
                    (g/qname (.getVertexClass (.getFrom ec)))
                    (g/qname ec)
                    (g/qname (.getVertexClass (.getTo ec))))
           ([~'g ~'alpha ~'omega]
            (create-edge! ~'g '~(g/qname ec) ~'alpha ~'omega))
           ([~'g ~'alpha ~'omega ~'attr-map]
            (create-edge! ~'g '~(g/qname ec) ~'alpha ~'omega ~'attr-map))))

     ;; ESEQ FN
     (defn ~(symbol (str prefix "eseq-" (g/escaped-uname-str ec)))
       ~(format "Returns the lazy sequence of %s edges in `g`.
  `g` may be a graph or an edge.  In the latter case, returns all edges
  following `g` in the edge sequence.

  [%s] --%s--> [%s]"
                (g/qname ec)
                (g/qname (.getVertexClass (.getFrom ec)))
                (g/qname ec)
                (g/qname (.getVertexClass (.getTo ec))))
       [~'g]
       (eseq ~'g '~(g/qname ec)))

     ;; ISEQ FN
     (defn ~(symbol (str prefix "iseq-" (g/escaped-uname-str ec)))
       ~(format "Returns the lazy sequence of `v`s %s incidences restricted by `ds`.
  `v` may be a vertex or an edge.  In the latter case, returns all incidences
  following `v` in the current vertex's incidence sequence.
  `ds` may be :in, :out, or :inout (the default).

  [%s] --%s--> [%s]"
                (g/qname ec)
                (g/qname (.getVertexClass (.getFrom ec)))
                (g/qname ec)
                (g/qname (.getVertexClass (.getTo ec))))
       ([~'v]
        (iseq ~'v '~(g/qname ec) :inout))
       ([~'v ~'ds]
        (iseq ~'v '~(g/qname ec) ~'ds)))

     ;; TYPE PRED
     (defn ~(symbol (str prefix "isa-" (g/escaped-uname-str ec) "?"))
       ~(format "Returns true if `e` is a %s-edge."
                (g/qname ec))
       [~'e]
       (g/has-type? ~'e '~(g/qname ec)))))

(defn ^:private create-attr-fns [attr owners prefix]
  (let [bool? (group-by (fn [^AttributedElementClass aec]
                          (-> (.getAttribute aec (name attr))
                              .getDomain
                              .isBoolean))
                        owners)]
    `(do
       ~@(when (bool? true)
           `[(defn ~(symbol (str prefix (name attr) "?"))
               ~(format "Checks if `ae` is %s.
  Possible types of `ae`: %s"
                        (name attr)
                        (str/join ", " (apply sorted-set (map g/qname (bool? true)))))
               [~'ae]
               (value ~'ae ~attr))])
       ~@(when (bool? false)
           `[(defn ~(symbol (str prefix (name attr)))
               ~(format "Returns the value of `ae`s %s attribute.
  Possible types of `ae`: %s"
                        (name attr)
                        (str/join ", " (apply sorted-set (map g/qname (bool? false)))))
               [~'ae]
               (value ~'ae ~attr))])
       (defn ~(symbol (str prefix "set-" (name attr) "!"))
         ~(format "Sets the value of `ae`s %s attribute to `val`.
  Possible types of `ae`: %s"
                  (name attr)
                  (str/join ", " (apply sorted-set (map g/qname owners))))
         [~'ae ~'val]
         (set-value! ~'ae ~attr ~'val)))))

(defn ^:private create-role-fns [role owners prefix]
  (let [v (with-meta 'v {:tag `Vertex})
        multi? (group-by (fn [^VertexClass vc]
                           (g/mm-multi-valued-property? vc role))
                         owners)
        owner-string (str/join ", " (apply sorted-set (map g/qname owners)))]
    `(do
       ;; GETTER
       ~(cond
          ;; This role is always multi-valued
          (and (multi? true) (not (multi? false)))
          `(defn ~(symbol (str prefix "->" (name role)))
             ~(format "Returns the vertices in `v`s %s role.
  Possible types of `v`: %s"
                      (name role)
                      owner-string)
             [~v]
             (.adjacences ~v ~(name role)))

          ;; This role is always single-valued
          (and (multi? false) (not (multi? true)))
          `(defn ~(symbol (str prefix "->" (name role)))
             ~(format "Returns the vertex in `v`s %s role.
  Possible types of `v`: %s"
                      (name role)
                      owner-string)
             [~v]
             (let [x# (.adjacences ~v ~(name role))]
               (if (next x#)
                 (u/errorf "Multiple vertices found in the single-valued role %s of %s."
                           ~(name role) ~v)
                 (first x#))))

          :else `(defn ~(symbol (str prefix "->" (name role)))
                   ~(format "Returns the vertex/vertices in `v`s %s role.
  Possible types of `v`: %s"
                            (name role)
                            owner-string)
                   [~v]
                   (if (g/mm-multi-valued-property? (attributed-element-class ~v) ~role)
                     (.adjacences ~v ~(name role))
                     (let [x# (.adjacences ~v ~(name role))]
                       (if (next x#)
                         (u/errorf "Multiple vertices found in the single-valued role %s of %s."
                                   ~(name role) ~v)
                         (first x#))))))

       ;; SETTER
       ~(cond
          ;; This role is always multi-valued
          (and (multi? true) (not (multi? false)))
          `(defn ~(symbol (str prefix "->set-" (name role) "!"))
             ~(format "Sets the %s role of `v` to `ovs`.
  `ovs` must be a collection of vertices.
  Possible types of `v`: %s"
                      (name role)
                      owner-string)
             [~v ~'ovs]
             (g/set-adjs! ~v ~role ~'ovs))

          ;; This role is always single-valued
          (and (multi? false) (not (multi? true)))
          `(defn ~(symbol (str prefix "->set-" (name role) "!"))
             ~(format "Sets the %s role of `v` to `ov`.
  `ov` must be a single vertex.
  Possible types of `v`: %s"
                      (name role)
                      owner-string)
             [~v ~'ov]
             (g/set-adj! ~v ~role ~'ov))

          :else `(defn ~(symbol (str prefix "->set-" (name role) "!"))
                   ~(format "Sets the %s role of `v` to `ov`.
  If `ov` must be a single vertex or a collection of vertices depends on the
  type of `v`.
  Possible types of `v`: %s"
                            (name role)
                            owner-string)
                   [~v ~'ov]
                   (if (g/mm-multi-valued-property? (attributed-element-class ~v) ~role)
                     (g/set-adjs! ~v ~role ~'ov)
                     (g/set-adj! ~v ~role ~'ov))))

       ;; ADDER
       ~@(when (multi? true)
           `[(defn ~(symbol (str prefix "->add-" (name role) "!"))
               ~(format "Adds `ov` and `more` vertices to `v`s %s role.
  Possible types of `v`: %s"
                        (name role)
                        owner-string)
               [~v ~'ov ~'& ~'more]
               (g/add-adj! ~v ~role ~'ov)
               (g/add-adjs! ~v ~role ~'more))
             (defn ~(symbol (str prefix "->addall-" (name role) "!"))
               ~(format "Adds all `vs` to `v`s %s role.
  Possible types of `v`: %s"
                        (name role)
                        owner-string)
               [~v ~'vs]
               (g/add-adjs! ~v ~role ~'vs))]))))

(defmacro generate-schema-functions
  "Generates a schema-specific API consisting of functions for creating
  vertices and edges and functions for accessing properties (attributes and
  roles).

  `schema-file` is the TG file containing the schema to generate an API for.

  `nssym` denotes the new namespace in which to generate the API.  It may be
  nil in which case the current namespace is used.

  `alias` is an alias under which the new generated namespace is required.

  `prefix` is an optional prefix added to all functions of the generated
  API.  (That's mostly useful when generating in the current namespace.)

  The API consists of the following functions.

  For any VertexClass Foo, there is a (create-Foo! graph) function, a (vseq-Foo
  graph) sequence function, and a (isa-Foo? v) type check predicate.

  For any EdgeClass HasFoo, there is a (create-HasFoo! graph alpha
  omega) function, (eseq-HasFoo graph) and (iseq-HasFoo v ds) sequence
  functions, and a (isa-HasFoo? e) type check predicate.

  For any attribute attr, there are the following functions:

    (attr el) ;; returns the attr value of el
    (set-attr! el val) ;; sets the attr value of el to val

  For boolean attributes attr, the getter is named (attr? el).

  For any role name role, there are the functions:

    (->role el) ;; returns the vertex/vertices in el's role
    (->set-role! el v) ;; sets the role
    (->add-role! el v1 v2 v3...) ;; adds v1, v2, and v3 to el's role
    (->addall-role! el vs) ;; adds all vertices in vs to el's role

  The add-* functions are only generated if role occurs as a multi-valued role
  for some VertexClass.  If role is a multi-valued role of el, then the setter
  must be given a collection of vertices, else a single vertex."
  ([schema-file]
     `(generate-schema-functions ~schema-file nil nil nil))
  ([schema-file nssym]
     `(generate-schema-functions ~schema-file ~nssym nil nil))
  ([schema-file nssym alias]
     `(generate-schema-functions ~schema-file ~nssym ~alias nil))
  ([schema-file nssym alias prefix]
     `(g/metamodel-api-generator ~schema-file
                                 ~nssym
                                 ~alias
                                 ~prefix
                                 create-vc-fns
                                 create-ec-fns
                                 create-attr-fns
                                 create-role-fns)))
