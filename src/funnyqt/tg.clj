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
functions `record` and `enum`.

Visualization
=============

See `tgtree`, `show-graph`, and `print-graph`."
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.utils)
  (:import
   (java.awt.event KeyEvent KeyListener)
   (java.util Collection)
   (java.lang.reflect Method)
   (javax.swing JFrame JScrollPane JLabel ImageIcon JOptionPane WindowConstants)
   (de.uni_koblenz.jgralab AttributedElement Graph GraphElement Vertex Edge
			   EdgeDirection GraphIO Record
                           ImplementationType)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain EnumDomain
                                  ListDomain SetDomain MapDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.utilities.tg2dot Tg2Dot)
   (de.uni_koblenz.jgralab.utilities.tg2dot.dot GraphVizProgram GraphVizOutputFormat)
   (de.uni_koblenz.jgralab.impl ConsoleProgressFunction)
   (org.pcollections ArrayPMap ArrayPSet ArrayPVector)))


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

(defn show-graph
  "Show graph `g` in a JFrame, possibly with `reversed` edges (default false).
  Does nothing except printing a warning message for too large graphs."
  ([^Graph g]
     (show-graph g false))
  ([^Graph g reversed]
     (if (> (+ (.getVCount g) (.getECount g)) 600)
       (do (println "Graph too big for visualizing!") nil)
       (let [img (.convertToGraphVizImageIcon
                  (doto ^Tg2Dot (Tg2Dot.)
                        (.setGraph ^Graph g)
                        (.setReversedEdges reversed)
                        (.setPrintEdgeAttributes true))
              (.outputFormat (GraphVizProgram.) GraphVizOutputFormat/PNG))
             label (JLabel. img)
             frame (JFrame. (str "Graph: " (.getId g)))
             scale (atom 1.0)]
         (doto frame
           (-> (.getContentPane)
               (.add (JScrollPane. label)))
           (.setDefaultCloseOperation WindowConstants/DISPOSE_ON_CLOSE)
           (.addKeyListener
            (proxy [KeyListener] []
              (keyPressed [^KeyEvent e]
                (try
                  (let [kc (.getKeyCode e)
                        sf (cond
                            (== kc KeyEvent/VK_PLUS)   0.1
                            (== kc KeyEvent/VK_MINUS) -0.1
                            :default 0)
                        i  (.getImage img)
                        io (.getImageObserver img)]
                    (when-not (zero? sf)
                      (swap! scale + sf)
                      (.setIcon label (ImageIcon. (.getScaledInstance
                                                   i
                                                   (int (* @scale (.getWidth i io)))
                                                   (int (* @scale (.getHeight i io)))
                                                   (int java.awt.Image/SCALE_SMOOTH))))
                      (.repaint frame)))
                  (catch Throwable ex
                    (JOptionPane/showMessageDialog frame (.getMessage ex)))))
              (keyReleased [e])
              (keyTyped [e])))
           (.show)
           (.pack))))))

(defn tgtree
  "Shows a simple Swing tree view representation of the graph `g`."
  [g]
  (.setVisible (de.uni_koblenz.jgralab.utilities.tgtree.TGTree. g) true))

(declare attributed-element-class)
(defn print-graph
  "Generates a visualization of `g` and saves it as `file`.
  The file type is determined by its extension (dot, xdot, ps, svg, svgz, png,
  gif, pdf) and defaults to PDF.  If `reversed` it true, the edges will point
  upwards.  `reversed-ecs` may be a seq of type names (as symbols) for which
  edges should be vizualized with the opposite of `reversed`s value."
  [^Graph g ^String file reversed & reversed-ecs]
  (let [suffix (second (re-matches #".*\.([^.]+)$" file))
        ^GraphVizOutputFormat of (cond
                                  (= suffix "dot")  GraphVizOutputFormat/DOT
                                  (= suffix "xdot") GraphVizOutputFormat/XDOT
                                  (= suffix "ps")   GraphVizOutputFormat/POSTSCRIPT
                                  (= suffix "svg")  GraphVizOutputFormat/SVG
                                  (= suffix "svgz") GraphVizOutputFormat/SVG_ZIPPED
                                  (= suffix "png")  GraphVizOutputFormat/PNG
                                  (= suffix "gif")  GraphVizOutputFormat/GIF
                                  :else             GraphVizOutputFormat/PDF)]
    (de.uni_koblenz.jgralab.utilities.tg2dot.Tg2Dot/convertGraph
     g file reversed of)))

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
  Supported impl types are :generic, :standard, :transaction, and :database."
  ([file]
     (load-graph file ImplementationType/GENERIC))
  ([file impl]
     (with-open [is (clojure.java.io/input-stream file)]
       (GraphIO/loadGraphFromStream
        (if (and (string? file)
                 (.endsWith ^String file ".gz"))
          (java.util.zip.GZIPInputStream. is)
          is)
        nil nil
        ^ImplementationType (impl-type impl)
        (ConsoleProgressFunction. "Loading")))))

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

;; TODO: We need to think about this.  Unlimited caching definitively isn't the
;; right thing, and it'll become wrong when metatransforms rename/delete/create
;; classes (can be disabled, see *allow-class-access-by-simple-name* below).
;; Or maybe access by simple name should be moved into jgralab itself...
(def ^:private aec-simple-name-map
  (memoize
   (fn [^Schema s]
     (let [m (atom {})
           gc (.getGraphClass s)]
       (doseq [^GraphElementClass
               gec (concat (.getVertexClasses gc)
                           (.getEdgeClasses gc))]
         (swap! m update-in [(.getSimpleName gec)] conj gec))
       (apply hash-map (mapcat (fn [[k v]]
                                 (if (> (count v) 1)
                                   []
                                   [k (first v)]))
                               @m))))))

(def ^:dynamic *allow-class-access-by-simple-name*
  "If locical true, graph element classes may be retrieved by their simple
  names if they are unique.  That is, if there's only a vertex class `foo.Bar`,
  then `(vseq g 'Bar)` will work just fine.  If there are classes `foo.Bar` and
  `baz.Bar` you'll get an error that the name is not unique, and you need to
  use the qualified name anyway."
  true)

(defn attributed-element-class
  "Returns `ae`s AttributedElementClass or the AttributedElementClass with the
  given `qname` in the schema of `ae`.  In the arity 2 version, `ae` may be an
  attributed element, an attributed element class, or a schema."
  ([^AttributedElement ae]
     (.getAttributedElementClass ae))
  ([elem qname]
     (condp instance? elem
       AttributedElement
       (let [^AttributedElement ae elem]
         (or (-> ae .getSchema (.getAttributedElementClass (name qname)))
             (and *allow-class-access-by-simple-name*
                  ((aec-simple-name-map (.getSchema ae)) (name qname)))
             (errorf "No such attributed element class %s" (name qname))))
       AttributedElementClass
       (let [^AttributedElementClass aec elem]
         (or (-> aec .getSchema (.getAttributedElementClass (name qname)))
             (and *allow-class-access-by-simple-name*
                  ((aec-simple-name-map (.getSchema aec)) (name qname)))
             (errorf "No such attributed element class %s" (name qname))))
       Schema
       (let [^Schema s elem]
         (or (.getAttributedElementClass s (name qname))
             (and *allow-class-access-by-simple-name*
                  ((aec-simple-name-map s) (name qname)))
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

(defn ^:private type-matcher-tg-1
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

(defn ^:private type-matcher-tg
  [g ts]
  (cond
   (nil? ts)   identity
   (fn? ts)    ts
   (qname? ts) (type-matcher-tg-1 g ts)
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
                       t-matchers (map #(type-matcher-tg g %) r)]
                   (apply op t-matchers))
                 ;; Empty collection given: (), [], that's also ok
                 identity)
   :else (errorf "Don't know how to create a TG type-matcher for %s" ts)))

(extend-protocol TypeMatcher
  GraphElement
  (type-matcher [ge ts]
    (type-matcher-tg (graph ge) ts))
  Graph
  (type-matcher [g ts]
    (type-matcher-tg g ts)))

(defmacro ^:private has-type?-1 [obj spec dc]
  ;; For simple type specs, we don't need to create a type-matcher.
  `(if (qname? ~spec)
     (let [[neg# cls# ex#] (type-with-modifiers (name ~spec))
           aec# (attributed-element-class ~obj cls#)
           r# (if ex#
                (identical? (attributed-element-class ~obj)
                            aec#)
                (and (instance? ~dc aec#)
                     (.isInstanceOf ~obj aec#)))]
       (if neg# (not r#) r#))
     ((type-matcher ~obj ~spec) ~obj)))

(extend-protocol InstanceOf
  Graph
  (is-instance? [object class]
    (and (instance? GraphClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    (has-type?-1 obj spec GraphClass))
  Vertex
  (is-instance? [object class]
    (and (instance? VertexClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    (has-type?-1 obj spec VertexClass))
  Edge
  (is-instance? [object class]
    (and (instance? EdgeClass class)
         (.isInstanceOf object class)))
  (has-type? [obj spec]
    (has-type?-1 obj spec EdgeClass)))


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

;;## Vertex, edge counts

(defn vcount
  "Returns the vertex count of `g` restricted by `ts`."
  ([^Graph g]    (.getVCount g))
  ([^Graph g ts] (count (vseq g ts))))

(defn ecount
  "Returns the edge count of `g` restricted by `ts`."
  ([^Graph g]    (.getECount g))
  ([^Graph g ts] (count (eseq g ts))))

;;# Modifications

;;## Creations

;; TODO: Basically, the impl should be determined by the schema.  Ask Volker!
(defn create-graph
  "Creates a graph with id `gid` of the given `schema` using implementation type `impl`.
  Supported impl types are :generic, :standard, :transaction, and :database.
  The graph id defaults to a creation timestamp, and the impl type to GENERIC."
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

(defn add-adj!
  "Creates an edge matching `role` between `v` and every vertex in `adjs`."
  [^Vertex v role & adjs]
  (doseq [a adjs]
    (.addAdjacence v (name role) a)))

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

(defn set-adjs!
  "Sets the `role` adjacency list of `v` to `adjvs`.
  This means, first all incident edges of that role are deleted, and then new
  edges are created."
  [^Vertex v role adjvs]
  (let [^de.uni_koblenz.jgralab.schema.impl.DirectedSchemaEdgeClass
        dec (.getDirectedEdgeClassForFarEndRole
             ^VertexClass (attributed-element-class v)
             (name role))
        ec (.getEdgeClass dec)
        ed (.getDirection dec)]
    (unlink! v #(is-instance? % ec) ed))
  (doseq [av adjvs]
    (add-adj! v role av)))


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

;;# toString/Serialization/Deserialization

;;## Normal toString()

(defmethod print-method Vertex
  [v out]
  (.write ^java.io.Writer out
          (str "#<v" (id v) ": " (qname v) ">")))

(defmethod print-method Edge
  [e out]
  (.write ^java.io.Writer out
          (str "#<e" (id e) ": " (qname e) ">")))

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

;;## Printing `read`-able

;; TODO: That should be converted to using reader literals if possible...
(def ^{:dynamic true :private true}
  *serialization-bindings* nil)

(let [sb-qname     (subs (str (var *serialization-bindings*)) 2)
      vertex-qname (subs (str (var vertex)) 2)
      edge-qname   (subs (str (var edge)) 2)]
  (defmethod print-dup Vertex [v out]
    (.write ^java.io.Writer out
            (format "#=(%s #=(%s \"%s\") %s)"
                    vertex-qname sb-qname (id (graph v)) (id v))))

  (defmethod print-dup Edge [e out]
    (.write ^java.io.Writer out
            (format "#=(%s #=(%s \"%s\") %s)"
                    edge-qname sb-qname (id (graph e)) (id e))))

  (defmethod print-dup Graph [g out]
    (.write ^java.io.Writer out
            (format "#=(%s \"%s\")"
                    sb-qname (id g)))))

(defn tg-pr-str
  "Prints arbitrary clojure data structures including vertices and edges to a
  string that can be read back using `tg-read-str`."
  [obj]
  (binding [*print-dup* true]
    (pr-str obj)))

(defn tg-read-str
  "Reads `str` and returns the object represented by `str`.
  If the object contains vertices or edges, the graphs holding them have to be
  provided as `gs`."
  [str & gs]
  (binding [*serialization-bindings* (into {} (map (fn [g] [(id g) g])
                                                   gs))]
    (read-string str)))

(defn tg-spit
  "Like `clojure.core/spit' but ensure that vertices and edges are printed in a
  readable manner."
  [f obj]
  (spit f (tg-pr-str obj)))

(defn tg-slurp
  "Reads an object from the file `f`, like `clojure.core/slurp'.
  If that object contains vertices and edges, then their hosting graphs have to
  be provided as gs."  [f & gs]
  (apply tg-read-str (slurp f) gs))
