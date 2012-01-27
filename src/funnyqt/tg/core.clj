(ns
  funnyqt.tg.core
  "Core functions for accessing and manipulating TGraphs."
  (:use funnyqt.generic)
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
   (de.uni_koblenz.jgralab.schema.impl.compilation  SchemaClassManager)
   (de.uni_koblenz.jgralab.utilities.tg2dot Tg2Dot)
   (de.uni_koblenz.jgralab.utilities.tg2dot.dot GraphVizProgram GraphVizOutputFormat)
   (de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.impl ConsoleProgressFunction)
   (org.pcollections ArrayPMap ArrayPSet ArrayPVector)))

(add-long-doc!
 "Loading/Saving
==============

See `load-graph', `save-graph', and `load-schema'.

Graph Elements
==============

For accessing elements, see `vertex', `edge', `first-vertex', `next-vertex',
`first-edge', `next-edge', `first-inc', and `next-inc'.  For more convenient
access, check out `vseq', `eseq', and `iseq' in the funql namespace.

For creating graphs and elements, see `create-graph', `create-vertex!', and
`create-edge!'.

Attributes
==========

To access attribute values or record components, use the function `value'.  To
set them, use `set-value!'.  All clojure collections and maps are automatically
converted to JGraLab's pcollections.  In case of RecordDomains and EnumDomains,
whose values are instances of generated classes, there are the constructor
functions `record' and `enum'.

Visualization
=============

See `tgtree', `show-graph', and `dot-graph'.")

;;* Utility Functions and Macros


;;** Graph utilities

(defn show-graph
  "Show graph `g' in a JFrame, possibly with `reversed' edges (default false).
  Does nothing except printing a warning message for too large graphs."
  ([^Graph g]
     (show-graph g false))
  ([^Graph g reversed]
     (if (> (+ (.getVCount g) (.getECount g)) 600)
       (do (println "Graph too big for visualizing!") nil)
       (let [img (-> (doto ^Tg2Dot (Tg2Dot.)
                           (.setGraph ^Graph g)
                           (.setReversedEdges reversed))
                     (.convertToGraphVizImageIcon
                      (-> (GraphVizProgram.)
                          (.outputFormat GraphVizOutputFormat/PNG))))
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
                    (when-not (== sf 0)
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
  "Shows a simple Swing tree view representation of the graph `g'."
  [g]
  (-> (de.uni_koblenz.jgralab.utilities.tgtree.TGTree. g)
      (.setVisible true)))

(declare attributed-element-class)
(defn dot-graph
  "Generates a visualization of `g' and saves it as `file'.
  The file type is determined by its extension (dot, xdot, ps, svg, svgz, png,
  gif, pdf) and defaults to PDF.  If `reversed' it true, the edges will point
  upwards.  `reversed-ecs' may be a seq of type names (as symbols) for which
  edges should be vizualized with the opposite of `reversed's value."
  [^Graph g file reversed & reversed-ecs]
  (let [suffix (second (re-matches #".*\.([^.]+)$" file))]
    (de.uni_koblenz.jgralab.utilities.tg2dot.Tg2Dot/convertGraph
     g file reversed
     (cond
      (= suffix "dot")  GraphVizOutputFormat/DOT
      (= suffix "xdot") GraphVizOutputFormat/XDOT
      (= suffix "ps")   GraphVizOutputFormat/POSTSCRIPT
      (= suffix "svg")  GraphVizOutputFormat/SVG
      (= suffix "svgz") GraphVizOutputFormat/SVG_ZIPPED
      (= suffix "png")  GraphVizOutputFormat/PNG
      (= suffix "gif")  GraphVizOutputFormat/GIF
      :else             GraphVizOutputFormat/PDF)
     (into-array AttributedElementClass (map #(attributed-element-class g %)
                                             reversed-ecs)))))

(defn load-schema
  "Loads a schema from `file' and possibly compile it using
  CodeGeneratorConfiguration `cg-conf' (default MINIMAL)."
  ([file]
     (load-schema file CodeGeneratorConfiguration/MINIMAL))
  ([file cg-conf]
     (let [^Schema s (GraphIO/loadSchemaFromFile file)]
       (.finish s)
       ;; TODO: What if the schema has already been compiled for this impl
       ;; type?
       (when cg-conf
         (.compile s cg-conf))
       s)))

(defn load-graph
  "Loads a graph from `file' using ImplementationType `impl'.
  The schema will be compiled automagically if needed."
  ([file]
     (load-graph file ImplementationType/STANDARD))
  ([file impl]
     (let [cg-config (cond
                      (= impl ImplementationType/GENERIC)     false
                      (= impl ImplementationType/STANDARD)    CodeGeneratorConfiguration/MINIMAL
                      (= impl ImplementationType/TRANSACTION) CodeGeneratorConfiguration/WITH_TRANSACTION_SUPPORT
                      (= impl ImplementationType/DATABASE)    CodeGeneratorConfiguration/WITH_DATABASE_SUPPORT
                      :else CodeGeneratorConfiguration/MINIMAL)
           s (load-schema file cg-config)]
       (GraphIO/loadGraphFromFile
        file s (ConsoleProgressFunction. "Loading") impl))))

(defn save-graph
  "Saves `g' to `file'."
  [^Graph g ^String file]
  (GraphIO/saveGraphToFile g file (ConsoleProgressFunction. "Saving")))

(defmacro dograph!
  "Like `clojure.core/io!', but with a more appropriate name and message."
  [& body]
  `(io!
    "Graph modification in a transaction!"
    ~@body))


;;* Schema Access

(defprotocol QualifiedName
  "A protocol for qualified names."
  (qname [this]
    "Returns the qualified name of this named element's class or named element
    class as a symbol.  For collection domains, it returns a vector of symbols:
    [List Integer] where Integer is the base domain, or [Map Integer String]
    where Integer is the key domain and String is the value domain.  Of course,
    that may be recursive, so [Map Integer [List String]] corresponds to the
    java domain Map<Integer, List<String>>."))

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

(defprotocol Abstractness
  "A protocol for checking if a graph element class is abstract."
  (abstract? [this]
    "Returns true, iff the given graph element class is abstract."))

(extend-protocol Abstractness
  GraphElementClass
  (abstract? [this]
    (.isAbstract this)))

(defprotocol Internalness
  "A protocol for checking if an attributed element class is internal."
  (internal? [this]
    "Returns true, iff the given attributed element class is abstract."))

(extend-protocol Internalness
  AttributedElementClass
  (internal? [this]
    (.isInternal this)))

(defprotocol Resolving
  "A protocol for resolving schema classes and domains."
  (attributed-element-class [this] [this qname]
    "Returns this element's attributed element class, or the attributed element
  class with the given qname in the schema of this.  qname may be a symbol,
  keyword or string.")
  (domain [this qname]
    "Returns the domain of the domain qname in this element's schema.
  qname may be a symbol, keyword, string, or a vector like [Set Integer]
  corresponding to the domain Set<Integer>, or [Map Integer [List String]]
  corresponding to Map<Integer, List<String>>.")
  (schema [this]
    "Returns the schema of this element."))

(declare domain-qname)
(defn- domain-vector-qname
  [v]
  (when (seq v)
    (let [s (map domain-qname (rest v))
          l (last s)
          f (butlast s)]
      (apply str (first v) "<"
             (concat (interleave f (iterate (constantly ", ") ", "))
                     [l ">"])))))

(defn domain-qname
  "Transforms a domain qname given as symbol, keyword, string, or vector to a
  canonical string representation:"
  [qn]
  (if (coll? qn)
    (domain-vector-qname qn)
    (name qn)))

(extend-protocol Resolving
  AttributedElement
  (attributed-element-class
    ([this]
       (.getAttributedElementClass this))
    ([this qname]
       (or (-> this .getSchema (.getAttributedElementClass (name qname)))
           (error (format "No such attributed element class %" (name qname))))))
  (domain [elem qname]
    (or (-> (.getSchema elem)
            (.getDomain (domain-qname qname)))
        (error (format "No such domain %s" (domain-qname qname)))))
  (schema [ae]
    (.getSchema ae))

  Domain
  (schema [this]
    (.getSchema this))

  AttributedElementClass
  (attributed-element-class
    ([this]
       this)
    ([this qname]
       (-> this .getSchema (.getAttributedElementClass (name qname)))))
  (domain [aec qname]
    (or (-> (.getSchema aec)
            (.getDomain (domain-qname qname)))
        (error (format "No such domain %s" (domain-qname qname)))))
  (schema [this]
    (.getSchema this))

  Schema
  (attributed-element-class
    ([this]
       (error "A schema is no attributed element class!"))
    ([this qname]
       (.getAttributedElementClass this (name qname))))
  (domain [s qname]
    (or (.getDomain s (domain-qname qname))
        (error (format "No such domain %s" (domain-qname qname)))))
  (schema [this]
    this))

(defn instance-of?
  "Return true, iff `elem' is an instance of the attributed element class
  `attr-elem-class'."
  [^AttributedElement elem ^AttributedElementClass attr-elem-class]
  (.isInstanceOf elem attr-elem-class))

(defn- type-matcher-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [g c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        type  (attributed-element-class g qname)]
    (cond
     (and (not neg) (not exact)) (fn [x] (instance-of? x type))
     (and (not neg) exact)       (fn [x] (identical? type (attributed-element-class x)))
     (and neg       (not exact)) (fn [x] (not (instance-of? x type)))
     :default                    (fn [x] (not (identical? type (attributed-element-class x)))))))

(defn type-matcher
  "Returns a matcher for either nil, !Foo!, [Foo Bar! !Baz], [:and 'Foo 'Bar],
  or [:or 'Foo 'Bar].  In a collection spec, the first element may be one of
  the keywords :or (default), :nor, :and, :nand, or :xor with the usual logic
  semantics."
  [g ts]
  (cond
   (nil? ts)   identity
   (fn? ts)    ts
   (qname? ts) (type-matcher-1 g ts)
   (coll? ts)  (if (seq ts)
                  (let [f (first ts)
                        [op r] (case f
                                 :and  [every-pred (next ts)]
                                 :nand [nand-fn    (next ts)]
                                 :or   [some-fn    (next ts)]
                                 :nor  [nor-fn     (next ts)]
                                 :xor  [xor-fn     (next ts)]
                                 [some-fn    ts])
                        t-matchers (map #(type-matcher (schema g) %) r)]
                    (apply op t-matchers))
                  ;; Empty collection given: (), [], that's also ok
                  identity)
   :else (RuntimeException.
          (format "Don't know how to create a type matcher for %s" ts))))

(defn has-type?
  "Returns true, iff attributed element `ae' has type `ts'.
  `ts' can be given as anything `type-matcher' understands."
  [ae ts]
  ((type-matcher ae ts) ae))

;;* Graph Access

(defn graph
  "Returns the graph containing the graph element `ge'."
  [^GraphElement ge]
  (.getGraph ge))

;;** Access by ID

(defprotocol IDOps
  "Protocol for types having IDs."
  (id [this]
    "Returns this element's ID.")
  (vertex [this id]
    "Returns the vertex with the given ID.")
  (edge [this id]
    "Returns the edge with the given ID."))

(extend-protocol IDOps
  Graph
  (id [g]
      (.getId g))
  (vertex [g id]
	  (.getVertex g id))
  (edge [g id]
	(.getEdge g id))

  GraphElement
  (id [ge]
      (.getId ge))
  (edge [ge id]
	(-> (.getGraph ge)
	    (.getEdge id)))
  (vertex [ge id]
	  (-> (.getGraph ge)
	      (.getVertex id))))

;;** Edge functions

(defn alpha
  "Returns the start vertex of edge `e'."
  [^Edge e]
  (.getAlpha e))

(defn omega
  "Returns the end vertex of edge `e'."
  [^Edge e]
  (.getOmega e))

(defn this
  "Returns `e's this vertex."
  [^Edge e]
  (.getThis e))

(defn that
  "Returns `e's that vertex."
  [^Edge e]
  (.getThat e))

(defn normal-edge
  "Returns `e's normal (forward-oriented) edge."
  [^Edge e]
  (.getNormalEdge e))

(defn reversed-edge
  "Returns `e's reversed (backward-oriented) edge."
  [^Edge e]
  (.getReversedEdge e))

(defn normal-edge?
  "Returns true, iff `e' is normal (forward-oriented)."
  [^Edge e]
  (.isNormal e))

;;** First, next elements

(defn first-vertex
  "Returns the first vertex of graph `g' accepted by type matcher `tm'."
  ([^Graph g]
     (first-vertex g identity))
  ([^Graph g tm]
     (loop [v (.getFirstVertex g)]
       (if (or (nil? v) (tm v))
         v
         (recur (.getNextVertex v))))))

(defn next-vertex
  "Returns the vertex following `v' in vseq accepted by type matcher `tm'."
  ([^Vertex v]
     (next-vertex v identity))
  ([^Vertex v tm]
     (loop [n (.getNextVertex v)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getNextVertex n))))))

(defn first-edge
  "Returns the first edge of graph `g' accepted by type matcher `tm'."
  ([^Graph g]
     (first-edge g identity))
  ([^Graph g tm]
     (loop [e (.getFirstEdge g)]
       (if (or (nil? e) (tm e))
         e
         (recur (.getNextEdge e))))))

(defn next-edge
  "Returns the edge following `e' in eseq accepted by type matcher `tm'."
  ([^Edge e]
     (next-edge e identity))
  ([^Edge e tm]
     (loop [n (.getNextEdge e)]
       (if (or (nil? n) (tm n))
         n
         (recur (.getNextEdge n))))))

(defn direction-matcher
  "Returns a matcher function that accepts edges of direction `dir'.
  `dir' may be an EdgeDirection enum literal or one of the keywords :in, :out,
  or :inout.
  If `dir' is nil, then any direction is allowed (aka :inout)."
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
     :default (error (format "Unknown direction %s" dir)))))

(defn first-inc
  "Returns the first incidence in iseq of `v' accepted by the type matcher `tm'
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

(defn next-inc
  "Returns the incidence following `e' in the current vertex's iseq accepted by
  type matcher `tm' and direction matcher `dm'."
  ([^Edge e]
     (next-inc e identity identity))
  ([^Edge e tm]
     (next-inc e tm identity))
  ([^Edge e tm dm]
     (loop [i (.getNextIncidence e)]
       (if (or (nil? i) (and (dm i) (tm i)))
         i
         (recur (.getNextIncidence i))))))

;;** Value access (including attribute setting)

(defprotocol ClojureValues2JGraLabValues
  "Protocol for transforming clojure persistent collections/maps into
  equivalent pcollections and ratios to doubles."
  (clj2jgval [coll]))

(extend-protocol ClojureValues2JGraLabValues
  clojure.lang.ISeq
  (clj2jgval [coll]
    (-> (ArrayPVector/empty)
        (.plusAll ^Collection (map clj2jgval coll))))

  clojure.lang.IPersistentVector
  (clj2jgval [coll]
    (-> (ArrayPVector/empty)
        (.plusAll ^Collection (map clj2jgval coll))))

  clojure.lang.IPersistentMap
  (clj2jgval [coll]
    (reduce (fn [m [k v]] (.plus ^org.pcollections.PMap m k v))
            (ArrayPMap/empty)
            (map (fn [k v] [k v])
                 (map clj2jgval (keys coll))
                 (map clj2jgval (vals coll)))))

  clojure.lang.IPersistentSet
  (clj2jgval [coll]
    (-> (ArrayPSet/empty)
        (.plusAll ^Collection (map clj2jgval coll))))

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

(defprotocol ValueAccess
  "Protocol for access to attribute values and record components."
  (value [this attr-or-comp]
    "Returns `this' element's `attr-or-comp' value.")
  (set-value! [this attr val]
    "Sets `this' element's `attr' value to `val' and returns `this'."))

(extend-protocol ValueAccess
  AttributedElement
  (value [ae attr]
    (.getAttribute ae (name attr)))

  (set-value! [ae attr val]
    (dograph! (doto ae (.setAttribute (name attr) (clj2jgval val)))))

  Record
  (value [rec comp]
    (.getComponent rec (name comp))))

(defn record
  "Creates a record of type `t' in the schema of element `e' with component
  values as specified by map `m'.  The map `m' must specify all components, and
  be sure that if a component is of type Integer, then use `Integer/valueOf'."
  [e t m]
  (let [^Graph g (if (instance? Graph e) e (graph e))]
    (.createRecord g
                   ^RecordDomain (domain e t)
                   ^java.util.Map (zipmap (map name (keys m))
                                          (vals m)))))

(defn enum
  "Returns the enum constant `c' of enum type `t' in the schema of element `e'.
  See `domain' for getting a Domain by its qualified name."
  [^AttributedElement e t c]
  (let [^Graph g (if (instance? Graph e) e (graph e))]
    (.getEnumConstant g
                      ^EnumDomain (domain e t)
                      ^String (name c))))

;;** Element Order

(defprotocol ElementOrder
  "Protocol for querying and setting the global order of vertices and edges,
  and the local order of incidences."
  (before? [this other]
    "Returns true, iff `this' element is defined before `other' in the global
  vertex/edge sequence.")
  (after? [this other]
    "Returns true, iff `this' element is defined after `other' in the global
  vertex/edge sequence.")
  (put-before! [this other]
    "Puts `this' element directly before `other' in the graph's vertex/edge
  sequence.")
  (put-after! [this other]
    "Puts `this' element directly after `other' in the graph's vertex/edge
  sequence.")
  (before-inc? [this other]
    "Returns true, iff `this' incidence is defined before `other' in the
  incidence sequence of the current vertex.")
  (after-inc? [this other]
    "Returns true, iff `this' incidence is defined after `other' in the
  incidence sequence of the current vertex.")
  (put-before-inc! [this other]
    "Puts `this' incidence directly before `other' in the current vertex's
  incidence sequence.")
  (put-after-inc! [this other]
    "Puts `this' incidence directly ofter `other' in the current vertex's
  incidence sequence."))

(extend-protocol ElementOrder
  Vertex
  (before? [this other]     (.isBefore this other))
  (after? [this other]      (.isAfter this other))
  (put-before! [this other] (dograph! (.putBefore this other)))
  (put-after! [this other]  (dograph! (.putAfter this other)))

  Edge
  (before? [this other]     (.isBeforeEdge this other))
  (after? [this other]      (.isAfterEdge this other))
  (put-before! [this other] (dograph! (.putBeforeEdge this other)))
  (put-after! [this other]  (dograph! (.putAfterEdge this other)))
  (before-inc? [this other] (.isBeforeIncidence this other))
  (after-inc? [this other]  (.isAfterIncidence this other))
  (put-before-inc! [this other] (dograph! (.putIncidenceBefore this other)))
  (put-after-inc! [this other]  (dograph! (.putIncidenceAfter this other))))

;;* Modifications

;;** Creations

;; TODO: Use the generic Schema.createGraph() method.  But why doesn't that
;; allow for setting the graph id anymore???
(defn create-graph
  "Creates a graph with id `gid', `vmax', and `emax' of the given `schema'
  class using the given `impl' type.  `vmax' and `emax' default to 1000, `impl'
  to ImplementationType/STANDARD."
  ([schema gid]
     (create-graph schema gid 1000 1000 ImplementationType/STANDARD))
  ([schema gid vmax emax]
     (create-graph schema gid vmax emax ImplementationType/STANDARD))
  ([^Schema schema gid vmax emax ^ImplementationType impl]
     (.createGraph schema impl gid
                   (Integer/valueOf (int vmax))
                   (Integer/valueOf (int emax)))))

(defn create-vertex!
  "Creates a new vertex of type `cls' in `g'.
  `cls' is a qualified name given as string, symbol, or keyword."
  [^Graph g cls]
  (let [^VertexClass aec (attributed-element-class g cls)]
    (dograph! (.createVertex g aec))))

(defn create-edge!
  "Creates a new edge of type `cls' starting at `from' and ending at `to'.
  `cls' is a qualified name given as string, symbol, or keyword."
  [cls ^Vertex from ^Vertex to]
  (let [^Graph g (.getGraph from)
        ^EdgeClass aec (attributed-element-class g cls)]
    (dograph! (.createEdge g aec from to))))

(defn set-alpha!
  "Sets the start vertex of `e' to `v' and returns `e'."
  [^Edge e ^Vertex v]
  (dograph! (doto e (.setAlpha v))))

(defn set-omega!
  "Sets the end vertex of `e' to `v' and returns `e'."
  [^Edge e ^Vertex v]
  (dograph! (doto e (.setOmega v))))

(defn set-this!
  "Sets the this vertex of `i' to `v' and returns `i'."
  [^Edge i ^Vertex v]
  (dograph! (doto i (.setThis v))))

(defn set-that!
  "Sets the that vertex of `i' to `v' and returns `i'."
  [^Edge i ^Vertex v]
  (dograph! (doto i (.setThat v))))

;;** Deletions

(defprotocol Delete
  "A protocol for deleting elements."
  (internal-delete [this]
    "Deletes this vertex or edge."))

(extend-protocol Delete
  Vertex (internal-delete [v] (dograph! (.delete v)))
  Edge   (internal-delete [e] (dograph! (.delete e))))

(alter-meta! (var internal-delete) assoc :private true)

(defn delete!
  "Deletes the given graph `elems'.
  Returns the deleted, invalid `elems', or nil, if no element was given."
  [& elems]
  (doseq [ge elems]
    (internal-delete ^GraphElement ge))
  elems)

;;** Relinking edges

(defn relink!
  "Relinks all incidences of vertex `from' to vertex `to' and returns `from'.
  The incidences can be restricted by type spec `ts' and `dir' (see
  `type-matcher' and `direction-matcher')."
  ([from to]
     (relink! from to identity identity))
  ([from to ts]
     (relink! from to ts identity))
  ([from to ts dir]
     (dograph!
      (let [tm (type-matcher from ts)
            dm (direction-matcher dir)]
        (loop [inc (first-inc from tm dm)]
          (when inc
            (set-this! inc to)
            (recur (first-inc from tm dm)))))
      from)))

;;* toString/Serialization/Deserialization

;; Normal toString()
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

;; Printing `read'-able
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
  string that can be read back using `tg-read-str'."
  [obj]
  (binding [*print-dup* true]
    (pr-str obj)))

(defn tg-read-str
  "Reads `str' and returns the object represented by `str'.
  If the object contains vertices or edges, the graphs holding them have to be
  provided as `gs'."
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
  "Reads an object from the file `f', like `clojure.core/slurp'.
  If that object contains vertices and edges, then their hosting graphs have to
  be provided as gs."  [f & gs]
  (apply tg-read-str (slurp f) gs))
