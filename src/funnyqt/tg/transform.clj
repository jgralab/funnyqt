(ns funnyqt.tg.transform
  "Schema-creating transformations on TGraphs.

This is basically a GReTL implementation in Clojure.  Consequently, it has
elementary creation operations in terms of functions.  In contrast to GReTL,
`create-edges!` and `create-edge-class!` receive the start and end vertices of
the edges to be created, not their archetypes.  Similarily, `set-values!` and
`create-attribute!` want the element whose attribute is going to be set, not
the archetype of it.  This design decision allows for using those functions to
extend an existing Graph without having to create artificial archetypes
before."
  (:use funnyqt.tg.core)
  (:use [funnyqt.utils :only [error split-qname]])
  (:require clojure.set)
  (:require clojure.pprint)
  (:require [clojure.tools.macro :as m])
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge)
   (de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.schema AggregationKind Attribute
                                  AttributedElementClass GraphElementClass
                                  GraphClass EdgeClass Schema VertexClass
                                  RecordDomain EnumDomain)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.impl SchemaImpl)))


;;# TODO List

;; - The usage of those dynamic Vars is not really optimal.  There's no good
;;   reason why you shouldn't be able to use create-vertices! outside of a
;;   transformation.  The mapping should be passed along...
;;
;; - Allow for many target graphs!
;;
;; - Implement Collection and Map attribute creation: List<String>,
;;   Map<Integer,String>, conversion of attribute values is already done.  And
;;   using (domain myelem [Map Integer String]) one can get the right domain.
;;
;; - Implement copy ops for VertexClasses, EdgeClasses, Attributes, and
;;   Record/EnumDomains...
;;
;; - arch/image-Funktionen müssen vom User bestimmt werden können (um nicht
;;   mehr strikt bijektiv zu sein)
;;
;;   + implizites globale Funktionen (wie jetzt)
;;
;;   + Blöcke mit eigenen Bindings (with-traceability foo (op1 ...) (op2 ...))

;;# Dynamic vars

(def ^{:dynamic true}
  $target-graph  nil)
(def ^{:dynamic true}
  $target-schema nil)
(def ^{:dynamic true :private true}
  $img           nil)
(def ^{:dynamic true :private true}
  $arch          nil)
(def ^{:dynamic true}
  $on-graph-fns  nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the start vertex class."}
  r-alpha nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the end vertex class."}
  r-omega nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-attribute!` / `set-values!`.
  Resolves the image of the given archetype in the img function corresponding
  to the attributed element class of the current attribute."}
  r-elem nil)

;;# Utility functions

(defn- aec-internal
  "Gets the AttributedElementClass by the given qname.
  Can only be called inside a deftransformation."
  [qname]
  (let [aec (.getAttributedElementClass
             ^Schema $target-schema (name qname))]
    (or aec
        (error (format "No such AttributedElementClass %s."
                       qname)))))

(defn- attr-internal
  "Returns the Attribute given by its qualified name.
  Can only be called inside a deftransformation."
  [qname]
  (let [[aec-qname attr-name _] (split-qname (name qname))
        ^AttributedElementClass aec (aec-internal aec-qname)]
    (or (.getAttribute aec attr-name)
        (error (format "No such Attribute %s at AttributedElementClass %s."
                       (name qname) aec)))))

(defn- dom-internal
  "Returns the Domain given by its qualified name `qname`.
  Can only be called inside a `deftransformation`."
  [qname]
  (.getDomain ^Schema $target-schema (name qname)))

(defn- merge-into-mappings
  "Merges into `oldmap` the new mappings for `cls` (an GraphElementClass)."
  [oldmap ^GraphElementClass cls new]
  (let [kvs (flatten
             (map (fn [c]
                    (let [old (oldmap c)
                          isect (seq (clojure.set/intersection
                                      (into #{} (keys old))
                                      (into #{} (keys new))))]
                      (when isect
                        (error (format "The keys %s already exist in img/arch"
                                       isect)))
                      [c (merge old new)]))
                  (conj (filter #(not (.isInternal ^AttributedElementClass %))
                                (.getAllSuperClasses cls))
                        cls)))]
    (apply assoc oldmap kvs)))

(defn- img-internal
  "Returns the image of `arch` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec arch]
  ((@$img aec) arch))

(defn img
  "Returns the map from archetypes to images for `aec`.
  `aec` can be given as string, symbol, or keyword denoting its qualified
  name."
  [aec]
  (@$img (aec-internal aec)))

(defn- arch-internal
  "Returns the archetype of `img` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec img]
  ((@$arch aec) img))

(defn arch
  "Returns the map from images to archetypes for `aec`.
  `aec` can be given as string, symbol, or keyword denoting its qualified
  name."
  [aec]
  (@$arch (aec-internal aec)))

;;# Instance only functions

(defn create-vertices!
  "Creates one vertex of type `cls` in `g` for each archetype returned by `archfn`.
  `archfn` has to be a function resulting in a seq of arbitrary
  objects (archetypes).  Returns a seq of the new vertices."
  [g cls archs]
  {:pre [cls (fn? archs)]}
  (let [^VertexClass vc (aec-internal cls)]
    (loop [as (archs)
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (create-vertex! g cls)
              a (first as)]
          ;;(println "Created" v "for" a)
          (recur (rest as)
                 (assoc! im a v)
                 (assoc! am v a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (swap! $img  merge-into-mappings vc img)
          (swap! $arch merge-into-mappings vc arch)
          (keys arch))))))

(defn create-edges!
  "Creates one edge of type `cls` for each archetype in `archfn`.
  `archfn` has to be a function resulting in a seq of triples.  Each triple has
  the form [arch start end] where arch is the archetype of the new edge, start
  is the new edge's start vertex, and end is the new edge's end vertex. Returns
  a seq of the new edges.
  See `r-alpha` and `r-omega` for conveniently resolving the start and end
  vertex by archetypes."
  [g cls archs]
  {:pre [cls (fn? archs)]}
  (let [^EdgeClass ec (aec-internal cls)
        saec (-> ec (.getFrom) (.getVertexClass))
        eaec (-> ec (.getTo)   (.getVertexClass))]
    (loop [as (binding [r-alpha #(img-internal saec %)
                        r-omega #(img-internal eaec %)]
                (doall (archs)))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [[a al om] (first as)
              e (create-edge! g cls al om)]
          (recur (rest as) (assoc! im a e) (assoc! am e a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (swap! $img  merge-into-mappings ec img)
          (swap! $arch merge-into-mappings ec arch)
          (keys arch))))))

(defn set-values!
  "Set the attribute of valfn's keys to the corresponding value.
  `a` is a qualified attribute name (foo.Bar.baz) and valfn a function
  resulting in a map assigning to attributed elements a value.  See `r-elem`
  for conveniently resolving the attributed element by archetype.  Returns a
  seq of attributed elements for whom an attribute has been set."
  [a valfn]
  {:pre [a (fn? valfn)]}
  (let [^Attribute a (attr-internal a)
        aec (.getAttributedElementClass a)
        name (.getName a)]
    (doseq [[elem val] (binding [r-elem (fn [a] (img-internal aec a))]
                         (doall (valfn)))]
      (set-value! elem name val))))

;;# Schema functions

;;## Creating Enum & Record domains

(defn create-record-domain!
  "Creates a RecordDomain of the given `name` and `comp-doms` in `sog`.
  `name` may be a String, keyword or symbol.
  `comp-doms` is a map from component name to domain name.  Both may be given
  as string, keyword, or symbol."
  [sog name comp-doms]
  (let [rd (.createRecordDomain ^Schema (schema sog) (clojure.core/name name))]
    (doseq [[comp dom] comp-doms]
      (.addComponent rd (clojure.core/name comp) (domain (schema sog) dom)))
    rd))

(defn create-enum-domain!
  "Creates an EnumDomain with the given `name` and `literals` in `sog`.
  `literals` is a seq of literal names.
  `name` and the `literals` may be given as string, keyword, or symbol."
  [sog name literals]
  (let [ed (.createEnumDomain ^Schema (schema sog)
                              (clojure.core/name name)
                              (vec (map clojure.core/name
                                        literals)))]))

;;## Creating VertexClasses

(defn- create-vc!
  [^Schema s {:keys [qname abstract]}]
  (-> (.getGraphClass s)
      (doto (.createVertexClass (name qname))
        (.setAbstract (boolean abstract)))))

(defn create-vertex-class!
  "Creates VertexClass + instances in `sog`.
  The map given as first argument provides the schema properties.
  For `archs`, see function `create-vertices!`."
  ([sog {:keys [qname abstract]
         :or {abstract false}
         :as props}]
     {:pre [qname]}
     (create-vc! (schema sog) {:qname qname :abstract abstract}))
  ([sog {:keys [qname abstract]
         :as props}
    archs]
     {:pre [qname (or (nil? archs) (fn? archs))]}
     (create-vertex-class! sog props)
     (create-vertices! qname archs)))

;;## Creating EdgeClasses

(defn- create-ec!
  [{:keys [qname abstract
           from from-multis from-role from-kind
           to to-multis to-role to-kind]}]
  (-> (.getGraphClass ^Schema $target-schema)
      (doto (.createEdgeClass (name qname)
                              (aec-internal from) (first from-multis)
                              (second from-multis) from-role from-kind
                              (aec-internal to) (first to-multis)
                              (second to-multis) to-role to-kind)
        (.setAbstract (boolean abstract)))))

(defn create-edge-class!
  "Creates an EdgeClass + instances.
  The map given as first argument provides the schema properties.
  For `archs`, see function `create-edges!`."
  ([{:keys [qname abstract
            from from-multis from-role from-kind
            to   to-multis   to-role   to-kind]
     :or {abstract false
          from-multis [0, Integer/MAX_VALUE]
          from-role ""
          from-kind AggregationKind/NONE
          to-multis [0, Integer/MAX_VALUE]
          to-role ""
          to-kind AggregationKind/NONE}
     :as props}]
     {:pre [qname to from]}
     (create-ec! {:qname qname :abstract abstract
                  :from from :from-multis from-multis :from-role (name from-role) :from-kind from-kind
                  :to   to   :to-multis   to-multis   :to-role   (name to-role)   :to-kind   to-kind}))
  ([{:keys [qname abstract
            from from-multis from-role from-kind
            to   to-multis   to-role   to-kind]
     :as props}
    archs]
     {:pre [qname (or (nil? archs) (fn? archs))]}
     (create-edge-class! props)
     (create-edges! qname archs)))

;;## Creating Attributes

;; TODO: Handle default values!
(defn- create-attr!
  [{:keys [qname domain default]}]
  (let [[qn a _] (split-qname qname)
        elem     ^AttributedElementClass (aec-internal qn)]
    (.addAttribute elem a (dom-internal domain))))

(defn create-attribute!
  "Creates an attribute and sets values.
  The map given as first argument determines the schema properties.
  For `valfn`, see `set-values!`."
  ([{:keys [qname domain default] :as props}]
     {:pre [qname domain]}
     (create-attr! props))
  ([{:keys [qname domain default] :as props} valfn]
     {:pre [valfn]}
     (create-attribute! props)
     (set-values! qname valfn)))

;;## Creating type hierarchies

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`."
  [super & subs]
  (let [s (aec-internal super)]
    (if (isa? (class s) VertexClass)
      (doseq [sub subs]
        (.addSuperClass ^VertexClass (aec-internal sub) ^VertexClass s))
      (doseq [sub subs]
        (.addSuperClass ^EdgeClass (aec-internal sub) ^EdgeClass s)))))

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`."
  [sub & supers]
  (let [s (aec-internal sub)]
    (if (isa? (class s) VertexClass)
      (doseq [super supers]
        (.addSuperClass ^VertexClass s ^VertexClass (aec-internal super)))
      (doseq [super supers]
        (.addSuperClass ^EdgeClass s ^EdgeClass (aec-internal super))))))

;;# The transformation macro itself

;; TODO: Generalize to allow for multiple target graphs.  Hm, thinking 'bout
;; it, maybe I should drop all that magic with $target-graph/schema and stuff
;; and make those parameters.  But then I have to think about how the target
;; graph can be bound before it exists (maybe delay/force...).
(defmacro deftransformation
  "Create a new transformation named `name` with optional `doc-string` and
  optional `attr-map`, the given `params` (input graph args), and the given
  `body`."
  ;; Nicer arglist in doc
  {:arglists '([name doc-string? attr-map? [params*] & body])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        in-graphs (first more)
        body (next more)]
    `(defn ~name
       ~(-> (meta name)
            (assoc :doc (str (:doc (meta name))
             "\n\n  The last parameter out defines the target graph.

    - If a graph is given, use that graph as target graph.
    - If a schema is given, use a new graph of this schema as target graph.
    - If a vector [<sname> <gcname>] is given, create a new schema (and graph)
      with the given schema and graph class name.

  The generated transformation function returns the target graph.")))
       [~@in-graphs out#]
       (binding [$arch          (atom {})
                 $img           (atom {})
                 $on-graph-fns  (atom [])
                 $target-schema
                 (cond
                  (instance? Schema out#) out#
                  (instance? Graph  out#) (.getSchema ^Graph out#)
                  (vector? out#) (let [sqname# (first out#)
                                       [prefix# sname#] (split-qname sqname#)]
                                   (or
                                    (try
                                      (-> (SchemaClassManager/instance sqname#)
                                          (.loadClass sqname#)
                                          (.getMethod "instance" (into-array Class []))
                                          (.invoke nil (into-array Object [])))
                                      (catch Exception e# nil))
                                    (doto (SchemaImpl. sname# prefix#)
                                      (.createGraphClass (name (second out#))))))
                  :default (error (str "No target graph, target schema, "
                                       " or vector [sname, gcname] given")))]
         (let [existing# (> 0 (.getVertexClassCount (.getGraphClass ^Schema $target-schema)))]
           (if-not existing#
             (do ~@body)
             (println "Using existing schema" (.getQualifiedName ^Schema $target-schema)))
           (binding [$target-graph (if (instance? Graph out#)
                                     out#
                                     (do
                                       (.finish ^Schema $target-schema)
                                       (create-graph $target-schema
                                                     (str "TransformationCreated-"
                                                          (System/currentTimeMillis)))))]
             (doseq [f# @$on-graph-fns] (f#))
             $target-graph))))))
