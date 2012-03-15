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
  (:use funnyqt.generic)
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

(def ^{:dynamic true :private true}
  *img*           nil)

(def ^{:dynamic true :private true}
  *arch*          nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the start vertex class."}
  resolve-alpha nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the end vertex class."}
  resolve-omega nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-attribute!` / `set-values!`.
  Resolves the image of the given archetype in the img function corresponding
  to the attributed element class of the current attribute."}
  resolve-element nil)

;;# Utility functions

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
  ((@*img* aec) arch))

(defn- arch-internal
  "Returns the archetype of `img` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec img]
  ((@*arch* aec) img))

;;# Instance only functions

(defn create-vertices! [g cls archfn]
  (let [^VertexClass vc (attributed-element-class g cls)]
    (loop [as (archfn)
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
          (when (bound? #'*img*)
            (swap! *img*  merge-into-mappings vc img))
          (when (bound? #'*arch*)
            (swap! *arch* merge-into-mappings vc arch))
          (keys arch))))))

(defn create-edges! [g cls archfn]
  (let [^EdgeClass ec (attributed-element-class g cls)
        saec (-> ec (.getFrom) (.getVertexClass))
        eaec (-> ec (.getTo)   (.getVertexClass))]
    (loop [as (binding [resolve-alpha #(img-internal saec %)
                        resolve-omega #(img-internal eaec %)]
                (doall (archfn)))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [[a al om] (first as)
              e (create-edge! g cls al om)]
          (recur (rest as) (assoc! im a e) (assoc! am e a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when (bound? #'*img*)
            (swap! *img*  merge-into-mappings ec img))
          (when (bound? #'*arch*)
            (swap! *arch* merge-into-mappings ec arch))
          (keys arch))))))

(defn set-values! [g a valfn]
  (let [[aecname attrname _] (split-qname a)
        aec (attributed-element-class g aecname)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (img-internal aec arch))]
                         (doall (valfn)))]
      (set-value! elem attrname val))))


;;# Schema functions

;;## Create a new schema

(defn- create-schema [sqname gcname]
  (let [[prefix sname] (split-qname sqname)]
    (doto (SchemaImpl. sname prefix)
      (.createGraphClass (name gcname)))))

(defn empty-graph [sqname gcname]
  (create-graph (create-schema sqname gcname)))

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
  ([g {:keys [qname abstract]
       :or {abstract false}
       :as props}]
     {:pre [qname]}
     (create-vc! (schema g) {:qname qname :abstract abstract}))
  ([g {:keys [qname abstract]
       :as props}
    archs]
     {:pre [qname (or (nil? archs) (fn? archs))]}
     (create-vertex-class! g props)
     (create-vertices! g qname archs)))

;;## Creating EdgeClasses

(defn- create-ec!
  [^Schema s {:keys [qname abstract
                     from from-multis from-role from-kind
                     to to-multis to-role to-kind]}]
  (-> (.getGraphClass s)
      (doto (.createEdgeClass
             (name qname)
             (attributed-element-class s from) (first from-multis)
             (second from-multis) from-role from-kind
             (attributed-element-class s to) (first to-multis)
             (second to-multis) to-role to-kind)
        (.setAbstract (boolean abstract)))))

(defn create-edge-class!
  "Creates an EdgeClass + instances.
  The map given as first argument provides the schema properties.
  For `archs`, see function `create-edges!`."
  ([g {:keys [qname abstract
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
     (create-ec! (schema g)
                 {:qname qname :abstract abstract
                  :from from :from-multis from-multis :from-role (name from-role) :from-kind from-kind
                  :to   to   :to-multis   to-multis   :to-role   (name to-role)   :to-kind   to-kind}))
  ([g {:keys [qname abstract
              from from-multis from-role from-kind
              to   to-multis   to-role   to-kind]
       :as props}
    archs]
     {:pre [qname (or (nil? archs) (fn? archs))]}
     (create-edge-class! g props)
     (create-edges! g qname archs)))

;;## Creating Attributes

;; TODO: Handle default values!
(defn- create-attr!
  [g {:keys [qname domain default]}]
  (let [[qn a _] (split-qname qname)
        elem     ^AttributedElementClass (attributed-element-class g qn)]
    (.addAttribute elem a (domain g domain))))

(defn create-attribute!
  "Creates an attribute and sets values.
  The map given as first argument determines the schema properties.
  For `valfn`, see `set-values!`."
  ([g {:keys [qname domain default] :as props}]
     {:pre [qname domain]}
     (create-attr! g props))
  ([g {:keys [qname domain default] :as props} valfn]
     {:pre [valfn]}
     (create-attribute! g props)
     (set-values! g qname valfn)))

;;## Creating type hierarchies

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`."
  [g super & subs]
  (let [s (attributed-element-class g super)]
    (if (isa? (class s) VertexClass)
      (doseq [sub subs]
        (.addSuperClass ^VertexClass (attributed-element-class g sub) ^VertexClass s))
      (doseq [sub subs]
        (.addSuperClass ^EdgeClass (attributed-element-class g sub) ^EdgeClass s)))))

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`."
  [g sub & supers]
  (let [s (attributed-element-class g sub)]
    (if (isa? (class s) VertexClass)
      (doseq [super supers]
        (.addSuperClass ^VertexClass s ^VertexClass (attributed-element-class g super)))
      (doseq [super supers]
        (.addSuperClass ^EdgeClass s ^EdgeClass (attributed-element-class g super))))))

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
        args (vec (first more))
        body (next more)]
    `(defn ~name
       ~(meta name)
       ~args
       (binding [*arch* (atom {})
                 *img*  (atom {})]
         (do ~@body)))))
