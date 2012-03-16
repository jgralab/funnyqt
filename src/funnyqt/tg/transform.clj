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
  (:use funnyqt.tg.query)
  (:use funnyqt.generic)
  (:use funnyqt.generic-protocols)
  (:use [funnyqt.utils :only [error split-qname]])
  (:require clojure.set)
  (:require clojure.pprint)
  (:require [clojure.tools.macro :as m])
  (:import
   (java.util Arrays)
   (de.uni_koblenz.jgralab Graph Vertex Edge)
   (de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.schema AggregationKind Attribute
                                  AttributedElementClass GraphElementClass
                                  GraphClass EdgeClass Schema VertexClass
                                  RecordDomain EnumDomain)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.impl SchemaImpl)
   (de.uni_koblenz.jgralab.impl.generic InternalAttributesArrayAccess
                                        InternalAttributesArrayAccess$OnAttributesFunction)))


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

(defn- on-attributes-fn
  "`f` : AttributedElement x Object[] -> Object[]"
  [f]
  (reify InternalAttributesArrayAccess$OnAttributesFunction
    (invoke [_ ae ary]
      (f ae ary))))

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

(defmacro with-open-schema [g & body]
  `(let [g# ~g
         ^Schema s# (schema g#)
         was-opened# (.reopen s#)
         r# (do ~@body)]
     (when was-opened#
       (.finish s#))
     r#))


;;# Schema functions

;;## Create a new schema

(defn- create-schema [sqname gcname]
  (let [[prefix sname] (split-qname sqname)]
    (doto (SchemaImpl. sname prefix)
      (.createGraphClass (name gcname)))))

(defn empty-graph [sqname gcname]
  (let [^Schema s (create-schema sqname gcname)]
    (.finish s)
    (create-graph s)))

;;## Creating Enum & Record domains

(defn create-record-domain!
  "Creates a RecordDomain of the given `name` and `comp-doms` in `g`.
  `name` may be a String, keyword or symbol.
  `comp-doms` is a map from component name to domain name.  Both may be given
  as string, keyword, or symbol."
  [g name comp-doms]
  (with-open-schema g
    (let [rd (.createRecordDomain ^Schema (schema g) (clojure.core/name name))]
      (doseq [[comp dom] comp-doms]
        (.addComponent rd (clojure.core/name comp) (domain (schema g) dom)))
      rd)))

(defn create-enum-domain!
  "Creates an EnumDomain with the given `name` and `literals` in `g`.
  `literals` is a seq of literal names.
  `name` and the `literals` may be given as string, keyword, or symbol."
  [g name literals]
  (with-open-schema g
    (let [ed (.createEnumDomain ^Schema (schema g)
                                (clojure.core/name name)
                                (vec (map clojure.core/name
                                          literals)))])))

;;## Creating VertexClasses

(defn- create-vc!
  [g {:keys [qname abstract]}]
  (with-open-schema g
    (-> (.getGraphClass ^Schema (schema g))
        (doto ^VertexClass (.createVertexClass (name qname))
          (.setAbstract (boolean abstract))))))

(defn create-vertex-class!
  "Creates VertexClass + instances in `g`.
  The map given as first argument provides the schema properties.
  For `archs`, see function `create-vertices!`."
  ([g {:keys [qname abstract]
       :or {abstract false}
       :as props}]
     {:pre [qname]}
     (create-vc! g {:qname qname :abstract abstract}))
  ([g {:keys [qname abstract]
       :as props}
    archs]
     {:pre [qname (or (nil? archs) (fn? archs))]}
     (create-vertex-class! g props)
     (create-vertices! g qname archs)))

;;## Creating EdgeClasses

(defn- create-ec!
  [^Graph g {:keys [qname abstract
                    from from-multis from-role from-kind
                    to to-multis to-role to-kind]}]
  (with-open-schema g
    (-> (.getGraphClass ^Schema (schema g))
        (doto (.createEdgeClass
               (name qname)
               (attributed-element-class g from) (first from-multis)
               (second from-multis) from-role from-kind
               (attributed-element-class g to) (first to-multis)
               (second to-multis) to-role to-kind)
          (.setAbstract (boolean abstract))))))

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
     (create-ec! g
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

;; TODO: Make work for deletion and rename!
(defn- fix-attr-array [^Attribute attr elems]
  (let [oaf (on-attributes-fn
             (fn [ae ^objects ary]
               (let [idx (.getAttributeIndex ^AttributedElementClass
                                             (attributed-element-class ae) (.getName attr))
                     ^objects ary (if (nil? ary) (to-array []) ary)
                     front (Arrays/copyOfRange ary (int 0) (int idx))
                     tail  (Arrays/copyOfRange ary (int idx) (int (alength ary)))]
                 (to-array (concat (seq front) [nil] (seq tail))))))]
    (doseq [^InternalAttributesArrayAccess e elems]
      (.invokeOnAttributesArray e oaf)
      (.setDefaultValue attr e))))

(defn- create-attr!
  [g {:keys [qname domain default]}]
  (with-open-schema g
    (let [[qn aname _] (split-qname qname)
          aec          ^AttributedElementClass (attributed-element-class g qn)]
      (.addAttribute aec aname (funnyqt.tg.core/domain g domain) default)
      (fix-attr-array (.getAttribute aec aname)
                          (cond
                           (instance? GraphClass aec)  [g]
                           (instance? VertexClass aec) (vseq g (funnyqt.generic-protocols/qname aec))
                           (instance? EdgeClass aec)   (eseq g (funnyqt.generic-protocols/qname aec))
                           :else (error (format "Cannot handle %s." aec)))))))

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

(defn- attribute-names [^AttributedElementClass aec]
  (set (map #(.getName ^Attribute %1)
            (.getAttributeList aec))))

(defn- check-for-attribute-clash [aec1 aec2]
  (let [isect (clojure.set/intersection (attribute-names aec1)
                                        (attribute-names aec2))]
    (when (seq isect)
      (error (format "Attribute clash between %s and %s: %s"
                     aec1 aec2 isect)))))

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`."
  [g super & subs]
  (with-open-schema g
    (let [^AttributedElementClass s (attributed-element-class g super)]
      (doseq [sub subs]
        (let [subaec (attributed-element-class g sub)]
          (check-for-attribute-clash s subaec)
          (if (isa? (class s) VertexClass)
            (do
              (.addSuperClass ^VertexClass subaec ^VertexClass s)
              (doseq [a (.getAttributeList s)]
                (fix-attr-array a (vseq g sub))))
            (do
              (.addSuperClass ^EdgeClass subaec ^EdgeClass s)
              (doseq [a (.getAttributeList s)]
                (fix-attr-array a (eseq g sub))))))))))

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`."
  [g sub & supers]
  (with-open-schema g
    (let [s (attributed-element-class g sub)]
      (doseq [super supers]
        (let [^AttributedElementClass superaec (attributed-element-class g super)]
          (check-for-attribute-clash superaec s)
          (if (isa? (class s) VertexClass)
            (do
              (.addSuperClass ^VertexClass s ^VertexClass superaec)
              (doseq [a (.getAttributeList superaec)]
                (fix-attr-array a (vseq g s))))
            (do
              (.addSuperClass ^EdgeClass s ^EdgeClass superaec)
              (doseq [a (.getAttributeList superaec)]
                (fix-attr-array a (eseq g s))))))))))

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
