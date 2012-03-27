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
  (:use [funnyqt.utils :only [error split-qname pr-identity]])
  (:require clojure.set)
  (:require clojure.pprint)
  (:require [clojure.tools.macro :as m])
  (:import
   (java.util Arrays)
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration)
   (de.uni_koblenz.jgralab.schema AggregationKind Attribute
                                  AttributedElementClass GraphElementClass
                                  GraphClass EdgeClass Schema VertexClass
                                  RecordDomain EnumDomain)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.impl SchemaImpl NamedElementImpl GraphClassImpl)
   (de.uni_koblenz.jgralab.impl.generic InternalAttributesArrayAccess
                                        InternalAttributesArrayAccess$OnAttributesFunction)))


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

(defn- on-attributes-fn
  "`f` : AttributedElement x Object[] -> Object[]"
  [f]
  (reify InternalAttributesArrayAccess$OnAttributesFunction
    (invoke [_ ae ary]
      (f ae ary))))

(defn- element-seq
  [g aec]
  (cond
   (instance? GraphClass aec)  [g]
   (instance? VertexClass aec) (vseq g (funnyqt.generic-protocols/qname aec))
   (instance? EdgeClass aec)   (eseq g (funnyqt.generic-protocols/qname aec))
   :else (error (format "Cannot handle %s." aec))))

(defn- checked-merge
  "Like a arity 2 variant of `clojure.core/merge` but checks for uniqueness of
  keys.  Throws an exception on a key clash."
  [m1 m2]
  (if-let [isect (seq (clojure.set/intersection
                       (keys m1) (keys m2)))]
    (error (format "Traceability clash! %s should be added but were already present."
                   isect))
    (merge m1 m2)))

(defn- into-trace-map [trace-map aec new]
  (update-in trace-map [aec] checked-merge new))

(defn- img-internal-1
  "Returns the image of `arch` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec arch]
  (or (when-let [m (@*img* aec)]
        (m arch))
      (first (remove nil?
                     (map #(img-internal-1 %1 arch)
                          (.getDirectSubClasses ^GraphElementClass aec))))))

(defn- img-internal
  [aec arch]
  (or (img-internal-1 aec arch)
      (error (format "Couldn't resolve image of %s in img fn of %s: %s"
                     arch aec @*img*))))

(defn- arch-internal-1
  "Returns the archetype of `img` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec img]
  (or (when-let [m (@*arch* aec)]
        (m img))
      (first (remove nil?
                     (map #(arch-internal-1 %1 img)
                          (.getDirectSubClasses ^GraphElementClass aec))))))

(defn- arch-internal
  [aec img]
  (or (arch-internal-1 aec img)
      (error (format "Couldn't resolve archetype of %s in arch fn of %s: %s"
                     img aec @*arch*))))

(defn get-field-reflectively
  [^Class klass obj field-name]
  (-> klass
      (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.get obj)))

(defn set-field-reflectively!
  [^Class klass obj field-name value]
  (-> klass
      (.getDeclaredField (name field-name))
      (doto (.setAccessible true))
      (.set obj value)))

(defn call-method-refectively
  [^Class klass obj method-name paramtypes & args]
  (-> klass
      (.getDeclaredMethod (name method-name)
                          (into-array Class paramtypes))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

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
            (swap! *img*  into-trace-map vc img))
          (when (bound? #'*arch*)
            (swap! *arch* into-trace-map vc arch))
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
            (swap! *img*  into-trace-map ec img))
          (when (bound? #'*arch*)
            (swap! *arch* into-trace-map ec arch))
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

;;## Create a new schema & empty graph

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
                                ^String (clojure.core/name name)
                                ^java.util.List (vec (map clojure.core/name
                                                          literals)))])))


;;## VertexClasses

;;### Creating

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

;;## EdgeClasses

;;### Creating

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

;;## Attributes

;;### Creating

(defn- fix-attr-array-after-add!
  "Resizes the attributes array of all `elems` after adding the `new-attrs`."
  [elems & new-attrs]
  (when (seq new-attrs)
    (let [oaf (on-attributes-fn
               (fn [ae ^objects ary]
                 (let [^AttributedElementClass aec (attributed-element-class ae)
                       new-attrs (set new-attrs)
                       ^objects new-ary (make-array Object (.getAttributeCount aec))]
                   (loop [atts (.getAttributeList aec), posinc 0]
                     (if (seq atts)
                       (let [^Attribute a (first atts)
                             idx (.getAttributeIndex aec (.getName a))]
                         (if (new-attrs a)
                           (recur (rest atts) (inc posinc))
                           (do
                             (aset new-ary idx (aget ary (- idx posinc)))
                             (recur (rest atts) posinc))))
                       new-ary)))))]
      (doseq [^InternalAttributesArrayAccess e elems]
        (.invokeOnAttributesArray e oaf)
        (doseq [^Attribute a new-attrs]
          (.setDefaultValue a e))))))

(defn- create-attr!
  [g {:keys [qname domain default]}]
  (with-open-schema g
    (let [[qn aname _] (split-qname qname)
          aec          ^AttributedElementClass (attributed-element-class g qn)]
      (.createAttribute aec aname (funnyqt.tg.core/domain g domain) default)
      (fix-attr-array-after-add!
       (element-seq g aec)
       (.getAttribute aec aname)))))

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

;;### Renaming

(defn rename-attribute!
  "Renames attribute `oldname` to `newname` in the schema of graph `g`.
  `oldname` is a fully qualified name (e.g., `contacts.Person.name`), and
  `newname` is just the new attribute name (e.g., `lastName`)"
  [g oldname newname]
  (let [[qn aname _] (split-qname oldname)
        aec ^AttributedElementClass (attributed-element-class g qn)
        ^Attribute attribute (.getAttribute aec aname)]
    ;; Check that no subclass (or even this class) contains attribute `newname`
    (when (.containsAttribute aec (name newname))
      (error (format "%s already has a %s attribute." aec (name newname))))
    (when (instance? GraphElementClass aec)
      (doseq [^GraphElementClass sub (.getAllSubClasses ^GraphElementClass aec)]
        (when (.containsAttribute sub (name newname))
          (error (format "%s subclass %s already has a %s attribute."
                         aec sub (name newname))))))
    (let [old-idx-map (if (instance? GraphClass aec)
                        {aec (.getAttributeIndex aec aname)}
                        (reduce (fn [m ^GraphElementClass sub]
                                  (assoc m sub (.getAttributeIndex sub aname)))
                                {}
                                (cons aec (.getAllSubClasses ^GraphElementClass aec))))
          oaf (on-attributes-fn
               (fn [^AttributedElement ae ^objects ary]
                 (let [^AttributedElementClass klass (attributed-element-class ae)
                       old-idx (old-idx-map klass)
                       new-idx (.getAttributeIndex klass (name newname))]
                   (if (== old-idx new-idx)
                     ary
                     (amap ary idx ret (cond
                                        (< idx new-idx) (aget ary (dec idx))
                                        (> idx new-idx) (aget ary (inc idx))
                                        :else (aget ary old-idx)))))))]
      (with-open-schema g
        (.setName attribute (name newname)))
      (doseq [^InternalAttributesArrayAccess ae (element-seq g aec)]
        (.invokeOnAttributesArray ae oaf)))))

;;## Type Hierarchies

;;### Creating

(defn- attr-name-dom-map
  [^AttributedElementClass aec]
  (apply hash-map (mapcat (fn [^Attribute a]
                            [(.getName a) (.getDomain a)])
                          (.getAttributeList aec))))

(defn- handle-attribute-clashes
  [^AttributedElementClass super ^AttributedElementClass sub]
  (let [supmap (attr-name-dom-map super)
        supkeys (set (keys supmap))
        submap (attr-name-dom-map sub)
        isect (clojure.set/intersection
               supkeys (set (keys submap)))]
    ;; For attribute clashes, we have to check if the domains match.  If so,
    ;; then we can simply delete the attribute at the subclass.  Otherwise,
    ;; that's an error.
    (doseq [a isect]
      (cond
       ;; Inheriting the same attribute via multiple paths is ok
       (= (.getAttribute super a) (.getAttribute sub a)) nil
       ;; Inheriting an equivalent attribute (same name/domain) is ok, if the
       ;; subclass itself declares it.  Then, it can simply be deleted in
       ;; favour of the inherited one.
       (= (supmap a) (submap a)) (if-let [^Attribute oa (.getOwnAttribute sub a)]
                                   (.delete oa)
                                   (error (format
                                           (str "%s tries to inherit different %s attributes. "
                                                "It already has one from %s and now gets another one from %s.")
                                           sub a (.getAttributedElementClass (.getAttribute sub a)) super)))
       :else (error (format "%s tries to inherit %s with different domains: %s from %s and %s from %s"
                            sub a
                            (supmap a) (.getAttributedElementClass (.getAttribute sub a))
                            (submap a) super))))
    ;; Return the seq of attributes that are really new for the subclass.  For
    ;; those, the attributes array has to be adjusted.
    (map #(.getAttribute super %1)
         (remove isect supkeys))))

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`."
  [g super & subs]
  (with-open-schema g
    (let [^GraphElementClass superaec (attributed-element-class g super)
          subaecs (map #(attributed-element-class g %1) subs)]
      (doseq [^GraphElementClass subaec subaecs]
        (let [new-atts (handle-attribute-clashes superaec subaec)]
          (if (isa? (class superaec) VertexClass)
            (do
              (.addSuperClass ^VertexClass subaec ^VertexClass superaec)
              (apply fix-attr-array-after-add! (vseq g subaec)
                     new-atts))
            (do
              (.addSuperClass ^EdgeClass subaec ^EdgeClass superaec)
              (apply fix-attr-array-after-add! (eseq g subaec)
                     new-atts))))))))

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`."
  [g sub & supers]
  (doseq [super supers]
    (add-sub-classes! g super sub)))

;;# The transformation macro itself

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
