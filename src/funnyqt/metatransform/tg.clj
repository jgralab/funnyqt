(ns funnyqt.metatransform.tg
  "Schema-creating transformations on TGraphs."
  (:require clojure.set
            clojure.pprint
            [clojure.tools.macro :as m]
            [funnyqt.generic :as p]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.query.tg :as qtg]
            [funnyqt.extensional.tg :as etg])
  (:import
   (java.util Arrays)
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Attribute
                                  AttributedElementClass GraphElementClass
                                  GraphClass EdgeClass Schema VertexClass
                                  RecordDomain EnumDomain)
   (de.uni_koblenz.jgralab.schema.impl.compilation SchemaClassManager)
   (de.uni_koblenz.jgralab.schema.impl SchemaImpl NamedElementImpl GraphClassImpl)
   (de.uni_koblenz.jgralab.impl.generic InternalAttributesArrayAccess
                                        InternalAttributesArrayAccess$OnAttributesFunction)))

;; TODO: When finishing a schema, we need to make sure to remove all previously
;; cached type-matchers in funnyqt.tg/type-matcher-cache.

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
   (instance? VertexClass aec) (tg/vseq g (funnyqt.generic/qname aec))
   (instance? EdgeClass aec)   (tg/eseq g (funnyqt.generic/qname aec))
   :else (u/errorf "Cannot handle %s." aec)))

(defmacro with-open-schema [g & body]
  `(let [g# ~g
         ^Schema s# (tg/schema g#)
         was-opened# (.reopen s#)
         r# (do ~@body)]
     (when was-opened#
       (.finish s#))
     r#))

;;# Schema functions

;;## Create a new schema & empty graph

(defn- create-schema [sqname gcname]
  (let [[prefix sname] (u/split-qname sqname)]
    (doto (SchemaImpl. sname prefix)
      (.createGraphClass (name gcname)))))

(defn empty-graph [sqname gcname]
  (let [^Schema s (create-schema sqname gcname)]
    (.finish s)
    (tg/new-graph s)))

;;## Creating Enum & Record domains

(defn create-record-domain!
  "Creates a RecordDomain of the given `name` and `comp-doms` in `g`.
  `name` may be a String, keyword or symbol.
  `comp-doms` is a map from component name to domain name.  Both may be given
  as string, keyword, or symbol."
  [g name comp-doms]
  (with-open-schema g
    (let [rd (.createRecordDomain ^Schema (tg/schema g) (clojure.core/name name))]
      (doseq [[comp dom] comp-doms]
        (.addComponent rd (clojure.core/name comp) (tg/domain (tg/schema g) dom)))
      rd)))

(defn create-enum-domain!
  "Creates an EnumDomain with the given `name` and `literals` in `g`.
  `literals` is a seq of literal names.
  `name` and the `literals` may be given as string, keyword, or symbol."
  [g name literals]
  (with-open-schema g
    (let [ed (.createEnumDomain ^Schema (tg/schema g)
                                ^String (clojure.core/name name)
                                ^java.util.List  (mapv clojure.core/name
                                                       literals))])))


;;## VertexClasses

;;### Creating

(defn- create-vc!
  [g {:keys [qname abstract]}]
  (with-open-schema g
    (-> (.getGraphClass ^Schema (tg/schema g))
        (doto ^VertexClass (.createVertexClass (name qname))
          (.setAbstract (boolean abstract))))))

(defn create-vertex-class!
  "Creates VertexClass + instances in `g`.
  The map given as first argument provides the schema properties.
  For `archs`, see function `etg/create-vertices!`."
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
     (etg/create-vertices! g qname archs)))

;;## EdgeClasses

;;### Creating

(defn- create-ec!
  [^Graph g {:keys [qname abstract
                    from from-multis from-role from-kind
                    to to-multis to-role to-kind]}]
  (with-open-schema g
    (-> (.getGraphClass ^Schema (tg/schema g))
        (doto (.createEdgeClass
               (name qname)
               (tg/attributed-element-class g from) (first from-multis)
               (second from-multis) from-role from-kind
               (tg/attributed-element-class g to) (first to-multis)
               (second to-multis) to-role to-kind)
          (.setAbstract (boolean abstract))))))

(defn create-edge-class!
  "Creates an EdgeClass + instances.
  The map given as first argument provides the schema properties.
  For `archs`, see function `etg/create-edges!`."
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
     (etg/create-edges! g qname archs)))

;;## Attributes

;;### Creating

(defn- fix-attr-array-after-add!
  "Resizes the attributes array of all `elems` after adding the `new-attrs`."
  [elems & new-attrs]
  (when (seq new-attrs)
    (let [oaf (on-attributes-fn
               (fn [ae ^objects ary]
                 (let [^AttributedElementClass aec (tg/attributed-element-class ae)
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
    (let [[qn aname _] (u/split-qname qname)
          aec          ^AttributedElementClass (tg/attributed-element-class g qn)]
      (.createAttribute aec aname (tg/domain g domain) default)
      (fix-attr-array-after-add!
       (element-seq g aec)
       (.getAttribute aec aname)))))

(defn create-attribute!
  "Creates an attribute and sets values.
  The map given as first argument determines the schema properties.
  For `valfn`, see `etg/set-values!`."
  ([g {:keys [qname domain default] :as props}]
     {:pre [qname domain]}
     (create-attr! g props))
  ([g {:keys [qname domain default] :as props} valfn]
     {:pre [valfn]}
     (create-attribute! g props)
     (etg/set-values! g qname valfn)))

;;### Renaming

(defn- old-attr-idx-map
  "Returns a map with `aec` and all its subclasses as keys, and the indices of
  attribute `aname` as values."
  [^AttributedElementClass aec aname]
  (if (instance? GraphClass aec)
    {aec (.getAttributeIndex aec aname)}
    (reduce (fn [m ^GraphElementClass sub]
              (assoc m sub (.getAttributeIndex sub aname)))
            {}
            (cons aec (.getAllSubClasses ^GraphElementClass aec)))))

(defn rename-attribute!
  "Renames attribute `oldname` to `newname` in the schema of graph `g`.
  `oldname` is a fully qualified name (e.g., `contacts.Person.name`), and
  `newname` is just the new attribute name (e.g., `lastName`)"
  [g oldname newname]
  (let [[qn aname _] (u/split-qname oldname)
        aec ^AttributedElementClass (tg/attributed-element-class g qn)
        ^Attribute attribute (.getAttribute aec aname)]
    ;; Check that no subclass (or even this class) contains attribute `newname`
    (when (.containsAttribute aec (name newname))
      (u/errorf "%s already has a %s attribute." aec (name newname)))
    (when (instance? GraphElementClass aec)
      (doseq [^GraphElementClass sub (.getAllSubClasses ^GraphElementClass aec)]
        (when (.containsAttribute sub (name newname))
          (u/errorf "%s subclass %s already has a %s attribute."
                  aec sub (name newname)))))
    (let [old-idx-map (old-attr-idx-map aec aname)
          oaf (on-attributes-fn
               (fn [^AttributedElement ae ^objects ary]
                 (let [^AttributedElementClass klass (tg/attributed-element-class ae)
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

(defn delete-attribute!
  "Deletes `attr` (a fully qualified name, e.g., `contacts.Person.name`."
  [g attr]
  (let [[qn aname _] (u/split-qname attr)
        aec ^AttributedElementClass (tg/attributed-element-class g qn)
        ^Attribute attribute (.getAttribute aec aname)
        old-idx-map (old-attr-idx-map aec aname)
        oaf (on-attributes-fn
             (fn [^AttributedElement ae ^objects ary]
               (let [^AttributedElementClass klass (tg/attributed-element-class ae)
                     old-idx (old-idx-map klass)
                     new-ary (object-array (dec (alength ary)))]
                 (dotimes [i (alength new-ary)]
                   (aset new-ary i (aget ary (if (< i old-idx) i (inc i)))))
                 new-ary)))]
    (with-open-schema g
      (.delete attribute))
    (doseq [^InternalAttributesArrayAccess ae (element-seq g aec)]
      (.invokeOnAttributesArray ae oaf))))

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
                                   (u/errorf
                                    (str "%s tries to inherit different %s attributes. "
                                         "It already has one from %s and now gets another one from %s.")
                                    sub a (.getAttributedElementClass (.getAttribute sub a)) super))
       :else (u/errorf
              "%s tries to inherit %s with different domains: %s from %s and %s from %s"
              sub a
              (supmap a) (.getAttributedElementClass (.getAttribute sub a))
              (submap a) super)))
    ;; Return the seq of attributes that are really new for the subclass.  For
    ;; those, the attributes array has to be adjusted.
    (map #(.getAttribute super %1)
         (remove isect supkeys))))

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`."
  [g super & subs]
  (with-open-schema g
    (let [^GraphElementClass superaec (tg/attributed-element-class g super)
          subaecs (map #(tg/attributed-element-class g %1) subs)]
      (doseq [^GraphElementClass subaec subaecs]
        (let [new-atts (handle-attribute-clashes superaec subaec)]
          (if (isa? (class superaec) VertexClass)
            (do
              (.addSuperClass ^VertexClass subaec ^VertexClass superaec)
              (apply fix-attr-array-after-add! (tg/vseq g subaec)
                     new-atts))
            (do
              (.addSuperClass ^EdgeClass subaec ^EdgeClass superaec)
              (apply fix-attr-array-after-add! (tg/eseq g subaec)
                     new-atts))))))))

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`."
  [g sub & supers]
  (doseq [super supers]
    (add-sub-classes! g super sub)))

