(ns funnyqt.metatransform.tg
  "Schema-creating transformations on TGraphs."
  (:require clojure.set
            clojure.pprint
            [clojure.tools.macro :as m]
            [funnyqt.generic :as g]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]
            [funnyqt.query :as q]
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

;;# Utility functions

(defn ^:private on-attributes-fn
  "`f` : AttributedElement x Object[] -> Object[]"
  [f]
  (reify InternalAttributesArrayAccess$OnAttributesFunction
    (invoke [_ ae ary]
      (f ae ary))))

(defn ^:private element-seq
  [g aec]
  (let [ts (g/qname aec)]
    (cond
     (tg/graph-class? aec)  [g]
     (tg/vertex-class? aec) (tg/vseq g ts)
     (tg/edge-class? aec)   (tg/eseq g ts)
     :else (u/errorf "Cannot handle %s" aec))))

(defmacro with-open-schema [g & body]
  `(let [g# ~g
         ^Schema s# (tg/schema g#)
         was-opened# (.reopen s#)
         r# (do ~@body)]
     (when was-opened#
       (.finish s#)
       (tg/reset-all-tg-caches))
     r#))

;;# Schema functions

;;## Create a new schema & empty graph

(defn ^:private create-schema [sqname gcname]
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

(defn ^:private create-vc!
  [g {:keys [qname abstract]}]
  (with-open-schema g
    (let [gc (.getGraphClass ^Schema (tg/schema g))
          vc (.createVertexClass gc (name qname))]
      (.setAbstract vc (boolean abstract))
      vc)))

(defn create-vertex-class!
  "Creates VertexClass + instances in `g`.
  Returns the newly created VertexClass.
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
     (let [vc (create-vertex-class! g props)]
       (etg/create-vertices! g qname archs)
       vc)))

;;## EdgeClasses

;;### Creating

(defn ^:private get-aec [g aec]
  (if (tg/attributed-element-class? aec)
    aec
    (tg/attributed-element-class g aec)))

(defn ^:private create-ec!
  [^Graph g {:keys [qname abstract
                    from from-multis from-role from-kind
                    to to-multis to-role to-kind]}]
  (let [source-vc (get-aec g from)
        target-vc (get-aec g to)]
    (with-open-schema g
      (let [gc (.getGraphClass ^Schema (tg/schema g))
            ec (.createEdgeClass gc (name qname)
                                 source-vc (first from-multis)
                                 (second from-multis) (name from-role) from-kind
                                 target-vc (first to-multis)
                                 (second to-multis) (name to-role) to-kind)]
        (.setAbstract ec (boolean abstract))
        ec))))

(defn create-edge-class!
  "Creates an EdgeClass + instances.
  Returns the newly created EdgeClass.
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
     (let [ec (create-edge-class! g props)]
       (etg/create-edges! g qname archs)
       ec)))

;;## Attributes

;;### Creating

(defn ^:private fix-attr-array-after-add!
  "Resizes the attributes array of all `elems` after adding the `new-attrs` if
  needed."
  [g aec aec2new-attrs-map]
  (let [elems (element-seq g aec)
        oaf (on-attributes-fn
             (fn [ae ^objects ary]
               (let [^AttributedElementClass aec (tg/attributed-element-class ae)
                     new-attrs (set (aec2new-attrs-map aec))
                     ^objects new-ary (make-array Object (.getAttributeCount aec))]
                 (when (not= (alength new-ary)
                             (+ (count new-attrs) (if ary (alength ary) 0)))
                   (u/errorf "Something's wrong!"))
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
    (doseq [^InternalAttributesArrayAccess e elems
            :let [new-attrs (aec2new-attrs-map (tg/attributed-element-class e))]]
      (.invokeOnAttributesArray e oaf)
      (doseq [^Attribute a new-attrs]
        (.setDefaultValue a e)))))

(defn ^:private create-attr!
  [g {:keys [qname domain default]}]
  (let [[qn aname _] (u/split-qname qname)
        aec          ^AttributedElementClass (tg/attributed-element-class g qn)
        check (fn [^AttributedElementClass aec]
                (when (.getAttribute aec aname)
                  (u/errorf "%s already has a %s attribute" aec aname)))]
    ;; Check that there's no such attribute yet
    (if (tg/graph-class? aec)
      (check aec)
      (doseq [sub (cons aec (seq (.getAllSubClasses ^GraphElementClass aec)))]
        (check sub)))
    (with-open-schema g
      (let [attr (.createAttribute aec aname (tg/domain g domain) default)]
        (fix-attr-array-after-add! g aec {aec #{attr}})
        attr))))

(defn create-attribute!
  "Creates an Attribute and sets values.
  Returns the newly created Attribute.
  The map given as first argument determines the schema properties.
  For `valfn`, see `etg/set-values!`."
  ([g {:keys [qname domain default] :as props}]
     {:pre [qname domain]}
     (create-attr! g props))
  ([g {:keys [qname domain default] :as props} valfn]
     {:pre [valfn]}
     (let [attr (create-attribute! g props)]
       (etg/set-values! g qname valfn)
       attr)))

;;### Renaming

(defn ^:private old-attr-idx-map
  "Returns a map with `aec` and all its subclasses as keys, and the indices of
  attribute `aname` as values."
  [^AttributedElementClass aec aname]
  (if (tg/graph-class? aec)
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
    (when-not (.getOwnAttribute aec aname)
      (u/errorf "Cannot rename attribute %s for class %s because it's owned by %s"
                aname aec (.getAttributedElementClass attribute)))
    ;; Check that no subclass (or even this class) contains attribute `newname`
    (when (.containsAttribute aec (name newname))
      (u/errorf "%s already has a %s attribute" aec (name newname)))
    (when (tg/graph-element-class? aec)
      (doseq [^GraphElementClass sub (.getAllSubClasses ^GraphElementClass aec)]
        (when (.containsAttribute sub (name newname))
          (u/errorf "%s subclass %s already has a %s attribute"
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

(defn ^:private attr-name-dom-map
  [^AttributedElementClass aec]
  (apply hash-map (mapcat (fn [^Attribute a]
                            [(.getName a) (.getDomain a)])
                          (.getAttributeList aec))))

(defn ^:private determine-new-attributes
  [^GraphElementClass super ^GraphElementClass sub]
  (let [supermap (attr-name-dom-map super)
        ;; We need to check for clashes also in sub's subclasses.
        all-subs (conj (seq (.getAllSubClasses sub)) sub)
        gec2new-attrs-map (atom {})]
    (doseq [^GraphElementClass sub all-subs
            :let [submap (attr-name-dom-map sub)
                  isect  (clojure.set/intersection (set (keys supermap))
                                                   (set (keys submap)))
                  new-attrs (clojure.set/difference (set (keys supermap))
                                                    (set (keys submap)))]]
      (doseq [a isect]
        (cond
         ;; The same attribute is inherited (maybe via different paths).
         ;; That's ok.
         (identical? (.getAttribute super a) (.getAttribute sub a))
         :this-is-ok
         ;;---
         (.getOwnAttribute sub a)
         (u/errorf "%s already has a %s attribute so cannot inherit another one"
                   sub a)
         ;;---
         :else (u/errorf
                "%s tries to inherit two different %s attributes, one from %s and one from %s"
                sub a
                (.getAttributedElementClass (.getAttribute sub a))
                (.getAttributedElementClass (.getAttribute super a)))))
      ;; Ok, add the new attrs of sub to the map if there are new attrs.
      (when (seq new-attrs)
        (swap! gec2new-attrs-map assoc sub (map (fn [new-attr-name]
                                                  (.getAttribute super new-attr-name))
                                                new-attrs))))
    @gec2new-attrs-map))

(defn add-sub-classes!
  "Makes all `subs` sub-classes of `super`.
  Returns `super` again."
  [g super & subs]
  (let [^GraphElementClass superaec (get-aec g super)
        subaecs (map #(get-aec g %1) subs)]
    (doseq [^GraphElementClass subaec subaecs]
      (let [gec2new-attrs-map (determine-new-attributes superaec subaec)]
        (if (instance? VertexClass superaec)
          (do
            (with-open-schema g
              (.addSuperClass ^VertexClass subaec ^VertexClass superaec))
            (when (seq gec2new-attrs-map)
              (fix-attr-array-after-add! g subaec gec2new-attrs-map)))
          (do
            (with-open-schema g
              (.addSuperClass ^EdgeClass subaec ^EdgeClass superaec))
            (when (seq gec2new-attrs-map)
              (fix-attr-array-after-add! g subaec gec2new-attrs-map)))))))
  super)

(defn add-super-classes!
  "Makes all `supers` super-classes of `sub`.
  Returns `sub` again."
  [g sub & supers]
  (doseq [super supers]
    (add-sub-classes! g super sub)
    sub))

