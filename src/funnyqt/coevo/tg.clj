(ns funnyqt.coevo.tg
  "Co-Evolution transformations on TGraphs.
  Those are transformations which change the schema and the graph in-place."
  (:require clojure.set
            clojure.pprint
            [clojure.tools.macro :as m]
            [funnyqt.generic :as g]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.extensional :as e]
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

(defn ^:private get-aec [g aec]
  (if (tg/attributed-element-class? aec)
    aec
    (tg/attributed-element-class g aec)))

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
  "Creates a RecordDomain of the given `qname` and `comp-doms` in `g`.
  `qname` is a symbol denoting the qualified name of the new RecordDomain.
  `comp-doms` is a map from component name to domain name both given as
  symbols."
  [g qname comp-doms]
  (with-open-schema g
    (let [rd (.createRecordDomain ^Schema (tg/schema g) (name qname))]
      (doseq [[comp dom] comp-doms]
        (.addComponent rd (name comp) (tg/domain (tg/schema g) dom)))
      rd)))

(defn create-enum-domain!
  "Creates an EnumDomain with the given `qname` and `literals` in `g`.
  `qname` is a symbol denoting the qualified name of the new EnumDomain.
  `literals` is a seq of literal names given as symbols."
  [g qname literals]
  (with-open-schema g
    (let [ed (.createEnumDomain ^Schema (tg/schema g)
                                ^String (name qname)
                                ^java.util.List  (mapv name literals))])))

(defn rename-domain!
  "Renames the custem EnumDomain or RecordDomain `old-qname` in the schema of
  graph `g` to `new-qname`.  `old-qname` and `new-qname` are qualified names
  given as symbols.  A rename from 'MyEnum to 'pkg.MyEnum is a effectively a
  package move."
  [g old-qname new-qname]
  (let [dom (tg/domain g old-qname)]
    (with-open-schema g
      (.setQualifiedName dom (name new-qname)))))

;;## AttributedElementClasses

(defn rename-attributed-element-class!
  "Renames the AttributedElementClass denoted by `old-qname` in the schema of
  `g` to `new-qname`.  Both `old-qname` and `new-qname` are qualified names
  given as symbols thus renaming a class foo.Bar to quux.Bar is essentially a
  package move."
  [g old-qname new-qname]
  (let [aec (tg/attributed-element-class g old-qname)
        ;; The hash of aec will change, so we have to remove its entry and add
        ;; it back after the renaming has been performed.
        img-val  (when e/*img*
                   (let [img-val (@e/*img* aec)]
                     (swap! e/*img* dissoc aec)
                     img-val))
        arch-val (when e/*arch*
                   (let [arch-val (@e/*arch* aec)]
                     (swap! e/*arch* dissoc aec)
                     arch-val))]
    (with-open-schema g
      (.setQualifiedName aec (name new-qname)))
    ;; Re-add the mappings.
    (swap! e/*img*  assoc aec img-val)
    (swap! e/*arch* assoc aec arch-val)))

(defn delete-graph-element-class!
  "Deletes the GraphElementClass with qualified name `qname` in the schema of
  graph `g`.  All its subclasses will be deleted, too, and likewise all
  instances in the graph.  Additionally, the traceability information
  wrt. class and its subclasses is purged from `funnyqt.extensional/*arch*` and
  `funnyqt.extensional/*img*`."
  [g qname]
  (let [aec (get-aec g qname)]
    (if (or (tg/vertex-class? aec) (tg/edge-class? aec))
      (let [els (vec (element-seq g aec))
            all-subs (conj (seq (.getAllSubClasses ^GraphElementClass aec)) aec)]
        (with-open-schema g
          (.delete ^GraphElementClass aec))
        (when e/*arch*
          (doseq [sub all-subs]
            (swap! e/*arch* dissoc sub)))
        (when e/*img*
          (doseq [sub all-subs]
            (swap! e/*img* dissoc sub)))
        (doseq [el els]
          (g/delete! el)))
      (u/errorf "Don't know how to delete %s." aec))))

;;## VertexClasses
;;### Creating

(defn ^:private create-vc! [g qname abstract]
  (with-open-schema g
    (let [gc (.getGraphClass ^Schema (tg/schema g))
          vc (.createVertexClass gc (name qname))]
      (.setAbstract vc (boolean abstract))
      vc)))

(defn create-vertex-class!
  "Creates a new VertexClass with qualified name `qname` in the graph `g`s schema.
  Returns the newly created VertexClass.  If `archfn` is supplied, also creates
  vertices of the new VertexClass in `g`.  For more details on `archfn`, see
  `funnyqt.extensional.tg/create-vertices!`."
  ([g qname]
   {:pre [g qname]}
   (create-vc! g qname false))
  ([g qname archfn]
   {:pre [g qname (or (nil? archfn) (fn? archfn))]}
   (let [vc (create-vc! g qname false)]
     (etg/create-vertices! g qname archfn)
     vc)))

(defn create-abstract-vertex-class!
  "Creates a new abstract VertexClass with qualified name `qname` in the schema
  of graph `g`."
  [g qname]
  {:pre [g qname]}
  (create-vc! g qname true))

;;## EdgeClasses

;;### Creating

(defn ^:private create-ec!
  [^Graph g qname abstract from to {:keys [from-multis from-role from-kind
                                           to-multis   to-role   to-kind]
                                    :or {from-multis [0, Integer/MAX_VALUE]
                                         from-role ""
                                         from-kind AggregationKind/NONE
                                         to-multis [0, Integer/MAX_VALUE]
                                         to-role ""
                                         to-kind AggregationKind/NONE}
                                    :as props}]
  (when-let [unknown (seq (remove #{:from-multis :from-role :from-kind
                                    :to-multis   :to-role   :to-kind}
                                  (keys props)))]
    (u/errorf "Unknown property keys: %s" unknown))
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
  "Creates a new EdgeClass with the qualified name `qname` starting at
  VertexClass `from` and ending at VertexClass `to` in the schema of graph `g`.
  Returns the newly created EdgeClass.  `props` is a map with additional
  properties for the edge class.  If omitted, the default is:

  {:from-multis [0, Integer/MAX_VALUE]
  :from-role \"\"
  :from-kind AggregationKind/NONE
  :to-multis [0, Integer/MAX_VALUE]
  :to-role \"\"
  :to-kind AggregationKind/NONE}

  If an `archfn` is supplied, also creates edges in `g`.  For the details of
  `archfn`, see function `funnyqt.extensional.tg/create-edges!`."
  ([g qname from to]
   {:pre [qname from to]}
   (create-ec! g qname false from to {}))
  ([g qname from to props-or-archfn]
   {:pre [qname from to]}
   (if (map? props-or-archfn)
     (create-edge-class! g qname from to props-or-archfn nil)
     (create-edge-class! g qname from to {} props-or-archfn)))
  ([g qname from to props archfn]
   {:pre [g qname from to (or (nil? archfn) (fn? archfn))]}
   (let [ec (create-ec! g qname false from to props)]
     (when archfn
       (etg/create-edges! g qname archfn))
     ec)))

(defn create-abstract-edge-class!
  "Creates a new abstract EdgeClass with the qualified name `qname` starting at
  VertexClass `from` and ending at VertexClass `to` in the schema of graph `g`.
  Returns the newly created EdgeClass.  `props` is a map with additional
  properties for the edge class.  If omitted, the default is:

  {:from-multis [0, Integer/MAX_VALUE]
  :from-role \"\"
  :from-kind AggregationKind/NONE
  :to-multis [0, Integer/MAX_VALUE]
  :to-role \"\"
  :to-kind AggregationKind/NONE}"
  [g qname from to {:keys [from-multis from-role from-kind
                           to-multis   to-role   to-kind]
                    :or {from-multis [0, Integer/MAX_VALUE]
                         from-role ""
                         from-kind AggregationKind/NONE
                         to-multis [0, Integer/MAX_VALUE]
                         to-role ""
                         to-kind AggregationKind/NONE}
                    :as props}]
  (create-ec! g qname true from to props))

;;## Attributes

;;### Creating

(defn ^:private fix-attr-array-after-add!
  "Resizes the attributes array of all `aec` instances after adding new
  attributes.  `aec2new-attrs-map` is a map of the form

    {aec [new-attr1 ...]
     sub-aec [new-attr1 ...]
     ...}"
  [g aec aec2new-attrs-map]
  (let [elems (element-seq g aec)
        oaf (on-attributes-fn
             (fn [ae ^objects ary]
               (let [^AttributedElementClass aec (tg/attributed-element-class ae)
                     new-attrs (set (aec2new-attrs-map aec))
                     ^objects new-ary (make-array Object (.getAttributeCount aec))]
                 (when (not= (alength new-ary)
                             (+ (count new-attrs) (if ary (alength ary) 0)))
                   (u/errorf "New attr-ary length %s but %s was expected for AEC %s."
                             (alength new-ary) (- (if ary (alength ary) 0) (count new-attrs)) aec))
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

(defn ^:private fix-attr-array-after-del!
  "Resizes the attributes array of all `aec` instances after deleting
  attributes.  `aec2obs-attrs-map` is a map of the form

    {aec [obsolete-attr1 ...]
     sub-aec [obsolete-attr1 ...]
     ...}"
  [g aec aec2obs-attrs-map]
  (let [elems (element-seq g aec)
        oaf (on-attributes-fn
             (fn [ae ^objects ary]
               (let [^AttributedElementClass aec (tg/attributed-element-class ae)
                     obs-attrs (set (aec2obs-attrs-map aec))
                     ^objects new-ary (make-array Object (.getAttributeCount aec))]
                 (when (not= (alength new-ary)
                             (- (if ary (alength ary) 0) (count obs-attrs)))
                   (u/errorf "New attr-ary length %s but %s was expected for AEC %s."
                             (alength new-ary) (- (if ary (alength ary) 0) (count obs-attrs)) aec))
                 (loop [atts (.getAttributeList aec), posdec 0]
                   (if (seq atts)
                     (let [^Attribute a (first atts)
                           idx (.getAttributeIndex aec (.getName a))]
                       (if (obs-attrs a)
                         (recur (rest atts) (inc posdec))
                         (do
                           (aset new-ary idx (aget ary (+ idx posdec)))
                           (recur (rest atts) posdec))))
                     new-ary)))))]
    (doseq [^InternalAttributesArrayAccess e elems]
      (.invokeOnAttributesArray e oaf))))

(defn ^:private create-attr!
  [g aec attr domain default]
  (let [aec ^AttributedElementClass (tg/attributed-element-class g aec)
        check (fn [^AttributedElementClass aec]
                (when (.getAttribute aec (name attr))
                  (u/errorf "%s already has a %s attribute" aec attr)))
        aec-and-subs (if (tg/graph-class? aec)
                       [aec]
                       (cons aec (seq (.getAllSubClasses ^GraphElementClass aec))))]
    ;; Check that there's no such attribute yet
    (doseq [sub aec-and-subs]
      (check sub))
    (with-open-schema g
      (let [attr (.createAttribute aec (name attr) (tg/domain g domain) default)]
        (fix-attr-array-after-add! g aec (zipmap aec-and-subs (repeat #{attr})))
        attr))))

(defn create-attribute!
  "Creates an new Attribute `attr` for AttributedElementClass `aec` in the
  schema of graph `g`.  Returns the newly created Attribute.  `aec` is a symbol
  denoting the qualified name of the attributed element class, `attr` is a
  keyword denoting the new attribute's name.  If a `valfn` is supplied, also
  sets the `attr` value of instances.  For details on `valfn`, see
  `funnyqt.extensional.tg/set-values!`."
  ([g aec attr domain]
   {:pre [g aec attr domain]}
   (create-attr! g aec attr domain nil))
  ([g aec attr domain default-or-valfn]
   {:pre [g aec attr domain]}
   (if (fn? default-or-valfn)
     (create-attribute! g aec attr domain nil default-or-valfn)
     (create-attribute! g aec attr domain default-or-valfn nil)))
  ([g aec attr domain default valfn]
   {:pre [g aec attr domain]}
   (let [at (create-attr! g aec attr domain default)]
     (when valfn
       (etg/set-values! g aec attr valfn))
     at)))

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
  "Renames attribute `oldname` declared for AttributedElementClass `aec` to
  `newname` in the schema of graph `g`.  `aec` is a symbol denoting the
  qualified name of the attributed element class, `oldname` and `newname` are
  keywords denoting the attribute names."
  [g aec oldname newname]
  (let [aec ^AttributedElementClass (tg/attributed-element-class g aec)
        ^Attribute attribute (.getAttribute aec (name oldname))]
    (when-not (.getOwnAttribute aec (name oldname))
      (u/errorf "Cannot rename attribute %s for class %s because it's owned by %s"
                oldname aec (.getAttributedElementClass attribute)))
    ;; Check that no subclass (or even this class) contains attribute `newname`
    (when (.containsAttribute aec (name newname))
      (u/errorf "%s already has a %s attribute" aec newname))
    (when (tg/graph-element-class? aec)
      (doseq [^GraphElementClass sub (.getAllSubClasses ^GraphElementClass aec)]
        (when (.containsAttribute sub (name newname))
          (u/errorf "%s subclass %s already has a %s attribute"
                    aec sub newname))))
    (let [old-idx-map (old-attr-idx-map aec (name oldname))
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

;;### Deleting

(defn delete-attribute!
  "Deletes AttributedElementClass `aec`s `attr` Attribute."
  [g aec attr]
  (let [aec ^AttributedElementClass (tg/attributed-element-class g aec)
        ^Attribute attribute (.getAttribute aec (name attr))
        old-idx-map (old-attr-idx-map aec (name attr))
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

(defn ^:private determine-new-attributes-before-adding-specialization
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
                    sub (keyword a))
          ;;---
          :else (u/errorf
                 "%s tries to inherit two different %s attributes, one from %s and one from %s"
                 sub (keyword a)
                 (.getAttributedElementClass (.getAttribute sub a))
                 (.getAttributedElementClass (.getAttribute super a)))))
      ;; Ok, add the new attrs of sub to the map if there are new attrs.
      (when (seq new-attrs)
        (swap! gec2new-attrs-map assoc sub (map (fn [new-attr-name]
                                                  (.getAttribute super new-attr-name))
                                                new-attrs))))
    @gec2new-attrs-map))

(defn ^:private determine-obsolete-attributes-after-removing-specialization
  [^GraphElementClass super all-subs]
  (let [supermap (attr-name-dom-map super)
        gec2obs-attrs-map (atom {})]
    (doseq [^GraphElementClass sub all-subs
            :let [submap (attr-name-dom-map sub)
                  obs-attrs (clojure.set/difference (set (keys supermap))
                                                    (set (keys submap)))]]
      (when (seq obs-attrs)
        (swap! gec2obs-attrs-map assoc sub (map (fn [obs-attr-name]
                                                  (.getAttribute super obs-attr-name))
                                                obs-attrs))))
    @gec2obs-attrs-map))

(defn create-specialization!
  "Creates a specialization between Vertex- or EdgeClasses `super` and `sub`.
  Returns `super` again."
  [g super sub]
  (let [^GraphElementClass super-gec (get-aec g super)
        ^GraphElementClass sub-gec (get-aec g sub)
        gec2new-attrs-map (determine-new-attributes-before-adding-specialization
                           super-gec sub-gec)]
    (when-let [dups (seq (clojure.set/intersection
                          (set (map (partial e/archetype super-gec)
                                    (element-seq g super-gec)))
                          (set (map (partial e/archetype sub-gec)
                                    (element-seq g sub-gec)))))]
      (u/errorf "Bijectivity violation: can't make %s subclass of %s because their sets of archetypes are not disjoint. Common archetypes: %s"
                sub super dups))
    (if (instance? VertexClass super-gec)
      (do
        (with-open-schema g
          (.addSuperClass ^VertexClass sub-gec ^VertexClass super-gec))
        (when (seq gec2new-attrs-map)
          (fix-attr-array-after-add! g sub-gec gec2new-attrs-map)))
      (do
        ;; With edge classes, sub may only become a subclass of super if its
        ;; source/target vertex classes are also subclasses of super's
        ;; source/target vertex classes.
        (let [super-start  (g/mm-relationship-class-source super-gec)
              super-target (g/mm-relationship-class-target super-gec)
              sub-start  (g/mm-relationship-class-source sub-gec)
              sub-target (g/mm-relationship-class-target sub-gec)]
          (when-not (or (g/mm-superclass? super-start sub-start)
                        (= super-start sub-start))
            (u/errorf
             "Can't make %s subclass of %s because %s's source element class %s is no subclass of or equal to %s's source element class %s."
             sub super sub (g/qname sub-start) super (g/qname super-start)))
          (when-not (or (g/mm-superclass? super-target sub-target)
                        (= super-target sub-target))
            (u/errorf
             "Can't make %s subclass of %s because %s's target element class %s is no subclass of or equal to %s's target element class %s."
             sub super sub (g/qname sub-target) super (g/qname super-target))))
        (with-open-schema g
          (.addSuperClass ^EdgeClass sub-gec ^EdgeClass super-gec))
        (when (seq gec2new-attrs-map)
          (fix-attr-array-after-add! g sub-gec gec2new-attrs-map)))))
  super)

(defn delete-specialization!
  "Deletes the specialization between Vertex/EdgeClasses `super` and `sub`.
  All attributes inherited from `super` to `sub` and its subclasses are deleted."
  [g super sub]
  (let [^GraphElementClass super-gec (get-aec g super)
        ^GraphElementClass sub-gec (get-aec g sub)
        all-subs (conj (seq (.getAllSubClasses sub-gec)) sub-gec)]
    ;; The schema compatibility (esp. when deleting a specialization between
    ;; vertex classes) is already checked by JGraLab.
    (with-open-schema g
      (if (instance? VertexClass super-gec)
        (.removeSuperClass ^VertexClass sub-gec ^VertexClass super-gec)
        (.removeSuperClass ^EdgeClass sub-gec ^EdgeClass super-gec)))
    (fix-attr-array-after-del! g sub-gec (determine-obsolete-attributes-after-removing-specialization
                                          super-gec all-subs))))
