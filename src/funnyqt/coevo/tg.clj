(ns funnyqt.coevo.tg
  "Co-Evolution transformations on TGraphs.
  Those are transformations which simultaneously change the schema and the
  graph in-place."
  (:require [clojure pprint set]
            [funnyqt
             [extensional :as e]
             [generic :as g]
             [tg :as tg]
             [utils :as u]])
  (:import [de.uni_koblenz.jgralab AttributedElement Graph GraphIO]
           [de.uni_koblenz.jgralab.impl.generic InternalAttributesArrayAccess
            InternalAttributesArrayAccess$OnAttributesFunction]
           [de.uni_koblenz.jgralab.schema AggregationKind Attribute
            AttributedElementClass EdgeClass GraphClass GraphElementClass
            IncidenceClass RecordDomain Schema VertexClass EnumDomain RecordDomain]
           de.uni_koblenz.jgralab.schema.impl.SchemaImpl))

;;# Utility functions

(defn ^:private on-attributes-fn
  "`f` : AttributedElement x Object[] -> Object[]"
  [f]
  (reify InternalAttributesArrayAccess$OnAttributesFunction
    (invoke [_ ae ary]
      (f ae ary))))

(defn ^:private get-aec ^AttributedElementClass [g aec]
  (if (tg/attributed-element-class? aec)
    aec
    (tg/attributed-element-class g aec)))

(defn ^:private element-seq
  ([g aec]
   (element-seq g aec false))
  ([g aec only-direct-instances]
   (let [aec (get-aec g aec)
         ts (g/qname aec)
         ts (if only-direct-instances
              (symbol (str ts "!"))
              ts)]
     (cond
       (tg/graph-class? aec)  [g]
       (tg/vertex-class? aec) (tg/vseq g ts)
       (tg/edge-class? aec)   (tg/eseq g ts)
       :else (u/errorf "Cannot handle %s" aec)))))

(defmacro ^:private with-open-schema [g & body]
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

(defn empty-graph
  "Creates an empty graph conforming to an empty schema.
The schema's qualified name is `sqname`, and it just contains the graph class
with name `gcname`."
  [sqname gcname]
  (let [^Schema s (create-schema sqname gcname)]
    (.finish s)
    (tg/new-graph s)))

;;## Creating, renaming, and deleting Enum & Record domains

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

(defn delete-domain!
  "Deletes the Enum- or RecordDomain with the given `qname` from `g`s schema.
  Throws an exception if the domain is still in use by some attribute."
  [g qname]
  (let [dom (tg/domain g qname)]
    (when-not (or (instance? EnumDomain dom)
                  (instance? RecordDomain dom))
      (u/errorf "Can only delete Enum- and RecordDomains but not %s." dom))
    (let [attrs (.getAttributes dom)]
      (when (seq attrs)
        (u/errorf (str "Cannot delete domain %s because these attributes"
                       " still have this type: %s")
                  dom attrs)))
    (with-open-schema g
      (.delete dom))))

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
        img-val  (when (bound? #'e/*img*)
                   (let [img-val (@e/*img* aec)]
                     (swap! e/*img* dissoc aec)
                     img-val))
        arch-val (when (bound? #'e/*arch*)
                   (let [arch-val (@e/*arch* aec)]
                     (swap! e/*arch* dissoc aec)
                     arch-val))]
    (with-open-schema g
      (.setQualifiedName aec (name new-qname)))
    ;; Re-add the mappings.
    (when (bound? #'e/*img*)
      (swap! e/*img*  assoc aec img-val)
      (swap! e/*arch* assoc aec arch-val))))

;;## GraphElementClasses

(defn delete-graph-element-class!
  "Deletes the GraphElementClass with qualified name `qname` in the schema of
  graph `g`.  VertexClasses can only be deleted if there are no incident
  EdgeClasses anymore.  All its subclasses will be deleted, too, and likewise
  all instances in the graph.  Additionally, the traceability information
  wrt. class and its subclasses is purged from `funnyqt.extensional/*arch*` and
  `funnyqt.extensional/*img*`."
  [g qname]
  (let [aec (get-aec g qname)]
    (if (or (tg/vertex-class? aec) (tg/edge-class? aec))
      (let [els (vec (element-seq g aec))
            all-subs (conj (seq (.getAllSubClasses ^GraphElementClass aec)) aec)]
        ;; JGraLab already takes care that one cannot delete a VC which still
        ;; has connected ECs.
        (with-open-schema g
          (.delete ^GraphElementClass aec))
        (when (bound? #'e/*arch*)
          (doseq [sub all-subs]
            (swap! e/*arch* dissoc sub)
            (swap! e/*img* dissoc sub)))
        (doseq [el els]
          (g/delete! el)))
      (u/errorf "Don't know how to delete %s." aec))))

(defn set-abstract!
  "Sets the abstract property of the GraphElementClass `gec-qname` to `val` in
  the schema of graph `g`.  If `val` is true, no direct instances of the graph
  element class may exist in the graph or an exception is thrown."
  [g gec-qname val]
  (let [^GraphElementClass gec (get-aec g gec-qname)]
    (if val
      (do
        ;; Check that there are no instances
        (when (seq (element-seq g gec true))
          (u/errorf "Can't make class %s abstract because there are still direct instances."
                    gec))
        (.setAbstract gec true))
      (.setAbstract gec false))))

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
  `funnyqt.extensional/create-elements!`."
  ([g qname]
   {:pre [g qname]}
   (create-vc! g qname false))
  ([g qname archfn]
   {:pre [g qname (or (nil? archfn) (fn? archfn))]}
   (let [vc (create-vc! g qname false)]
     (e/create-elements! g qname archfn)
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
  properties for the edge class' incidence classes.  If omitted, the default
  is:

  {:from-multis [0, Integer/MAX_VALUE]
   :from-role \"\"
   :from-kind AggregationKind/NONE
   :to-multis [0, Integer/MAX_VALUE]
   :to-role \"\"
   :to-kind AggregationKind/NONE}

  If an `archfn` is supplied, also creates edges in `g`.  For the details of
  `archfn`, see function `funnyqt.extensional/create-relationships!`."
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
       (e/create-relationships! g qname archfn))
     ec)))

(defn create-abstract-edge-class!
  "Creates a new abstract EdgeClass with the qualified name `qname` starting at
  VertexClass `from` and ending at VertexClass `to` in the schema of graph `g`.
  Returns the newly created EdgeClass.  `props` is a map with additional
  properties for the edge class' incidence classes.  If omitted, the default
  is:

  {:from-multis [0, Integer/MAX_VALUE]
   :from-role \"\"
   :from-kind AggregationKind/NONE
   :to-multis [0, Integer/MAX_VALUE]
   :to-role \"\"
   :to-kind AggregationKind/NONE}"
  ([g qname from to]
   (create-abstract-edge-class! g qname from to {}))
  ([g qname from to {:keys [from-multis from-role from-kind
                            to-multis   to-role   to-kind]
                     :or {from-multis [0, Integer/MAX_VALUE]
                          from-role ""
                          from-kind AggregationKind/NONE
                          to-multis [0, Integer/MAX_VALUE]
                          to-role ""
                          to-kind AggregationKind/NONE}
                     :as props}]
   (create-ec! g qname true from to props)))

(defn set-incidence-class-props!
  "In the schema of graph `g`, sets the properties of the IncidenceClasses of
  the EdgeClass `ec-qname` according to `props`.  `props` is a map with the
  properties for the edge class' incidence classes.  It has the form:

  {:from-multis [0, Integer/MAX_VALUE]
   :from-role \"\"
   :from-kind AggregationKind/NONE
   :to-multis [0, Integer/MAX_VALUE]
   :to-role \"\"
   :to-kind AggregationKind/NONE}"
  [g ec-qname props]
  (let [^EdgeClass ec (get-aec g ec-qname)
        ^IncidenceClass from (.getFrom ec)
        ^IncidenceClass to (.getTo ec)]
    (with-open-schema g
      (when-let [[fmin fmax] (:from-multis props)]
        (.setMin from fmin)
        (.setMax from fmax))
      (when-let [frole (:from-role props)]
        (.setRolename from (name frole)))
      (when-let [[tmin tmax] (:to-multis props)]
        (.setMin to tmin)
        (.setMax to tmax))
      (when-let [trole (:to-role props)]
        (.setRolename to (name trole)))
      (let [^AggregationKind fak (:from-kind props)
            ^AggregationKind tak (:to-kind props)]
        ;; In order to be able to swap the aggregation kinds of an EC, the
        ;; setting to NONE must happen first.
        (if (= fak AggregationKind/NONE)
          (do (.setAggregationKind from fak)
              (when tak (.setAggregationKind to tak)))
          (if (= tak AggregationKind/NONE)
            (do (.setAggregationKind to tak)
                (when fak (.setAggregationKind from fak)))
            ;; Else, probably only fak or tak has been given.
            (do
              (when fak (.setAggregationKind from fak))
              (when tak (.setAggregationKind to tak)))))))))

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
      (doseq [^Attribute a new-attrs
              :when (.getDefaultValueAsString a)]
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
  (let [domain (tg/domain g domain)
        gio (GraphIO/createStringWriter (tg/schema g))
        default-tg (when default
                     (do (.serializeGenericAttribute domain gio default)
                         (.getStringWriterResult gio)))
        aec ^AttributedElementClass (tg/attributed-element-class g aec)
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
      (let [attr (.createAttribute aec (name attr) domain default-tg)]
        (fix-attr-array-after-add! g aec (zipmap aec-and-subs (repeat #{attr})))
        attr))))

(defn create-attribute!
  "Creates an new Attribute `attr` for AttributedElementClass `aec` in the
  schema of graph `g`.  Returns the newly created Attribute.  `aec` is a symbol
  denoting the qualified name of the attributed element class, `attr` is a
  keyword denoting the new attribute's name.  If a `valfn` is supplied, also
  sets the `attr` value of instances.  For details on `valfn`, see
  `funnyqt.extensional/set-avals!`."
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
       (e/set-avals! g aec attr valfn))
     at)))

(defn pull-up-attribute!
  "Pulls up `attr` into `super` from all subclasses of `super`.
  Throws if not all subclasses declare `attr` or the declarations have no
  unique domain and default value."
  [g super attr]
  ;; TODO: We must fix the attr array of direct instances of super!
  (let [^GraphElementClass super-gec (get-aec g super)
        sub-gecs  (.getDirectSubClasses super-gec)
        _ (when-not (seq sub-gecs)
            (u/errorf "Can't pull up %s; %s has no subclasses!" attr super))
        ^Attribute a (.getAttribute ^GraphElementClass (first sub-gecs) (name attr))
        dom (.getDomain a)
        def (.getDefaultValueAsString a)
        err-subs (remove (fn [^GraphElementClass sub]
                           (when-let [a (.getAttribute sub (name attr))]
                             (and (= dom (.getDomain a))
                                  (= def (.getDefaultValueAsString a)))))
                         sub-gecs)]
    (when (seq err-subs)
      (u/errorf "Cannot pull up attribute %s into %s because the subclasses %s don't have it at all or define it with a different domain/default value!"
                attr super err-subs))
    (with-open-schema g
      (doseq [^GraphElementClass sub sub-gecs]
        (-> sub
            (.getAttribute (name attr))
            .delete))
      (.createAttribute super-gec (name attr) dom def))))

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
  [g aec-qname oldname newname]
  (let [aec (tg/attributed-element-class g aec-qname)
        ^Attribute attribute (.getAttribute aec (name oldname))]
    (when-not attribute
      (u/errorf "No such attribute %s at attributed element class %s."
                oldname aec))
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
    (when-let [dups (when (bound? #'e/*img*)
                      (seq (clojure.set/intersection
                            (into #{} (comp (map (partial e/archetype super-gec))
                                            (remove nil?))
                                  (element-seq g super-gec))
                            (into #{} (comp (map (partial e/archetype sub-gec))
                                            (remove nil?))
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
  The values of properties inherited from `super` to `sub` and its subclasses
  are removed on the instance level."
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
    (fix-attr-array-after-del!
     g sub-gec (determine-obsolete-attributes-after-removing-specialization
                super-gec all-subs))))

(defn downtype!
  "Retypes direct instances of super-graph element class `super` for which
  `predicate` holds to instances of subclass `sub`.  Concretely, for each
  `super` instance a corresponding `sub` instance is created, and all attribute
  values are copied over.  When retyping between a vertex classes, also the
  edges incident to the selected `super` instances are relinked to the new
  `sub` instance.

  Also the traceability mappings are adapted accordingly, i.e., archetypes of
  `super` instances become instances of `sub` instances."
  [g super sub predicate]
  (let [els       (into [] (filter predicate) (element-seq g super true))
        super-gec (get-aec g super)
        sub-gec   (get-aec g sub)]
    (when-not (g/mm-superclass? super-gec sub-gec)
      (u/errorf "%s is no superclass of %s so can't downtype!" super sub))
    (when (seq els)
      (let [[nimg narch]
            (e/with-trace-mappings
              (cond
                (tg/vertex? (first els))
                (e/create-elements! g sub (fn [] els))
                ;;---
                (tg/edge? (first els))
                (e/create-relationships!
                 g sub (fn []
                         (map (fn [edge]
                                [edge (tg/alpha edge) (tg/omega edge)])
                              els))))
              (doseq [attr (map #(.getName ^Attribute %) (.getAttributeList super-gec))]
                (e/set-avals! g super attr
                              (fn []
                                (map (fn [el]
                                       [(e/element-image el) (tg/value el attr)])
                                     els))))
              (when (tg/vertex? (first els))
                (doseq [el els]
                  (tg/relink! el (e/image g sub el)))))]
        ;; Fix mappings so that archetypes for old super instances are now
        ;; archetypes for the retyped sub instances.
        (when (bound? #'e/*img*)
          (let [old-archs (into [] (filter (fn [[oel oarch]]
                                             (get-in nimg [sub-gec oel])))
                                (e/archetype-map super-gec))]
            (doseq [[oel oarch] old-archs]
              (swap! e/*arch* update-in [super-gec] dissoc oel)
              (swap! e/*arch* update-in [sub-gec] assoc ((nimg sub-gec) oel) oarch)
              (swap! e/*img* update-in [super-gec] dissoc oarch)
              (swap! e/*img* update-in [sub-gec] assoc oarch ((nimg sub-gec) oel)))))
        ;; Finally, delete the old elements!
        (g/delete! els true)))))
