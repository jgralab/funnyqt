(ns funnyqt.relational.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.util)
  (:require [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]
            funnyqt.query.tg
            funnyqt.query
            [funnyqt.utils :as u]
            [funnyqt.relational :as rel]
            [funnyqt.relational.tmp-elem :as tmp]
            clojure.java.io)
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass IncidenceClass)))

(defn ^:private tmp-typeo
  [g e t]
  (fn [a]
    (let [ge (walk a e)
          gt (walk a t)]
      ;; We assume gt is a ground symbol.  We may assume that since bidi
      ;; transformations usually should only use the generated +Foo relations.
      (when-not (ground? gt)
        (u/error "The type given to tmp-typeo must be ground."))
      (let [aec (tg/attributed-element-class g gt)
            vc? (tg/vertex-class? aec)]
        (if (ground? ge)
          (if (or (and (tmp/tmp-element? ge)
                       (tmp/set-type ge gt)
                       (tmp/set-kind ge (if vc? :vertex :edge)))
                  (p/has-type? ge gt))
            (succeed a)
            (fail a))
          (let [[sfn tefn] (if vc?
                             [(partial tg/vseq g gt) (partial tmp/make-tmp-vertex g gt)]
                             [(partial tg/eseq g gt) (partial tmp/make-tmp-edge g gt)])]
            (to-stream
             (->> (map #(unify a e %) (concat (sfn) [(tefn)]))
                  (remove not)))))))))

(defn typeo
  "A relation where in graph `g`, vertex or edge `e` has the type `t`, a graph
  element class name.  In fact, `t` may be any type specification (see
  `type-matcher`).  The graph `g` must be ground."
  [g e t]
  (if tmp/*make-tmp-elements*
    (tmp-typeo g e t)
    (fn [a]
      (let [ge (walk a e)
            gt (walk a t)]
        (cond
         (and (ground? ge)
              (ground? gt))
         (if (and (tg/attributed-element? ge)
                  (or (coll? gt) (symbol? gt))
                  (p/has-type? ge gt))
           (succeed a)
           (fail a))

         (ground? ge)
         (or (and (tg/attributed-element? ge)
                  (unify a t (p/qname ge)))
             (fail a))

         (ground? gt)
         (if (symbol? gt)
           ;; Ok, here we can determine if its a vertex or an edge class
           (let [[_ tn _] (u/type-with-modifiers (name gt))
                 aec      (tg/attributed-element-class g tn)]
             (if (tg/vertex-class? aec)
               (to-stream
                (->> (map #(unify a e %) (tg/vseq g gt))
                     (remove not)))
               (to-stream
                (->> (map #(unify a e %) (tg/eseq g gt))
                     (remove not)))))
           (to-stream
            (->> (map #(unify a e %)
                      (concat (tg/vseq g gt)
                              (tg/eseq g gt)))
                 (remove not))))

         :else (to-stream
                (->> (for [elem (concat (tg/vseq g) (tg/eseq g))]
                       (unify a [e t] [elem (p/qname elem)]))
                     (remove not))))))))

(defn ^:private tmp-vertexo [g v]
  (fn [a]
    (let [gv (walk a v)]
      (if (fresh? gv)
        (to-stream
         (->> (map #(unify a v %) (concat (tg/vseq g)
                                          [(tmp/make-tmp-vertex g)]))
              (remove not)))
        (if (or (and (tg/vertex? gv)
                     (tg/contains-vertex? g gv))
                (and (tmp/tmp-element? gv)
                     (tmp/set-kind gv :vertex)))
          (succeed a)
          (fail a))))))

(defn vertexo
  "A relation where `v` is a vertex in graph `g`.
  `g` has to be ground."
  [g v]
  (if tmp/*make-tmp-elements*
    (tmp-vertexo g v)
    (fn [a]
      (let [gv (walk a v)]
        (if (fresh? gv)
          (to-stream
           (->> (map #(unify a v %) (tg/vseq g))
                (remove not)))
          (if (and (tg/vertex? gv)
                   (tg/contains-vertex? g gv))
            (succeed a)
            (fail a)))))))

(defn ^:private tmp-edgeo [g e al om]
  (fn [a]
    (let [ge  (walk a e)
          gal (walk a al)
          gom (walk a om)]
      (if (fresh? ge)
        ;; If the edge is fresh, it might be an existing edge or a new tmp edge.
        (to-stream
         (->> (map (fn [[ed alp ome]]
                     ;; ed is always an edge or a TmpElement edge, but alp and
                     ;; ome might be nil.  In that case, we don't want to unify
                     ;; with nil.
                     (cond
                      (and alp ome) (unify a [e al om] [ed alp ome])
                      alp           (unify a [e al] [ed alp])
                      ome           (unify a [e om] [ed ome])
                      :else         (unify a e ed)))
                   (concat
                    (for [edge (cond
                                (and (ground? gal) (tg/vertex? gal)) (tg/iseq gal nil :out)
                                (and (ground? gom) (tg/vertex? gom)) (tg/iseq gom nil :in)
                                :else (tg/eseq g))]
                      [edge (tg/alpha edge) (tg/omega edge)])
                    ;; If alpha or omega are ground, set them in the new
                    ;; TmpElement edge.
                    [(let [tmp (tmp/make-tmp-edge g)]
                       (when (ground? gal)
                         (tmp/set-alpha tmp gal))
                       (when (ground? gom)
                         (tmp/set-omega tmp gom))
                       [tmp (when (ground? gal) gal) (when (ground? gom) gom)])]))
              (remove not)))
        ;; The edge is ground.  All we can do is unifying alpha and omega if it
        ;; is a real edge.
        (cond
         (tg/edge? ge) (or (unify a [al om] [(tg/alpha ge) (tg/omega ge)])
                           (fail a))
         (tmp/tmp-edge? ge) (do
                              (when (ground? gal)
                                (tmp/set-alpha ge gal))
                              (when (ground? gom)
                                (tmp/set-omega ge gom))
                              (succeed a))
         :else (fail a))))))

(defn edgeo
  "A relation where `e` is an edge in graph `g` from `alpha` to `omega`."
  [g e alpha omega]
  (if tmp/*make-tmp-elements*
    (tmp-edgeo g e alpha omega)
    (fn [a]
      (let [ge     (walk a e)
            galpha (walk a alpha)
            gomega (walk a omega)]
        (cond
         (ground? ge)
         (or (and (tg/edge? ge)
                  (unify a [alpha omega] [(tg/alpha ge) (tg/omega ge)]))
             (fail a))

         (ground? galpha)
         (if (tg/vertex? galpha)
           (to-stream
            (->> (map #(unify a [e omega] [% (tg/omega %)])
                      (tg/iseq galpha nil :out))
                 (remove not)))
           (fail a))

         (ground? gomega)
         (if (tg/vertex? gomega)
           (to-stream
            (->> (map #(unify a [e alpha] [% (tg/alpha %)])
                      (tg/iseq gomega nil :in))
                 (remove not)))
           (fail a))

         :else (to-stream
                (->> (for [edge (tg/eseq g)]
                       (unify a [e alpha omega]
                              [edge (tg/alpha edge) (tg/omega edge)]))
                     (remove not))))))))

(defn ^:private tmp-valueo [g ae at val]
  (fn [a]
    (let [gae  (walk a ae)
          gat  (walk a at)
          gval (walk a val)]
      ;; We assume gat must be given similar to gt in typeo.  gval must also be
      ;; given.
      (when-not (or (ground? gat) (not (keyword? gat)))
        (u/errorf "tmp-valueo's attribute name must be ground and given as keyword!"))
      (when-not (ground? gval)
        (u/errorf "attribute value not given. Please declare the element before its attributes."))
      (if (fresh? gae)
        (to-stream
         (->> (concat
               (for [elem (concat (tg/vseq g) (tg/eseq g))
                     :when (.containsAttribute (tg/attributed-element-class elem)
                                               (name gat))
                     :when (= gval (tg/value elem (name gat)))]
                 (unify a ae elem))
               [(unify a ae (doto (tmp/make-tmp-element g)
                              (tmp/add-attr gat gval)))])
              (remove not)))
        (cond
         (tg/attributed-element? gae) (if (= gval (tg/value gae gat))
                                        (succeed a)
                                        (fail a))
         (tmp/tmp-element? gae) (if (tmp/add-attr gae gat gval)
                                  (succeed a)
                                  (fail a))
         :else (fail a))))))

(defn valueo
  "A relation where graph `g`s attributed element `ae` has value `val` for its
  `at` attribute."
  [g ae at val]
  (if tmp/*make-tmp-elements*
    (tmp-valueo g ae at val)
    (fn [a]
      (let [gae  (walk a ae)
            gat  (walk a at)
            gval (walk a val)]
        (cond
         (and (ground? gae)
              (ground? gat))
         (or (and (tg/attributed-element? gae)
                  (keyword? gat)
                  (.getAttribute ^AttributedElementClass
                                 (tg/attributed-element-class gae)
                                 (name gat))
                  (unify a val (tg/value gae gat)))
             (fail a))

         (ground? gae)
         (if (tg/vertex? gae)
           (to-stream
            (->> (for [^Attribute attr (seq (.getAttributeList
                                             ^AttributedElementClass
                                             (tg/attributed-element-class gae)))
                       :let [an (keyword (.getName attr))]]
                   (unify a [at val] [an (tg/value gae an)]))
                 (remove not)))
           (fail a))

         :else (to-stream
                (->> (for [elem (concat (tg/vseq g) (tg/eseq g))
                           ^Attribute attr (seq (.getAttributeList
                                                 ^AttributedElementClass
                                                 (tg/attributed-element-class elem)))
                           :let [an (keyword (.getName attr))]]
                       (unify a [ae at val] [elem an (tg/value elem an)]))
                     (remove not))))))))

(defn adjo
  "A relation where vertex `rv` is in the `role` role of vertex `v` in graph
  `g`."
  [g v role rv]
  (fn [a]
    (let [edge-class-roles (fn [^EdgeClass ec from-or-to]
                             (remove empty? (.getAllRoles (if (= :to from-or-to)
                                                            (.getTo ec)
                                                            (.getFrom ec)))))
          gv    (walk a v)
          grole (walk a role)
          grv   (walk a rv)]
      (cond
       (and (ground? gv) (ground? grole))
       (if (and (tg/vertex? gv) (keyword? grole))
         (to-stream
          (->> (for [refed (funnyqt.query/adjs* gv grole)]
                 (unify a [rv] [refed]))
               (remove not)))
         (fail a))

       (ground? gv)
       (if (tg/vertex? gv)
         (to-stream
          (->> (for [e (tg/iseq gv)
                     rn (edge-class-roles (tg/attributed-element-class e)
                                          (if (tg/normal-edge? e) :to :from))
                     :when rn
                     :let [rn (keyword rn)]]
                 (unify a [role rv] [rn (tg/that e)]))
               (remove not)))
         (fail a))

       (ground? grv)
       (if (tg/vertex? grv)
         (to-stream
          (->> (for [e (tg/iseq grv)
                     rn (edge-class-roles (tg/attributed-element-class e)
                                          (if (tg/normal-edge? e) :from :to))
                     :when rn
                     :let [rn (keyword rn)]]
                 (unify a [v role] [(tg/that e) rn]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [s (tg/vseq g)
                         e (tg/iseq s)
                         rn (edge-class-roles (tg/attributed-element-class e)
                                              (if (tg/normal-edge? e) :to :from))
                         :when rn
                         :let [rn (keyword rn)]]
                     (unify a [v role rv] [(tg/this e) rn (tg/that e)]))
                   (remove not)))))))

;;# Metamodel specific

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the class `c`."
  [^AttributedElementClass c]
  (let [n (.getUniqueName c)
        fqn (.getQualifiedName c)]
    (mapv (fn [s]
            (with-meta (symbol s)
              {:unique-name
               (symbol (str "+" (clojure.string/replace
                                 s #"([!])?.*[.]" #(or (nth % 1) ""))))}))
          [fqn (str fqn "!") (str "!" fqn) (str "!" fqn "!")])))

(defn ^:private create-vc-relations
  "Creates relations for the given vertex class."
  [vc]
  (for [na (class->rel-symbols vc)]
    `(defn ~(:unique-name (meta na))
       ~(format "A relation where `v` is a %s vertex of graph `g`." na)
       [~'g ~'v]
       (all
        (typeo ~'g ~'v '~na)
        (vertexo ~'g ~'v)))))

(defn ^:private create-ec-relations
  "Creates relations for the given edge class."
  [^EdgeClass ec]
  (for [na (class->rel-symbols ec)]
    `(defn ~(:unique-name (meta na))
       ~(format "A relation where `e` is a %s edge from `al` to `om` in graph `g`." na)
       [~'g ~'e ~'al ~'om]
       (all
        (typeo ~'g ~'e '~na)
        (edgeo ~'g ~'e ~'al ~'om)))))

(defn ^:private create-attr-relation
  "Creates relations for the given attribute."
  [[attr aecs]] ;; attr is an attr name keyword, aecs the set of classes having
                ;; such an attr
  (let [ts (mapv #(p/qname %) aecs)]
    `(defn ~(symbol (str "+" (name attr)))
       ~(format "A relation where `ae` has value `val` for its %s attribute in graph `g`." attr)
       [~'g ~'ae ~'val]
       (all
        (typeo ~'g ~'ae '~ts)
        (valueo ~'g ~'ae ~attr ~'val)))))

(defn ^:private create-reference-relation
  "Creates a relation for the given role name."
  [rn owners]
  (let [role-rel-sym (symbol (str "+->" rn))
        make-one   (fn [[^EdgeClass ec dir]]
                     (let [ec-rel-sym (symbol (str "+" (.getUniqueName ec)))]
                       (if (= dir :omega)
                          `(~ec-rel-sym ~'g ~'ign ~'sv ~'tv)
                          `(~ec-rel-sym ~'g ~'ign ~'tv ~'sv))))
        make (fn [tups]
               (if (> (count tups) 1)
                 `(conde
                   ~@(mapv (fn [t] [(make-one t)]) tups))
                 (make-one (first tups))))]
    `(defn ~role-rel-sym
       ~(format "A relation where `sv` references `tv` in its `%s` role." rn)
       [~'g ~'sv ~'tv]
       (fresh [~'ign]
         ~(make owners)))))

(defmacro generate-schema-relations
  "Generates schema-specific relations in the namespace denoted by `nssym`.
  If `nssym` is nil (or not given), generate them in the current namespace.
  `schema-file` is the TG file with the schema."
  ([schema-file] `(generate-schema-relations ~schema-file nil))
  ([schema-file nssym]
     (let [^Schema schema (tg/load-schema
                           (if (.exists (clojure.java.io/file schema-file))
                             schema-file
                             (clojure.java.io/resource schema-file)))
           atts (atom {}) ;; map from attribute names given as keywords to set
                          ;; of attributed element classes that have it
           refs (atom {}) ;; map from role names given as keywords to set of
                          ;; [edgeclass dir] tuples that have it
           old-ns *ns*]
       `(do
          ~@(when nssym
              `[(ns ~nssym
                  (:refer-clojure :exclude [~'==]))])
          ;; The schema specific ones
          ~@(concat
             (doall
              (mapcat
               (fn [^VertexClass vc]
                 (doseq [a (mapv #(keyword (.getName ^Attribute %))
                                 (seq (.getOwnAttributeList vc)))]
                   (swap! atts
                          #(update-in %1 [%2] conj vc)
                          a))
                 (create-vc-relations vc))
               (seq (-> schema .getGraphClass .getVertexClasses))))
             (doall
              (mapcat
               (fn [^EdgeClass ec]
                 (doseq [a (mapv #(keyword (.getName ^Attribute %))
                                 (seq (.getOwnAttributeList ec)))]
                   (swap! atts
                          #(update-in %1 [%2] conj ec)
                          a))
                 (when-let [from-rn (-> ec .getFrom .getRolename)]
                   ;; Skip empty role names!
                   (when (seq from-rn)
                     (swap! refs
                            #(update-in %1 [%2] conj [ec :alpha])
                            from-rn)))
                 (when-let [to-rn (-> ec .getTo .getRolename)]
                   (when (seq to-rn)
                     (swap! refs
                            #(update-in %1 [%2] conj [ec :omega])
                            to-rn)))
                 (create-ec-relations ec))
               (seq (-> schema .getGraphClass .getEdgeClasses))))
             (doall
              (for [^Attribute a @atts]
                (create-attr-relation a)))
             (doall
              (for [[role owners] @refs]
                (create-reference-relation role owners))))
          (in-ns '~(ns-name old-ns))))))
