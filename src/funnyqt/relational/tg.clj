(ns funnyqt.relational.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.core.logic.protocols :only [walk]]
        funnyqt.relational.util)
  (:require [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]
            funnyqt.query.tg
            funnyqt.query
            [funnyqt.utils :as u]
            [funnyqt.relational :as rel]
            clojure.java.io)
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass IncidenceClass)))

(defn typeo
  "A relation where in graph `g`, vertex or edge `e` has the type `t`, a graph
  element class name.  In fact, `t` may be any type specification (see
  `funnyqt.protocols/type-matcher`).  The graph `g` must be ground."
  [g e t]
  (fn [a]
    (let [ge (walk a e)
          gt (walk a t)]
      (cond
       (or (and (ground? ge) (not (tg/attributed-element? ge)))
           (and (ground? gt) (not (or (symbol? gt) (coll? gt)))))
       (fail a)

       (and (ground? ge) (ground? gt))
       (if (p/has-type? ge gt) (succeed a) (fail a))

       (ground? ge)
       (unify a t (p/qname ge))

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
                   (remove not)))))))

(defn vertexo
  "A relation where `v` is a vertex in graph `g`.
  `g` has to be ground."
  [g v]
  (fn [a]
    (let [gv (walk a v)]
      (if (ground? gv)
        (if (and (tg/vertex? gv) (tg/contains-vertex? g gv))
          (succeed a)
          (fail a))
        (to-stream
         (->> (map #(unify a v %) (tg/vseq g))
              (remove not)))))))

(defn edgeo
  "A relation where `e` is an edge in graph `g` from `alpha` to `omega`.
  `g` has to be ground."
  [g e alpha omega]
  (fn [a]
    (let [ge     (walk a e)
          galpha (walk a alpha)
          gomega (walk a omega)]
      (cond
       (or (and (ground? ge) (not (tg/edge? ge)))
           (and (ground? galpha) (not (tg/vertex? galpha)))
           (and (ground? gomega) (not (tg/vertex? gomega))))
       (fail a)

       (ground? ge)
       (unify a [alpha omega] [(tg/alpha ge) (tg/omega ge)])

       (ground? galpha)
       (to-stream
        (->> (map #(unify a [e omega] [% (tg/omega %)])
                  (tg/iseq galpha nil :out))
             (remove not)))

       (ground? gomega)
       (to-stream
        (->> (map #(unify a [e alpha] [% (tg/alpha %)])
                  (tg/iseq gomega nil :in))
             (remove not)))

       :else (to-stream
              (->> (for [edge (tg/eseq g)]
                     (unify a [e alpha omega]
                            [edge (tg/alpha edge) (tg/omega edge)]))
                   (remove not)))))))

(defn ^:private attribute-list
  "Gets the list of all attributes of ae's attributed element class."
  [ae]
  (seq (.getAttributeList (tg/attributed-element-class ae))))

(defn valueo
  "A relation where graph `g`s attributed element `ae` has value `val` for its
  `at` attribute."
  [g ae at val]
  (fn [a]
    (let [gae  (walk a ae)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (or (and (ground? gae) (not (tg/attributed-element? gae)))
           (and (ground? gat) (not (keyword? gat)))
           (and (ground? gae) (ground? gat)
                (not (.getAttribute (tg/attributed-element-class gae)
                                    (name gat)))))
       (fail a)

       (and (ground? gae) (ground? gat))
       (unify a val (tg/value gae gat))

       (ground? gae)
       (to-stream
        (->> (for [^Attribute attr (attribute-list gae)
                   :let [an (keyword (.getName attr))]]
               (unify a [at val] [an (tg/value gae an)]))
             (remove not)))

       :else (to-stream
              (->> (for [elem (concat (tg/vseq g) (tg/eseq g))
                         ^Attribute attr (attribute-list elem)
                         :let [an (keyword (.getName attr))]]
                     (unify a [ae at val] [elem an (tg/value elem an)]))
                   (remove not)))))))

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
       ;; Ground lvars must have the expected types
       (or (and (ground? gv) (not (tg/vertex? gv)))
           (and (ground? grole) (not (keyword? grole)))
           (and (ground? grv)   (not (tg/vertex? grv))))
       (fail a)

       (and (ground? gv) (ground? grole))
       (to-stream
        (->> (for [refed (funnyqt.query/adjs* gv grole)]
               (unify a [rv] [refed]))
             (remove not)))

       (ground? gv)
       (to-stream
        (->> (for [e (tg/iseq gv)
                   rn (edge-class-roles (tg/attributed-element-class e)
                                        (if (tg/normal-edge? e) :to :from))
                   :when rn
                   :let [rn (keyword rn)]]
               (unify a [role rv] [rn (tg/that e)]))
             (remove not)))

       (ground? grv)
       (to-stream
        (->> (for [e (tg/iseq grv)
                   rn (edge-class-roles (tg/attributed-element-class e)
                                        (if (tg/normal-edge? e) :from :to))
                   :when rn
                   :let [rn (keyword rn)]]
               (unify a [v role] [(tg/that e) rn]))
             (remove not)))

       :else (to-stream
              (->> (for [s (tg/vseq g)
                         e (tg/iseq s)
                         rn (if (ground? grole)
                              [grole]
                              (edge-class-roles (tg/attributed-element-class e)
                                                (if (tg/normal-edge? e) :to :from)))
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
        role-rel-kw  (keyword rn)]
    `(defn ~role-rel-sym
       ~(format "A relation where `sv` references `tv` in its `%s` role." rn)
       [~'g ~'sv ~'tv]
       (adjo ~'g ~'sv ~role-rel-kw ~'tv))))

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
