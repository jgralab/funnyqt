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
  `type-matcher`).  The graph `g` must be ground."
  [g e t]
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
                   (remove not)))))))

(defn vertexo
  "A relation where `v` is a vertex in graph `g`.
  `g` has to be ground."
  [g v]
  (fn [a]
    (let [gv (walk a v)]
      (if (fresh? gv)
        (to-stream
         (->> (map #(unify a v %) (tg/vseq g))
              (remove not)))
        (if (and (tg/vertex? gv)
                 (tg/contains-vertex? g gv))
          (succeed a)
          (fail a))))))

(defn edgeo
  "A relation where `e` is an edge in graph `g` from `alpha` to `omega`."
  [g e alpha omega]
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
                   (remove not)))))))

(defn valueo
  "A relation where graph `g`s attributed element `ae` has value `val` for its
  `at` attribute."
  [g ae at val]
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
       {:doc ~(format "A relation where `v` is a %s vertex." na)}
       [~'v]
       (fn [~'a]
         (let [~'gv (walk ~'a ~'v)]
           (cond
            (ground? ~'gv)
            (or (and (tg/vertex? ~'gv)
                     (tg/contains-vertex? ~'*model* ~'gv)
                     (p/has-type? ~'gv '~na)
                     (succeed ~'a))
                (and rel/*make-tmp-elements*
                     (tmp-element? ~'gv)
                     (set-tmp-kind ~'gv :vertex)
                     (set-tmp-type ~'gv '~na)
                     (succeed ~'a))
                (fail ~'a))

            :else (to-stream
                   (->> (map (fn [~'vertex] (unify ~'a ~'v ~'vertex))
                             (concat (tg/vseq ~'*model* '~na)
                                     (if rel/*make-tmp-elements*
                                       [(make-tmp-vertex ~'*model* '~na)]
                                       [])))
                   (remove not)))))))))

(defn ^:private create-ec-relations
  "Creates relations for the given edge class."
  [^EdgeClass ec]
  (let [from-vc-qn (p/qname (-> ec .getFrom .getVertexClass))
        to-vc-qn   (p/qname (-> ec .getTo .getVertexClass))]
    (for [na (class->rel-symbols ec)]
      `(defn ~(:unique-name (meta na))
         {:doc ~(format "A relation where `e' is a %s edge from `al' to `om'." na)}
         [~'e ~'al ~'om]
         (fn [~'a]
           (let [~'ge  (walk ~'a ~'e)
                 ~'gal (walk ~'a ~'al)
                 ~'gom (walk ~'a ~'om)]
             (cond
              (ground? ~'ge)
              (or (and (tg/edge? ~'ge)
                       (unify ~'a [~'al ~'om]
                              [(tg/alpha ~'ge) (tg/omega ~'ge)]))
                  (fail ~'a))

              (and (ground? ~'gal) (ground? ~'gom)
                   (or (tmp-element? ~'gal) (tmp-element? ~'gom)))
              (or (unify ~'a ~'e
                         (let [tmp# (make-tmp-edge ~'*model* '~na)]
                           (set-tmp-alpha tmp# ~'gal)
                           (set-tmp-omega tmp# ~'gom)
                           tmp#))
                  (fail ~'a))

              (and (ground? ~'gal) (ground? ~'gom)
                   (tg/vertex? ~'gal) (tg/vertex? ~'gom))
              (to-stream
               (->> (concat
                     (map (fn [~'inc]
                            (when (= ~'gom (tg/omega ~'inc))
                              (unify ~'a ~'e ~'inc)))
                          (tg/iseq ~'gal '~na :out))
                     (if rel/*make-tmp-elements*
                       (let [tmp# (make-tmp-edge ~'*model* '~na)]
                         (set-tmp-alpha tmp# ~'gal)
                         (set-tmp-omega tmp# ~'gom)
                         [(unify ~'a ~'e tmp#)])
                       []))
                     (remove not)))

              (ground? ~'gal)
              (cond
               (tg/vertex? ~'gal)
               (to-stream
                (->> (map (fn [~'incidence]
                            (unify ~'a [~'e ~'om]
                                   [~'incidence (tg/omega ~'incidence)]))
                          (tg/iseq ~'gal '~na :out))
                     (remove not)))

               :else (fail ~'a))

              (ground? ~'gom)
              (cond
               (tg/vertex? ~'gom)
               (to-stream
                (->> (map (fn [~'incidence]
                            (unify ~'a [~'e ~'al]
                                   [~'incidence (tg/alpha ~'incidence)]))
                          (tg/iseq ~'gom '~na :in))
                     (remove not)))

               :else (fail ~'a))

              :else (to-stream
                     (->> (for [~'edge (concat (tg/eseq ~'*model* '~na)
                                               (if rel/*make-tmp-elements*
                                                 [(make-tmp-edge ~'*model* '~na)]
                                                 []))]
                            (unify ~'a [~'e ~'al ~'om]
                                   [~'edge (tg/alpha ~'edge) (tg/omega ~'edge)]))
                          (remove not))))))))))

(defn ^:private create-attr-relation
  "Creates relations for the given attribute."
  [[attr aecs]]   ;;; attr is an attr name symbol, aecs the set of classes
                  ;;; having such an attr
  (let [ts     (mapv #(p/qname %) aecs) ;; a type spec
        seqf   (cond
                (every? #(instance? VertexClass %) aecs) `tg/vseq
                (every? #(instance? EdgeClass %)   aecs) `tg/eseq
                :else `(fn [graph# ts#]
                         (apply concat ((juxt tg/vseq
                                              tg/eseq)
                                        graph# ts#))))]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `ae' has value `val' for its %s attribute." attr)}
       [~'ae ~'val]
       (fn [~'a]
         (let [~'gae (walk ~'a ~'ae)
               ~'gval (walk ~'a ~'val)]
           (cond
            (ground? ~'gae)
            (or (and (tg/attributed-element? ~'gae)
                     (.getAttribute ^AttributedElementClass
                                    (tg/attributed-element-class ~'gae)
                                    ~(name attr))
                     (unify ~'a ~'val (tg/value ~'gae ~attr)))
                (and rel/*make-tmp-elements*
                     (tmp-element? ~'gae)
                     (add-tmp-attr ~'gae ~attr ~'gval)
                     (succeed ~'a))
                (fail ~'a))

            :else (to-stream
                   (->> (concat (for [~'oae (~seqf ~'*model* '~ts)
                                      :let [~'oval (tg/value ~'oae ~attr)]]
                                  (unify ~'a [~'ae ~'val] [~'oae ~'oval]))
                                (if rel/*make-tmp-elements*
                                  [(let [tmp# (make-tmp-element ~'*model*)]
                                     (if ~'gval
                                       (do (add-tmp-attr tmp# ~attr ~'gval)
                                           (unify ~'a [~'ae ~'val] [tmp# ~'gval]))
                                       (unify ~'a ~'ae tmp#)))]
                                  []))
                        (remove not)))))))))

(defn ^:private create-reference-relation
  "Creates a relation for the given role name."
  [rn owners]
  (let [role-rel-sym (symbol (str "+->" rn))
        make-one   (fn [[^EdgeClass ec dir]]
                     (let [ec-rel-sym (symbol (str "+" (.getUniqueName ec)))]
                       (if (= dir :omega)
                          `(~ec-rel-sym ~'ign ~'sv ~'tv)
                          `(~ec-rel-sym ~'ign ~'tv ~'sv))))
        make (fn [tups]
               (if (> (count tups) 1)
                 `(conde
                   ~@(mapv (fn [t] [(make-one t)]) tups))
                 (make-one (first tups))))]
    `(defn ~role-rel-sym
       ~(format "A relation where `sv` references `tv` in its `%s` role." rn)
       [~'sv ~'tv]
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
           atts (atom {}) ;; map from attribute names to set of attributed element classes that have it
           refs (atom {}) ;; map from role names to set of [edgeclass dir] tuples  that have it
           old-ns *ns*]
       `(do
          ~@(when nssym
              `[(ns ~nssym
                  (:refer-clojure :exclude [~'==]))

                (def ~(vary-meta '*model* assoc :dynamic true))
                          ;; The standard relations
                (make-standard-tg-relations ~'*model*)])
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
