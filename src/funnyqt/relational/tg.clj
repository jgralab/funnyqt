(ns funnyqt.relational.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.core.logic.protocols :only [walk]]
        funnyqt.relational.util)
  (:require [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.relational :as rel]
            [funnyqt.relational.tmp-elem :as tmp]
            clojure.java.io)
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass IncidenceClass)
   (funnyqt.relational.tmp_elem WrapperElement TmpElement)))

(defn kind-aec-tup-from-spec [g spec]
  (let [aecfn (fn [ts]
                (tg/attributed-element-class
                 g (second (u/type-with-modifiers (name ts)))))
        kindfn #(if (tg/vertex-class? %) :element :relationship)]
    (cond
     (symbol? spec) (let [aec (aecfn spec)]
                      [(kindfn aec) aec])
     (vector? spec) (let [aecs  (set (map aecfn spec))
                          kinds (set (map kindfn aecs))]
                      [kinds aecs])
     :else (u/errorf "Unknown spec %s." spec))))

(defn tmp-typeo [g e t]
  (fn [a]
    (let [ge (walk a e)
          gt (walk a t)]
      (cond
       (not (ground? gt))
       (u/errorf "tmp-typeo: type must be ground.")

       (not (or (fresh? ge)
                (tmp/tmp-or-wrapper-element? ge)))
       (u/errorf "tmp-typeo: e must be fresh or a ground Wrapper/TmpElement but was %s." ge)

       (ground? ge)
       (let [[kind aec] (kind-aec-tup-from-spec g gt)]
         (if (and (tmp/set-type ge gt)
                  (tmp/set-kind ge kind))
           (succeed a)
           (fail a)))

       :else (let [[kind aec] (kind-aec-tup-from-spec g gt)
                   seqfn (cond
                          (= kind :element) tg/vseq
                          (= kind :relationship)   tg/eseq
                          :else (fn [g gt]
                                  (concat (tg/vseq g gt) (tg/eseq g gt))))]
               (to-stream
                (->> (map #(unify a e %)
                          (concat
                           ;; Existing vertices/edges wrapped
                           (map (partial tmp/make-wrapper g)
                                (seqfn g gt))
                           ;; One new vertex/edge tmp element
                           [(tmp/make-tmp-element g kind gt)]))
                     (remove not))))))))

(defn typeo
  "A relation where in graph `g`, vertex or edge `e` has the type `t`, a graph
  element class name.  In fact, `t` may be any type specification (see
  `funnyqt.protocols/type-matcher`).  The graph `g` must be ground."
  [g e t]
  (if tmp/*make-tmp-elements*
    (tmp-typeo g e t)
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
                     (remove not))))))))

(defn tmp-vertexo [g v]
  (fn [a]
    (let [gv (walk a v)]
      (cond
       (not (or (fresh? gv)
                (tmp/tmp-or-wrapper-element? gv)))
       (u/errorf "tmp-vertexo: v must be fresh or a ground Wrapper/TmpElement but was %s."
                 gv)

       (ground? gv)
       (if (tmp/set-kind gv :element)
         (succeed a)
         (fail a))

       :else (to-stream
              (->> (map #(unify a v %)
                        (concat
                         ;; Existing vertices wrapped
                         (map (partial tmp/make-wrapper g)
                              (tg/vseq g))
                         ;; One new vertex tmp element
                         [(tmp/make-tmp-element g :element)]))
                   (remove not)))))))

(defn vertexo
  "A relation where `v` is a vertex in graph `g`.
  `g` has to be ground."
  [g v]
  (if tmp/*make-tmp-elements*
    (tmp-vertexo g v)
    (fn [a]
      (let [gv (walk a v)]
        (if (ground? gv)
          (if (and (tg/vertex? gv) (tg/contains-vertex? g gv))
            (succeed a)
            (fail a))
          (to-stream
           (->> (map #(unify a v %) (tg/vseq g))
                (remove not))))))))

(defn tmp-edgeo [g e alpha omega]
  (fn [a]
    (let [ge     (walk a e)
          galpha (walk a alpha)
          gomega (walk a omega)]
      ;;(println (format "(tmp-edgeo g %s %s %s)" ge galpha gomega))
      (cond
       (not (or (fresh? ge) (tmp/tmp-or-wrapper-element? ge)))
       (u/errorf "tmp-edgeo: e must be fresh or a ground Wrapper/TmpElement but was %s."
                 ge)

       (not (or (fresh? galpha) (tmp/tmp-or-wrapper-element? galpha)))
       (u/errorf "tmp-edgeo: alpha must be fresh or a ground Wrapper/TmpElement but was %s."
                 galpha)

       (not (or (fresh? gomega) (tmp/tmp-or-wrapper-element? gomega)))
       (u/errorf "tmp-edgeo: omega must be fresh or a ground Wrapper/TmpElement but was %s."
                 gomega)

       (tmp/wrapper-element? ge)
       (unify a [alpha omega]
              (let [edge (.wrapped-element ^WrapperElement ge)]
                [(tmp/make-wrapper g (tg/alpha edge))
                 (tmp/make-wrapper g (tg/omega edge))]))

       (and (fresh? ge) (tmp/wrapper-element? galpha) (tmp/wrapper-element? gomega))
       (to-stream
        (->> (map (fn [ed]
                    (unify a e ed))
                  (concat
                   (map (partial tmp/make-wrapper g)
                        (filter
                         #(= (.wrapped-element ^WrapperElement gomega) (tg/omega %))
                         (tg/iseq (.wrapped-element ^WrapperElement galpha) nil :out)))
                   [(doto (tmp/make-tmp-element g :relationship)
                      (tmp/set-alpha galpha)
                      (tmp/set-omega gomega))]))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? galpha) (tmp/wrapper-element? gomega))
       (if (and (tmp/set-alpha ge galpha)
                (tmp/set-omega ge gomega))
         (succeed a)
         (fail a))

       (and (fresh? ge) (tmp/wrapper-element? galpha))
       (to-stream
        (->> (map (fn [ed-om]
                    (unify a [e omega] ed-om))
                  (concat
                   (map (fn [ed]
                          [(tmp/make-wrapper g ed)
                           (tmp/make-wrapper g (tg/omega ed))])
                        (tg/iseq (.wrapped-element ^WrapperElement galpha) nil :out))
                   (let [ed (tmp/make-tmp-element g :relationship)]
                     (tmp/set-alpha ed galpha)
                     (tmp/set-omega ed gomega)
                     [[ed gomega]])))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? galpha))
       (if (and (tmp/set-alpha ge galpha)
                (tmp/set-omega ge gomega))
         (succeed a)
         (fail a))

       (and (fresh? ge) (tmp/wrapper-element? gomega))
       (to-stream
        (->> (map (fn [ed-al]
                    (unify a [e alpha] ed-al))
                  (concat
                   (map (fn [ed]
                          [(tmp/make-wrapper g ed)
                           (tmp/make-wrapper g (tg/alpha ed))])
                        (tg/iseq (.wrapped-element ^WrapperElement gomega) nil :in))
                   (let [ed (tmp/make-tmp-element g :relationship)]
                     (tmp/set-alpha ed galpha)
                     (tmp/set-omega ed gomega)
                     [[ed galpha]])))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? gomega))
       (if (and (tmp/set-alpha ge galpha)
                (tmp/set-omega ge gomega))
         (succeed a)
         (fail a))

       :else (u/errorf "(tmp-edgeo %s %s %s %s)" g ge galpha gomega)))))

(defn edgeo
  "A relation where `e` is an edge in graph `g` from `alpha` to `omega`.
  `g` has to be ground."
  [g e alpha omega]
  (if tmp/*make-tmp-elements*
    (tmp-edgeo g e alpha omega)
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
                     (remove not))))))))

(defn ^:private attribute-list
  "Gets the list of all attributes of ae's attributed element class."
  [ae]
  (seq (.getAttributeList (tg/attributed-element-class ae))))

(defn tmp-valueo [g ae at val]
  (fn [a]
    (let [gae  (walk a ae)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (not (tmp/tmp-or-wrapper-element? gae))
       (u/errorf "tmp-valueo: ae has to be a ground Tmp/WrapperElement but was %s."
               gae)

       (not (keyword? gat))
       (u/errorf "tmp-valueo: at must be a ground keyword but was %s." gat)

       :else (if (tmp/add-attr gae gat gval)
               (succeed a)
               (fail a))))))

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
                     (remove not))))))))

(defn tmp-adjo [g v role rv]
  (fn [a]
    (let [gv    (walk a v)
          grole (walk a role)
          grv   (walk a rv)]
      (cond
       (not (tmp/tmp-or-wrapper-element? gv))
       (u/errorf "tmp-adjo: v has to be a ground Tmp/WrapperElement but was %s."
                 gv)

       (not (keyword? grole))
       (u/errorf "tmp-adjo: role must be a ground keyword but was %s." grole)

       (and (tmp/wrapper-element? gv) (tmp/tmp-or-wrapper-element? grv))
       (if (tmp/add-ref gv grole grv)
         (succeed a)
         (fail a))

       (and (tmp/wrapper-element? gv) (fresh? grv))
       (to-stream
        (->> (map #(unify a rv (if (fn? %) (%) %))
                  (concat
                   (map #(tmp/make-wrapper g %)
                        (q/adjs (.wrapped-element ^WrapperElement gv) grole))
                   ;; This must not be executed if there's an existing adjacent
                   ;; vertex, so we wrap it in a function.
                   [#(let [refed (tmp/make-tmp-element g :element)]
                       (tmp/add-ref gv grole refed)
                       refed)]))
             (remove not)))

       :else (u/errorf "unsupported args to tmp-adjo:\n  v = %s\n  role = %s\n rv = %s"
                       gv grole grv)))))

(defn adjo
  "A relation where vertex `rv` is in the `role` role of vertex `v` in graph
  `g`."
  [g v role rv]
  (if tmp/*make-tmp-elements*
    (tmp-adjo g v role rv)
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
                     (remove not))))))))

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
