(ns funnyqt.relational
  "Generic relations"
  (:require clojure.walk
            clojure.string
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.relational.util :as ru]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.generic :as g])
  (:import (funnyqt.relational.tmp_elem WrapperElement)))

;;* Utils

(defmacro with-fresh
  "Replace all symbols with a leading question mark with fresh lvars.
  In addition, all occurences of `_' are replaced with fresh lvars, one per
  occurence.  That means, that in `forms` all occurences of ?foo will be
  unified, but all occurences of `_' are not."
  [& forms]
  (let [fs (clojure.walk/postwalk #(if (= '_ %) (gensym "?") %) forms)
	qsyms (vec (distinct (filter ru/qmark-symbol? (flatten fs))))]
    `(ccl/fresh ~qsyms
       ~@fs)))

(defn echo
  "Prints the values of all `lvars`.  Always succeeds."
  ([lvars]
     (echo nil lvars))
  ([prompt lvars]
     (fn [a]
       (println "################# " (or prompt "ECHO") " #################")
       #_(clojure.pprint/pprint a)
       (println (clojure.string/join
                 (map (fn [^clojure.core.logic.LVar v]
                        (if (ccl/lvar? v)
                          (let [w (cclp/walk a v)]
                            (str (:oname v) " = " w "\n"))
                          (str "### = " v "\n")))
                      lvars)))
       (ccl/succeed a))))

(defn ^:private str-splits [s]
  (loop [idx 0, r []]
    (if (<= idx (count s))
      (recur (inc idx)
	     (conj r [(subs s 0 idx) (subs s idx)]))
      r)))

(defn stro
  "A relation where the strings `x` and `y` concatenate to `xy`, or the strings
  `x`, `y`, and `z` concatenate to `xyz`.  Not fully relational."
  ([x y xy]
     (fn [a]
       (let [wx  (cclp/walk a x)
             wy  (cclp/walk a y)
             wxy (cclp/walk a xy)]
         (cond
          (and (ru/ground? wx) (ru/ground? wy) (ru/ground? wxy))
          (if (= (str wx wy) wxy) (ccl/succeed a) (ccl/fail a))

          (and (ru/ground? wx) (ru/ground? wy))
          (or (ccl/unify a xy (str wx wy))
              (ccl/fail a))

          (and (ru/ground? wx) (ru/ground? wxy) (string? wxy)
               (.startsWith ^String wxy wx))
          (or (ccl/unify a y (subs wxy (count wx)))
              (ccl/fail a))

          (and (ru/ground? wy) (ru/ground? wxy) (string? wxy)
               (.endsWith ^String wxy wy))
          (or (ccl/unify a x (subs wxy 0 (- (count wxy) (count wy))))
              (ccl/fail a))

          (ru/ground? wxy)
          (ccl/to-stream
           (->> (map (fn [[s1 s2]]
                       (ccl/unify a [x y] [s1 s2]))
                     (str-splits wxy))
                (remove not)))

          ;; TODO: we should not fail here...
          :else (ccl/fail a)))))
  ([x y z xyz]
     (ccl/fresh [front]
       (ccl/conde
        ;; This one works if x and y are ground
        [(stro x y front)
         (stro front z xyz)]
        ;; This one works if xyz is ground
        [(stro front z xyz)
         (stro x y front)]))))

;;# Model Relations

(defn ^:private kind-class-tup-from-spec [m spec]
  (let [aecfn (fn [ts]
                (g/mm-class m (second (u/type-with-modifiers (name ts)))))
        kindfn #(cond (g/mm-element-class? %)      :element
                      (g/mm-relationship-class? %) :relationship
                      :else (u/errorf "Cannot handle %s" %))]
    (cond
     (symbol? spec) (let [aec (aecfn spec)]
                      [(kindfn aec) aec])
     (vector? spec) (let [aecs  (set (map aecfn spec))
                          kinds (set (map kindfn aecs))]
                      [kinds aecs])
     :else (u/errorf "Unknown spec %s." spec))))

;;## Typing Relation

(defn ^:private tmp-typeo [g e t]
  (fn [a]
    (let [ge (cclp/walk a e)
          gt (cclp/walk a t)]
      (cond
       (not (ru/ground? gt))
       (u/errorf "tmp-typeo: type must be ground.")

       (not (or (ru/fresh? ge)
                (tmp/tmp-or-wrapper-element? ge)))
       (u/errorf "tmp-typeo: e must be fresh or a ground Wrapper/TmpElement but was %s." ge)

       (ru/ground? ge)
       (let [[kind aec] (kind-class-tup-from-spec g gt)]
         (if (and (tmp/set-type ge gt)
                  (tmp/set-kind ge kind))
           (ccl/succeed a)
           (ccl/fail a)))

       :else (let [[kind aec] (kind-class-tup-from-spec g gt)
                   seqfn (cond
                           (= kind :element)        g/elements
                           (= kind :relationship)   g/relationships
                           :else (fn [g gt]
                                   (concat (g/elements g gt)
                                           (when (satisfies? g/IRelationships g)
                                             (g/relationships g gt)))))]
               (ccl/to-stream
                (->> (map #(ccl/unify a e %)
                          (concat
                           ;; Existing vertices/edges wrapped
                           (map (partial tmp/make-wrapper g e)
                                (seqfn g gt))
                           ;; One new vertex/edge tmp element
                           [(tmp/make-tmp-element g kind gt)]))
                     (remove not))))))))

(defn typeo
  "A relation where in graph `g`, vertex or edge `e` has the type `t`, a graph
  element class name.  In fact, `t` may be any type specification (see
  `funnyqt.generic/type-matcher`).  The graph `g` must be ground."
  [g e t]
  (if tmp/*make-tmp-elements*
    (tmp-typeo g e t)
    (fn [a]
      (let [ge (cclp/walk a e)
            gt (cclp/walk a t)]
        (cond
          (or (and (ru/ground? ge) (not (or (g/element? ge) (g/relationship? ge))))
              (and (ru/ground? gt) (not (or (symbol? gt) (coll? gt)))))
          (ccl/fail a)

          (and (ru/ground? ge) (ru/ground? gt))
          (if (g/has-type? ge gt) (ccl/succeed a) (ccl/fail a))

          (ru/ground? ge)
          (ccl/unify a t (g/qname ge))

          (ru/ground? gt)
          (if (symbol? gt)
            ;; Ok, here we can determine if its a vertex or an edge class
            (let [[_ tn _] (u/type-with-modifiers (name gt))
                  aec      (g/mm-class g tn)]
              (cond
                (g/mm-element-class? aec)
                (ccl/to-stream
                 (->> (map #(ccl/unify a e %) (g/elements g gt))
                      (remove not)))
                ;;---
                (g/mm-relationship-class? aec)
                (ccl/to-stream
                 (->> (map #(ccl/unify a e %) (g/relationships g gt))
                      (remove not)))
                ;;---
                :else (u/errorf "%s is neither element nor relationship class." aec)))
            (ccl/to-stream
             (->> (map #(ccl/unify a e %)
                       (concat (g/elements g gt)
                               (when (satisfies? g/IRelationships g)
                                 (g/relationships g gt))))
                  (remove not))))

          :else (ccl/to-stream
                 (->> (for [elem (concat (g/elements g)
                                         (when (satisfies? g/IRelationships g)
                                           (g/relationships g)))]
                        (ccl/unify a [e t] [elem (g/qname elem)]))
                      (remove not))))))))

;;## Element Relation

(defn ^:private tmp-elemento [g v]
  (fn [a]
    (let [gv (cclp/walk a v)]
      (cond
       (not (or (ru/fresh? gv)
                (tmp/tmp-or-wrapper-element? gv)))
       (u/errorf "tmp-elemento: v must be fresh or a ground Wrapper/TmpElement but was %s."
                 gv)

       (ru/ground? gv)
       (if (tmp/set-kind gv :element)
         (ccl/succeed a)
         (ccl/fail a))

       :else (ccl/to-stream
              (->> (map #(ccl/unify a v %)
                        (concat
                         ;; Existing vertices wrapped
                         (map (partial tmp/make-wrapper g v)
                              (g/elements g))
                         ;; One new vertex tmp element
                         [(tmp/make-tmp-element g :element)]))
                   (remove not)))))))

(defn elemento
  "A relation where `v` is a vertex in graph `g`.
  `g` has to be ground."
  [g v]
  (if tmp/*make-tmp-elements*
    (tmp-elemento g v)
    (fn [a]
      (let [gv (cclp/walk a v)]
        (if (ru/ground? gv)
          ;; TODO: Maybe we also want a contains check here, e.g., is the
          ;; element contained in this model?
          (if (g/element? gv)
            (ccl/succeed a)
            (ccl/fail a))
          (ccl/to-stream
           (->> (map #(ccl/unify a v %) (g/elements g))
                (remove not))))))))

;;## Relationships

(defn ^:private tmp-relationshipo [g e alpha omega]
  (fn [a]
    (let [ge     (cclp/walk a e)
          galpha (cclp/walk a alpha)
          gomega (cclp/walk a omega)]
      (cond
       (not (or (ru/fresh? ge) (tmp/tmp-or-wrapper-element? ge)))
       (u/errorf "tmp-relationshipo: e must be fresh or a ground Wrapper/TmpElement but was %s."
                 ge)

       (not (or (ru/fresh? galpha) (tmp/tmp-or-wrapper-element? galpha)))
       (u/errorf "tmp-relationshipo: alpha must be fresh or a ground Wrapper/TmpElement but was %s."
                 galpha)

       (not (or (ru/fresh? gomega) (tmp/tmp-or-wrapper-element? gomega)))
       (u/errorf "tmp-relationshipo: omega must be fresh or a ground Wrapper/TmpElement but was %s."
                 gomega)

       (tmp/wrapper-element? ge)
       (ccl/unify a [alpha omega]
                    (let [edge (.wrapped-element ^WrapperElement ge)]
                      [(tmp/make-wrapper g alpha (g/source edge))
                       (tmp/make-wrapper g omega (g/target edge))]))

       (and (ru/fresh? ge) (tmp/wrapper-element? galpha) (tmp/wrapper-element? gomega))
       (ccl/to-stream
        (->> (map (fn [ed]
                    (ccl/unify a e ed))
                  (concat
                   (map (partial tmp/make-wrapper g e)
                        (filter
                         #(= (.wrapped-element ^WrapperElement gomega) (g/target %))
                         (g/incident-relationships (.wrapped-element ^WrapperElement galpha) nil :out)))
                   [(doto (tmp/make-tmp-element g :relationship)
                      (tmp/set-source alpha)
                      (tmp/set-target omega))]))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? galpha) (tmp/wrapper-element? gomega))
       (if (and (tmp/set-source ge alpha)
                (tmp/set-target ge omega))
         (ccl/succeed a)
         (ccl/fail a))

       (and (ru/fresh? ge) (tmp/wrapper-element? galpha))
       (ccl/to-stream
        (->> (map (fn [ed-om]
                    (ccl/unify a [e omega] ed-om))
                  (concat
                   (map (fn [ed]
                          [(tmp/make-wrapper g e ed)
                           (tmp/make-wrapper g omega (g/target ed))])
                        (g/incident-relationships (.wrapped-element ^WrapperElement galpha) nil :out))
                   [(let [ed (tmp/make-tmp-element g :relationship)]
                      (tmp/set-source ed alpha)
                      (tmp/set-target ed omega)
                      [ed gomega])]))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? galpha))
       (if (and (tmp/set-source ge alpha)
                (tmp/set-target ge omega))
         (ccl/succeed a)
         (ccl/fail a))

       (and (ru/fresh? ge) (tmp/wrapper-element? gomega))
       (ccl/to-stream
        (->> (map (fn [ed-al]
                    (ccl/unify a [e alpha] ed-al))
                  (concat
                   (map (fn [ed]
                          [(tmp/make-wrapper g e ed)
                           (tmp/make-wrapper g alpha (g/source ed))])
                        (g/incident-relationships (.wrapped-element ^WrapperElement gomega) nil :in))
                   [(let [ed (tmp/make-tmp-element g :relationship)]
                      (tmp/set-source ed alpha)
                      (tmp/set-target ed omega)
                      [ed galpha])]))
             (remove not)))

       (and (tmp/tmp-element? ge) (tmp/wrapper-element? gomega))
       (if (and (tmp/set-source ge alpha)
                (tmp/set-target ge omega))
         (ccl/succeed a)
         (ccl/fail a))

       :else (u/errorf "Can't handle (tmp-relationshipo %s %s %s %s)" g ge galpha gomega)))))

(defn relationshipo
  "A relation where `e` is an edge in graph `g` from `alpha` to `omega`.
  `g` has to be ground."
  [g e alpha omega]
  (if tmp/*make-tmp-elements*
    (tmp-relationshipo g e alpha omega)
    (fn [a]
      (let [ge     (cclp/walk a e)
            galpha (cclp/walk a alpha)
            gomega (cclp/walk a omega)]
        (cond
         (or (and (ru/ground? ge) (not (g/relationship? ge)))
             (and (ru/ground? galpha) (not (g/element? galpha)))
             (and (ru/ground? gomega) (not (g/element? gomega))))
         (ccl/fail a)

         (ru/ground? ge)
         (ccl/unify a [alpha omega] [(g/source ge) (g/target ge)])

         (ru/ground? galpha)
         (ccl/to-stream
          (->> (map #(ccl/unify a [e omega] [% (g/target %)])
                    (g/incident-relationships galpha nil :out))
               (remove not)))

         (ru/ground? gomega)
         (ccl/to-stream
          (->> (map #(ccl/unify a [e alpha] [% (g/source %)])
                    (g/incident-relationships gomega nil :in))
               (remove not)))

         :else (ccl/to-stream
                (->> (for [edge (g/relationships g)]
                       (ccl/unify a [e alpha omega]
                                  [edge (g/source edge) (g/target edge)]))
                     (remove not))))))))

;;## Attribute values

(defn ^:private property-list [el method]
  (let [c (if (g/mm-class? el)
            el
            (g/mm-class el))
        l (seq (method c))
        supers (g/mm-direct-superclasses c)]
    (if (seq supers)
      (concat l (mapcat #(property-list % method) supers))
      l)))

(defn ^:private tmp-avalo [m o at val may-override]
  (fn [a]
    (let [go  (cclp/walk a o)
          gat (cclp/walk a at)]
      (cond
       (not (tmp/tmp-or-wrapper-element? go))
       (u/errorf "tmp-valueo: o has to be a ground Tmp/WrapperElement but was %s."
                 go)

       (not (keyword? gat))
       (u/errorf "tmp-valueo: at must be a ground keyword but was %s." gat)

       :else (do (tmp/add-attr go gat val may-override)
                 (ccl/succeed a))))))


(defn avalo
  "A relation where graph `g`s attributed element `ae` has value `val` for its
  `at` attribute.

  The parameter `may-override` specifies that the attribute value may be
  overridden when transforming into the direction of `g` in a bidirectional
  transformation.  (You normally shouldn't use that parameter directly, but use
  the API generated by `generate-schema-relations`.)"
  ([g ae at val]
     (avalo g ae at val false))
  ([g ae at val may-override]
     (if tmp/*make-tmp-elements*
       (tmp-avalo g ae at val may-override)
       (fn [a]
         (let [gae  (cclp/walk a ae)
               gat  (cclp/walk a at)
               gval (cclp/walk a val)]
           (cond
             (or (and (ru/ground? gae)
                      (not (or (g/element? gae)
                               (and (satisfies? g/IRelationship gae)
                                    (g/relationship? gae)))))
                 (and (ru/ground? gat) (not (keyword? gat)))
                 (and (ru/ground? gae) (ru/ground? gat)
                      (not (q/member? gat (property-list gae g/mm-attributes)))))
             (ccl/fail a)

             (and (ru/ground? gae) (ru/ground? gat))
             (ccl/unify a val (g/aval gae gat))

             (ru/ground? gae)
             (ccl/to-stream
              (->> (for [an (property-list gae g/mm-attributes)]
                     (ccl/unify a [at val] [an (g/aval gae an)]))
                   (remove not)))

             :else (ccl/to-stream
                    (->> (for [elem (concat (g/elements g)
                                            (when (satisfies? g/IRelationships g)
                                              (g/relationships g)))
                               an (property-list elem g/mm-attributes)]
                           (ccl/unify a [ae at val] [elem an (g/aval elem an)]))
                         (remove not)))))))))

;;## Adjacences

(defn ^:private tmp-adjo [m o ref ro may-override]
  (fn [a]
    (let [go   (cclp/walk a o)
          gref (cclp/walk a ref)
          gro  (cclp/walk a ro)]
      (cond
        (not (tmp/tmp-or-wrapper-element? go))
        (u/errorf "tmp-adjo: o has to be a ground Tmp/WrapperElement but was %s."
                  go)

        (not (keyword? gref))
        (u/errorf "tmp-adjo: ref must be a ground keyword but was %s." gref)

        (or (and (tmp/tmp-or-wrapper-element? go) (tmp/tmp-or-wrapper-element? gro))
            (and (tmp/tmp-element? go)            (ru/fresh? gro)))
        (do (tmp/add-ref go gref ro may-override)
            (ccl/succeed a))

        (and (tmp/wrapper-element? go) (ru/fresh? gro))
        (ccl/to-stream
         (->> (map #(ccl/unify a ro (if (fn? %) (%) %))
                   (concat
                    (map #(tmp/make-wrapper m ro %)
                         (g/adjs (.wrapped-element ^WrapperElement go) gref))
                    [#(let [refed (tmp/make-tmp-element m :element)]
                        (tmp/add-ref go gref ro may-override)
                        refed)]))
              (remove not)))

        :else (u/errorf "unsupported args to tmp-adjo:\n  o = %s\n  ref = %s\n ro = %s"
                        go gref gro)))))

(defn adjo
  "A relation where vertex `rv` is in the `role` role of vertex `v` in graph
  `g`.

  The parameter `may-override` specifies that in case of a single-valued role,
  the referenced vertex may be overridden when transforming into the direction
  of `g` in a bidirectional transformation.  (You normally shouldn't use that
  parameter directly, but use the API generated by
  `generate-schema-relations`.)"
  ([g v role rv]
   (adjo g v role rv false))
  ([g v role rv may-override]
   (if tmp/*make-tmp-elements*
     (tmp-adjo g v role rv may-override)
     (fn [a]
       (let [gv    (cclp/walk a v)
             grole (cclp/walk a role)
             grv   (cclp/walk a rv)]
         (cond
           ;; Ground lvars must have the expected types
           (or (and (ru/ground? gv)    (not (g/element? gv)))
               (and (ru/ground? grole) (not (keyword? grole)))
               (and (ru/ground? grv)   (not (g/element? grv))))
           (ccl/fail a)

           (and (ru/ground? gv) (ru/ground? grole) (ru/ground? grv))
           (if (q/member? grv (g/adjs gv grole))
             (ccl/succeed a)
             (ccl/fail a))

           (and (ru/ground? gv) (ru/ground? grole))
           (ccl/to-stream
            (->> (for [refed (g/adjs gv grole)]
                   (ccl/unify a [rv] [refed]))
                 (remove not)))

           (ru/ground? gv)
           (ccl/to-stream
            (->> (for [rn (property-list gv g/mm-references)
                       refed (g/adjs gv rn)]
                   (ccl/unify a [role rv] [rn refed]))
                 (remove not)))

           :else (ccl/to-stream
                  (->> (for [s (g/elements g)
                             rn (if (ru/ground? grole)
                                  [grole]
                                  (property-list s g/mm-references))
                             t (g/adjs s rn)]
                         (ccl/unify a [v role rv] [s rn t]))
                       (remove not)))))))))

;;## Metamodel Relation Generator

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the class `c`."
  [c prefix]
  (let [n ((if (satisfies? g/IUniqueName c)
             g/uname g/qname) c)]
    (mapv (fn [s]
            (with-meta (symbol s)
              {:relation-name
               (symbol (str prefix (clojure.string/replace
                                    s #"([!])?.*[.]" #(or (nth % 1) ""))))}))
          [n (str n "!") (str "!" n) (str "!" n "!")])))

(defn ^:private create-element-relations
  "Creates relations for the given element class."
  [elc prefix]
  `(do
     ~@(for [na (class->rel-symbols elc prefix)]
         `(defn ~(:relation-name (meta na))
            ~(format "A relation where `el` is a %s element of model `m`." na)
            [~'m ~'el]
            (ccl/all
             (typeo ~'m ~'el '~na)
             (elemento ~'m ~'el))))))

(defn ^:private create-relationship-relations
  "Creates relations for the given relationship class."
  [relc prefix]
  `(do
     ~@(for [na (class->rel-symbols relc prefix)]
         `(defn ~(:relation-name (meta na))
            ~(format "A relation where `rel` is a %s relationship from `src` to `trg` in model `m`." na)
            [~'m ~'rel ~'src ~'trg]
            (ccl/all
             (typeo ~'m ~'rel '~na)
             (relationshipo ~'m ~'rel ~'src ~'trg))))))

(defn ^:private create-attr-relations
  "Creates relations for the given attribute."
  [attr classes prefix]
  ;; attr is an attr name keyword, classes the set of classes having such an attr
  (let [ts (mapv #(g/qname %) classes)]
    `(do
       (defn ~(symbol (str prefix (clojure.string/replace (name attr) "_" "-")))
         ~(format "A relation where `el` has value `val` for its %s attribute in model `m`." attr)
         [~'m ~'el ~'val]
         (ccl/all
          (typeo ~'m ~'el '~ts)
          (avalo ~'m ~'el ~attr ~'val false)))
       (defn ~(symbol (str prefix (clojure.string/replace (name attr) "_" "-") "*"))
         ~(format "A relation where `el` has value `val` for its %s attribute in model `m`.
  When used in a bidirectional transformation executed in the direction of `g`
  the attribute value may be overridden." attr)
         [~'m ~'el ~'val]
         (ccl/all
          (typeo ~'m ~'el '~ts)
          (avalo ~'m ~'el ~attr ~'val true))))))

(defn ^:private create-reference-relations
  "Creates a relation for the given role name."
  [role vcs prefix]
  (let [ts (mapv #(g/qname %) vcs)]
    `(do
       (defn ~(symbol (str prefix "->" (clojure.string/replace (name role) "_" "-")))
         ~(format "A relation where `el` references `refed` with its `%s` role in model `m`." (name role))
         [~'m ~'el ~'refed]
         (ccl/all
          (typeo ~'m ~'el '~ts)
          (adjo ~'m ~'el ~role ~'refed false)))
       ~(when (some #(not (g/mm-multi-valued-property? % role)) vcs)
          `(defn ~(symbol (str prefix "->" (clojure.string/replace (name role) "_" "-") "*"))
             ~(format "A relation where `el` references `refed` with its `%s` role in model `m`.
  When used in a bidirectional transformation executed in the direction of `m`
  the element in that single-valued role may be overridden." (name role))
             [~'m ~'el ~'refed]
             (ccl/all
              (typeo ~'m ~'el '~ts)
              (adjo ~'m ~'el ~role ~'refed true)))))))

(defmacro generate-metamodel-relations
  "Generates schema-specific relations in the namespace denoted by `nssym`.
  `mm-file` is the file defining the metamodel, i.e., a TG or Ecore file.

  If `nssym` is nil (or not given), generate them in the current namespace.
  If `nssym` was given, require that namespace as `alias`."
  ([mm-file]
   `(generate-metamodel-relations ~mm-file nil nil nil))
  ([mm-file nssym]
   `(generate-metamodel-relations ~mm-file ~nssym nil nil))
  ([mm-file nssym alias]
   `(generate-metamodel-relations ~mm-file ~nssym ~alias nil))
  ([mm-file nssym alias prefix]
   `(g/metamodel-api-generator ~mm-file
                               ~nssym
                               ~alias
                               ~prefix
                               create-element-relations
                               create-relationship-relations
                               create-attr-relations
                               create-reference-relations)))
