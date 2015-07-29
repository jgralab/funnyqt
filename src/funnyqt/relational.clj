(ns funnyqt.relational
  "Relational Model Querying."
  (:require [clojure string walk]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt
             [generic :as g]
             [query :as q]
             [utils :as u]]
            [funnyqt.relational
             [tmp-elem :as tmp]
             [util :as ru]])
  (:import funnyqt.relational.tmp_elem.WrapperElement))

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

(defn ^:private tmp-typeo [m el-or-rel type]
  (fn [a]
    (let [gel-or-rel (cclp/walk a el-or-rel)
          gtype      (cclp/walk a type)]
      (cond
       (not (ru/ground? gtype))
       (u/errorf "tmp-typeo: type must be ground.")

       (not (or (ru/fresh? gel-or-rel)
                (tmp/tmp-or-wrapper-element? gel-or-rel)))
       (u/errorf "tmp-typeo: el-or-rel must be fresh or a ground Wrapper/TmpElement but was %s." gel-or-rel)

       (ru/ground? gel-or-rel)
       (let [[kind aec] (kind-class-tup-from-spec m gtype)]
         (if (and (tmp/set-type gel-or-rel gtype)
                  (tmp/set-kind gel-or-rel kind))
           (ccl/succeed a)
           (ccl/fail a)))

       :else (let [[kind aec] (kind-class-tup-from-spec m gtype)
                   seqfn (cond
                           (= kind :element)        g/elements
                           (= kind :relationship)   g/relationships
                           :else (fn [model type-spec]
                                   (concat (g/elements model type-spec)
                                           (when (satisfies? g/IRelationships model)
                                             (g/relationships model type-spec)))))]
               (ccl/to-stream
                (->> (map #(ccl/unify a el-or-rel (if (fn? %) (%) %))
                          (concat
                           ;; Existing vertices/edges wrapped
                           (map (partial tmp/make-wrapper m el-or-rel)
                                (seqfn m gtype))
                           ;; One new vertex/edge tmp element
                           [#(tmp/make-tmp-element m kind gtype)]))
                     (remove not))))))))

(defn typeo
  "A relation where in model `m` the element or relationship `el-or-rel` has
  the given `type`.  In fact, `type` may be any type specification (see
  `funnyqt.generic/type-matcher`).  The model `m` must be ground."
  [m el-or-rel type]
  (if tmp/*make-tmp-elements*
    (tmp-typeo m el-or-rel type)
    (fn [a]
      (let [gel-or-rel (cclp/walk a el-or-rel)
            gtype      (cclp/walk a type)]
        (cond
          (or (and (ru/ground? gel-or-rel)
                   (not (or (g/element? gel-or-rel) (g/relationship? gel-or-rel))))
              (and (ru/ground? gtype) (not (or (symbol? gtype) (coll? gtype)))))
          (ccl/fail a)

          (and (ru/ground? gel-or-rel) (ru/ground? gtype))
          (if (g/has-type? gel-or-rel gtype) (ccl/succeed a) (ccl/fail a))

          (ru/ground? gel-or-rel)
          (ccl/unify a type (g/qname gel-or-rel))

          (ru/ground? gtype)
          (if (symbol? gtype)
            ;; Ok, here we can determine if its a element or a relationship
            ;; class
            (let [[_ tn _] (u/type-with-modifiers (name gtype))
                  mm-cls   (g/mm-class m tn)]
              (cond
                (g/mm-element-class? mm-cls)
                (ccl/to-stream
                 (->> (map #(ccl/unify a el-or-rel %) (g/elements m gtype))
                      (remove not)))
                ;;---
                (g/mm-relationship-class? mm-cls)
                (ccl/to-stream
                 (->> (map #(ccl/unify a el-or-rel %) (g/relationships m gtype))
                      (remove not)))
                ;;---
                :else (u/errorf "%s is neither element nor relationship class." mm-cls)))
            (ccl/to-stream
             (->> (map #(ccl/unify a el-or-rel %)
                       (concat (g/elements m gtype)
                               (when (satisfies? g/IRelationships m)
                                 (g/relationships m gtype))))
                  (remove not))))

          :else (ccl/to-stream
                 (->> (for [elem (concat (g/elements m)
                                         (when (satisfies? g/IRelationships m)
                                           (g/relationships m)))]
                        (ccl/unify a [el-or-rel type] [elem (g/qname elem)]))
                      (remove not))))))))

;;## Element Relation

(defn ^:private tmp-elemento [m el]
  (fn [a]
    (let [gel (cclp/walk a el)]
      (cond
       (not (or (ru/fresh? gel)
                (tmp/tmp-or-wrapper-element? gel)))
       (u/errorf "tmp-elemento: el must be fresh or a ground Wrapper/TmpElement but was %s."
                 gel)

       (ru/ground? gel)
       (if (tmp/set-kind gel :element)
         (ccl/succeed a)
         (ccl/fail a))

       :else (ccl/to-stream
              (->> (map #(ccl/unify a el (if (fn? %) (%) %))
                        (concat
                         ;; Existing vertices wrapped
                         (map (partial tmp/make-wrapper m el)
                              (g/elements m))
                         ;; One new vertex tmp element
                         [#(tmp/make-tmp-element m :element)]))
                   (remove not)))))))

(defn elemento
  "A relation where `el` is an element of model `m`.
  `m` has to be ground."
  [m el]
  (if tmp/*make-tmp-elements*
    (tmp-elemento m el)
    (fn [a]
      (let [gel (cclp/walk a el)]
        (if (ru/ground? gel)
          ;; TODO: Maybe we also want a contains check here, e.g., is the
          ;; element contained in this model?
          (if (g/element? gel)
            (ccl/succeed a)
            (ccl/fail a))
          (ccl/to-stream
           (->> (map #(ccl/unify a el %) (g/elements m))
                (remove not))))))))

;;## Relationships

(defn ^:private tmp-relationshipo [m rel src trg]
  (fn [a]
    (let [grel (cclp/walk a rel)
          gsrc (cclp/walk a src)
          gtrg (cclp/walk a trg)]
      (cond
       (not (or (ru/fresh? grel) (tmp/tmp-or-wrapper-element? grel)))
       (u/errorf "tmp-relationshipo: e must be fresh or a ground Wrapper/TmpElement but was %s."
                 grel)

       (not (or (ru/fresh? gsrc) (tmp/tmp-or-wrapper-element? gsrc)))
       (u/errorf "tmp-relationshipo: alpha must be fresh or a ground Wrapper/TmpElement but was %s."
                 gsrc)

       (not (or (ru/fresh? gtrg) (tmp/tmp-or-wrapper-element? gtrg)))
       (u/errorf "tmp-relationshipo: omega must be fresh or a ground Wrapper/TmpElement but was %s."
                 gtrg)

       (tmp/wrapper-element? grel)
       (ccl/unify a [src trg]
                    (let [wrel (.wrapped-element ^WrapperElement grel)]
                      [(tmp/make-wrapper m src (g/source wrel))
                       (tmp/make-wrapper m trg (g/target wrel))]))

       (and (ru/fresh? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
       (ccl/to-stream
        (->> (map #(ccl/unify a rel (if (fn? %) (%) %))
                  (concat
                   (map (partial tmp/make-wrapper m rel)
                        (filter
                         #(= (.wrapped-element ^WrapperElement gtrg) (g/target %))
                         (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out)))
                   [#(doto (tmp/make-tmp-element m :relationship)
                       (tmp/set-source src)
                       (tmp/set-target trg))]))
             (remove not)))

       (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
       (if (and (tmp/set-source grel src)
                (tmp/set-target grel trg))
         (ccl/succeed a)
         (ccl/fail a))

       (and (ru/fresh? grel) (tmp/wrapper-element? gsrc))
       (ccl/to-stream
        (->> (map #(ccl/unify a [rel trg] (if (fn? %) (%) %))
                  (concat
                   (map (fn [orel]
                          [(tmp/make-wrapper m rel orel)
                           (tmp/make-wrapper m trg (g/target orel))])
                        (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out))
                   [#(let [trel (tmp/make-tmp-element m :relationship)]
                       (tmp/set-source trel src)
                       (tmp/set-target trel trg)
                       [trel gtrg])]))
             (remove not)))

       (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc))
       (if (and (tmp/set-source grel src)
                (tmp/set-target grel trg))
         (ccl/succeed a)
         (ccl/fail a))

       (and (ru/fresh? grel) (tmp/wrapper-element? gtrg))
       (ccl/to-stream
        (->> (map #(ccl/unify a [rel src] (if (fn? %) (%) %))
                  (concat
                   (map (fn [orel]
                          [(tmp/make-wrapper m rel orel)
                           (tmp/make-wrapper m src (g/source orel))])
                        (g/incident-relationships (.wrapped-element ^WrapperElement gtrg) nil :in))
                   [#(let [trel (tmp/make-tmp-element m :relationship)]
                       (tmp/set-source trel src)
                       (tmp/set-target trel trg)
                       [trel gsrc])]))
             (remove not)))

       (and (tmp/tmp-element? grel) (tmp/wrapper-element? gtrg))
       (if (and (tmp/set-source grel src)
                (tmp/set-target grel trg))
         (ccl/succeed a)
         (ccl/fail a))

       :else (u/errorf "Can't handle (tmp-relationshipo %s %s %s %s)" m grel gsrc gtrg)))))

(defn relationshipo
  "A relation where `rel` is a relationship in model `m` starting at element
  `src` and ending at element `trg`.  `m` has to be ground."
  [m rel src trg]
  (when-not (satisfies? g/IRelationships m)
    (u/errorf "Cannot use relationshipo with model %s which doesn't support relationships."
              m))
  (if tmp/*make-tmp-elements*
    (tmp-relationshipo m rel src trg)
    (fn [a]
      (let [grel (cclp/walk a rel)
            gsrc (cclp/walk a src)
            gtrg (cclp/walk a trg)]
        (cond
         (or (and (ru/ground? grel) (not (g/relationship? grel)))
             (and (ru/ground? gsrc) (not (g/element? gsrc)))
             (and (ru/ground? gtrg) (not (g/element? gtrg))))
         (ccl/fail a)

         (ru/ground? grel)
         (ccl/unify a [src trg] [(g/source grel) (g/target grel)])

         (ru/ground? gsrc)
         (ccl/to-stream
          (->> (map #(ccl/unify a [rel trg] [% (g/target %)])
                    (g/incident-relationships gsrc nil :out))
               (remove not)))

         (ru/ground? gtrg)
         (ccl/to-stream
          (->> (map #(ccl/unify a [rel src] [% (g/source %)])
                    (g/incident-relationships gtrg nil :in))
               (remove not)))

         :else (ccl/to-stream
                (->> (for [orel (g/relationships m)]
                       (ccl/unify a [rel src trg]
                                  [orel (g/source orel) (g/target orel)]))
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

(defn ^:private tmp-avalo [m el-or-rel attr val may-override]
  (fn [a]
    (let [gel-or-rel (cclp/walk a el-or-rel)
          gattr      (cclp/walk a attr)]
      (cond
       (not (tmp/tmp-or-wrapper-element? gel-or-rel))
       (u/errorf "tmp-valueo: o has to be a ground Tmp/WrapperElement but was %s."
                 gel-or-rel)

       (not (keyword? gattr))
       (u/errorf "tmp-valueo: at must be a ground keyword but was %s." gattr)

       :else (do (tmp/add-attr gel-or-rel gattr val may-override)
                 (ccl/succeed a))))))


(defn avalo
  "A relation where model `m`s element or relationship `el-or-rel` has value
  `val` for its `attr` attribute.

  The parameter `may-override` specifies that the attribute value may be
  overridden when transforming into the direction of `m` in a bidirectional
  transformation.  (You normally shouldn't use that parameter directly but use
  the API generated by `generate-metamodel-relations`.)"
  ([m el-or-rel attr val]
   (avalo m el-or-rel attr val false))
  ([m el-or-rel attr val may-override]
   (if tmp/*make-tmp-elements*
     (tmp-avalo m el-or-rel attr val may-override)
     (fn [a]
       (let [gel-or-rel (cclp/walk a el-or-rel)
             gattr      (cclp/walk a attr)
             gval       (cclp/walk a val)]
         (cond
           (or (and (ru/ground? gel-or-rel)
                    (not (or (g/element? gel-or-rel)
                             (and (satisfies? g/IRelationship gel-or-rel)
                                  (g/relationship? gel-or-rel)))))
               (and (ru/ground? gattr) (not (keyword? gattr)))
               (and (ru/ground? gel-or-rel) (ru/ground? gattr)
                    (not (q/member? gattr (property-list gel-or-rel g/mm-attributes)))))
           (ccl/fail a)

           (and (ru/ground? gel-or-rel) (ru/ground? gattr))
           (ccl/unify a val (g/aval gel-or-rel gattr))

           (ru/ground? gel-or-rel)
           (ccl/to-stream
            (->> (for [an (property-list gel-or-rel g/mm-attributes)]
                   (ccl/unify a [attr val] [an (g/aval gel-or-rel an)]))
                 (remove not)))

           :else (ccl/to-stream
                  (->> (for [oel-or-rel (concat (g/elements m)
                                                (when (satisfies? g/IRelationships m)
                                                  (g/relationships m)))
                             an (property-list oel-or-rel g/mm-attributes)]
                         (ccl/unify a [el-or-rel attr val] [oel-or-rel an (g/aval oel-or-rel an)]))
                       (remove not)))))))))

;;## Adjacences

(defn ^:private tmp-adjo [m el ref refed-el may-override]
  (fn [a]
    (let [gel       (cclp/walk a el)
          gref      (cclp/walk a ref)
          grefed-el (cclp/walk a refed-el)]
      (cond
        (not (tmp/tmp-or-wrapper-element? gel))
        (u/errorf "tmp-adjo: o has to be a ground Tmp/WrapperElement but was %s."
                  gel)

        (not (keyword? gref))
        (u/errorf "tmp-adjo: ref must be a ground keyword but was %s." gref)

        (or (and (tmp/tmp-or-wrapper-element? gel) (tmp/tmp-or-wrapper-element? grefed-el))
            (and (tmp/tmp-element? gel)            (ru/fresh? grefed-el)))
        (do (tmp/add-ref gel gref refed-el may-override)
            (ccl/succeed a))

        (and (tmp/wrapper-element? gel) (ru/fresh? grefed-el))
        (ccl/to-stream
         (->> (map #(ccl/unify a refed-el (if (fn? %) (%) %))
                   (concat
                    (map #(tmp/make-wrapper m refed-el %)
                         (g/adjs (.wrapped-element ^WrapperElement gel) gref))
                    [#(let [refed (tmp/make-tmp-element m :element)]
                        (tmp/add-ref gel gref refed-el may-override)
                        refed)]))
              (remove not)))

        :else (u/errorf "unsupported args to tmp-adjo:\n  o = %s\n  ref = %s\n ro = %s"
                        gel gref grefed-el)))))

(defn adjo
  "A relation where element `el` references `refed-el` with its `ref` reference
  in model `m`.

  The parameter `may-override` specifies that in case of a single-valued role,
  the referenced element may be overridden when transforming into the direction
  of `m` in a bidirectional transformation.  (You normally shouldn't use that
  parameter directly but use the API generated by
  `generate-metamodel-relations`.)"
  ([m el ref refed-el]
   (adjo m el ref refed-el false))
  ([m el ref refed-el may-override]
   (if tmp/*make-tmp-elements*
     (tmp-adjo m el ref refed-el may-override)
     (fn [a]
       (let [gel       (cclp/walk a el)
             gref      (cclp/walk a ref)
             grefed-el (cclp/walk a refed-el)]
         (cond
           ;; Ground lvars must have the expected types
           (or (and (ru/ground? gel)    (not (g/element? gel)))
               (and (ru/ground? gref) (not (keyword? gref)))
               (and (ru/ground? grefed-el)   (not (g/element? grefed-el))))
           (ccl/fail a)

           (and (ru/ground? gel) (ru/ground? gref) (ru/ground? grefed-el))
           (if (q/member? grefed-el (g/adjs gel gref))
             (ccl/succeed a)
             (ccl/fail a))

           (and (ru/ground? gel) (ru/ground? gref))
           (ccl/to-stream
            (->> (for [orefed-el (g/adjs gel gref)]
                   (ccl/unify a [refed-el] [orefed-el]))
                 (remove not)))

           (ru/ground? gel)
           (ccl/to-stream
            (->> (for [rn (property-list gel g/mm-references)
                       orefed-el (g/adjs gel rn)]
                   (ccl/unify a [ref refed-el] [rn orefed-el]))
                 (remove not)))

           :else (ccl/to-stream
                  (->> (for [oel (g/elements m)
                             rn (if (ru/ground? gref)
                                  [gref]
                                  (property-list oel g/mm-references))
                             orefed-el (g/adjs oel rn)]
                         (ccl/unify a [el ref refed-el] [oel rn orefed-el]))
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
  "Generates metamodel-specific relations in the namespace denoted by `nssym`.
  `mm-file` is the file defining the metamodel, i.e., a TG or Ecore file.

  If `nssym` is nil (or not given), generate them in the current namespace.  If
  `nssym` was given, require that namespace as `alias`.  An optional `prefix`
  is supported for prefixing the generated var names.  This can be handy when
  generating into the current namespace to achieve uniqueness but it is
  generally better to generate into a fresh namespace and then use a short
  alias."
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
