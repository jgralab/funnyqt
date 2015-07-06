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

(defmacro condx
  "Expands into a `conda` checking if all vars used in the questions are ground.
  If so, then use a (conda ~@clauses), else use (conde ~@clauses).
  Thus, `condx` can be used as a generator just like `conde`, but if everything
  is ground, then the use of `conda` improves the performance."
  [& clauses]
  (let [vars (mapcat (fn [c]
		       (let [f (first c)]
			 (if (list? f)
			   (rest f)
			   [])))
		     clauses)]
    `(ccl/conda
      [(all ~@(map (fn [v] `(ccl/nonlvaro ~v)) vars))
       (ccl/conda ~@clauses)]
      [ccl/succeed
       (ccl/conde ~@clauses)])))

(defn ^:private str-splits [s]
  (loop [idx 0, r []]
    (if (<= idx (count s))
      (recur (inc idx)
	     (conj r [(subs s 0 idx) (subs s idx)]))
      r)))

(defn stro
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

(defn ^:private u-or-qname [c]
  ((if (satisfies? g/IUniqueName c)
     g/uname g/qname) c))

;;## Elements

(defn tmp-elemento
  ([m el]
   (fn [a]
     (let [gel (cclp/walk a el)]
       (cond
         (not (or (ru/fresh? gel) (tmp/tmp-or-wrapper-element? gel)))
         (u/errorf "tmp-element/2: el must be fresh or a ground Wrapper/TmpElement but was %s."
                   gel)

         (ru/ground? gel)
         (if (tmp/set-kind gel :element)
           (ccl/succeed a)
           (ccl/fail a))

         :else (ccl/to-stream
                (->> (map #(ccl/unify a el %)
                          (concat
                           (map (partial tmp/make-wrapper m el)
                                (g/elements m))
                           [(tmp/make-tmp-element m :element)]))
                     (remove not)))))))
  ([m el t]
   (fn [a]
     (let [gel (cclp/walk a el)
           gt  (cclp/walk a t)]
       (cond
         (not (ru/ground? gt))
         (u/errorf "tmp-elemento/3: type must be ground.")

         (not (or (ru/fresh? gel) (tmp/tmp-or-wrapper-element? gel)))
         (u/errorf "tmp-elemento/3: el must be fresh or a ground Wrapper/TmpElement but was %s."
                   gel)

         (ru/ground? gel) ;; TODO: we probably need something like tg/kind-aec-tup-from-spec, too
         (if (and (tmp/set-kind gel :element)
                  (tmp/set-type gel gt))
           (ccl/succeed a)
           (ccl/fail a))

         :else (ccl/to-stream
                (->> (map #(ccl/unify a el %)
                          (concat
                           (map (partial tmp/make-wrapper m el)
                                (g/elements m gt))
                           [(tmp/make-tmp-element m :element gt)]))
                     (remove not))))))))

(defn elemento
  "A relation where the element `el` has the type `t` in model `m`.  In fact,
  `t` may be any type specification (see `funnyqt.generic/type-matcher`)."
  ([m el]
     (if tmp/*make-tmp-elements*
       (tmp-elemento m el)
       (fn [a]
         (let [gel (cclp/walk a el)]
           (if (ru/ground? gel)
             (if (g/element? gel) (ccl/succeed a) (ccl/fail a))
             (ccl/to-stream
              (->> (map #(ccl/unify a el %)
                        (g/elements m))
                   (remove not))))))))
  ([m el t]
     (if tmp/*make-tmp-elements*
       (tmp-elemento m el t)
       (fn [a]
         (let [gel (cclp/walk a el)
               gt (cclp/walk a t)]
           (cond
             (or (and (ru/ground? gel) (not (g/element? gel)))
                 (and (ru/ground? gt) (not (or (symbol? gt) (coll? gt)))))
             (ccl/fail a)

             (and (ru/ground? gel) (ru/ground? gt))
             (if (g/has-type? gel gt)
               (ccl/succeed a)
               (ccl/fail a))

             (ru/ground? gel)
             (ccl/unify a t (g/qname gel))

             (ru/ground? gt)
             (ccl/to-stream
              (->> (map #(ccl/unify a el %) (g/elements m gt))
                   (remove not)))

             :else (ccl/to-stream
                    (->> (for [elem (g/elements m)]
                           (ccl/unify a [el t] [elem (g/qname elem)]))
                         (remove not)))))))))

;;## Relationships

(defn tmp-relationshipo
  ([m rel src trg]
   (fn [a]
     (let [grel (cclp/walk a rel)
           gsrc (cclp/walk a src)
           gtrg (cclp/walk a trg)]
       (cond
         (not (or (ru/fresh? grel) (tmp/tmp-or-wrapper-element? grel)))
         (u/errorf "tmp-relationshipo: rel must be fresh or a ground Wrapper/TmpElement but was %s."
                   grel)

         (not (or (ru/fresh? gsrc) (tmp/tmp-or-wrapper-element? gsrc)))
         (u/errorf "tmp-relationshipo: src must be fresh or a ground Wrapper/TmpElement but was %s."
                   gsrc)

         (not (or (ru/fresh? gtrg) (tmp/tmp-or-wrapper-element? gtrg)))
         (u/errorf "tmp-relationshipo: trg must be fresh or a ground Wrapper/TmpElement but was %s."
                   gtrg)

         (tmp/wrapper-element? grel)
         (ccl/unify a [src trg]
                    (let [edge (.wrapped-element ^WrapperElement grel)]
                      [(tmp/make-wrapper m src (g/source edge))
                       (tmp/make-wrapper m trg (g/target edge))]))

         (and (ru/fresh? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
         (ccl/to-stream
          (->> (map (fn [ed]
                      (ccl/unify a rel ed))
                    (concat
                     (map (partial tmp/make-wrapper m rel)
                          (filter
                           #(= (.wrapped-element ^WrapperElement gtrg) (g/target %))
                           (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out)))
                     [(doto (tmp/make-tmp-element m :relationship)
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
          (->> (map (fn [ed-om]
                      (ccl/unify a [rel trg] ed-om))
                    (concat
                     (map (fn [ed]
                            [(tmp/make-wrapper m rel ed)
                             (tmp/make-wrapper m trg (g/target ed))])
                          (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out))
                     [(let [ed (tmp/make-tmp-element m :relationship)]
                        (tmp/set-source ed src)
                        (tmp/set-target ed trg)
                        [ed gtrg])]))
               (remove not)))

         (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc))
         (if (and (tmp/set-source grel src)
                  (tmp/set-target grel trg))
           (ccl/succeed a)
           (ccl/fail a))

         (and (ru/fresh? grel) (tmp/wrapper-element? gtrg))
         (ccl/to-stream
          (->> (map (fn [ed-al]
                      (ccl/unify a [rel src] ed-al))
                    (concat
                     (map (fn [ed]
                            [(tmp/make-wrapper m rel ed)
                             (tmp/make-wrapper m src (g/source ed))])
                          (g/incident-relationships (.wrapped-element ^WrapperElement gtrg) nil :in))
                     [(let [ed (tmp/make-tmp-element m :relationship)]
                        (tmp/set-source ed src)
                        (tmp/set-target ed trg)
                        [ed gsrc])]))
               (remove not)))

         (and (tmp/tmp-element? grel) (tmp/wrapper-element? gtrg))
         (if (and (tmp/set-source grel src)
                  (tmp/set-target grel trg))
           (ccl/succeed a)
           (ccl/fail a))

         :else (u/errorf "Can't handle (tmp-relationshipo %s %s %s %s)" m grel gsrc gtrg)))))
  ([m rel t src trg]
   (fn [a]
     (let [grel (cclp/walk a rel)
           gt   (cclp/walk a t)
           gsrc (cclp/walk a src)
           gtrg (cclp/walk a trg)]
       (cond
         (not (or (ru/fresh? grel) (tmp/tmp-or-wrapper-element? grel)))
         (u/errorf "tmp-relationshipo: rel must be fresh or a ground Wrapper/TmpElement but was %s."
                   grel)

         (not (or (ru/fresh? gsrc) (tmp/tmp-or-wrapper-element? gsrc)))
         (u/errorf "tmp-relationshipo: src must be fresh or a ground Wrapper/TmpElement but was %s."
                   gsrc)

         (not (or (ru/fresh? gtrg) (tmp/tmp-or-wrapper-element? gtrg)))
         (u/errorf "tmp-relationshipo: trg must be fresh or a ground Wrapper/TmpElement but was %s."
                   gtrg)

         (tmp/wrapper-element? grel)
         (ccl/unify a [t src trg]
                    (let [edge (.wrapped-element ^WrapperElement grel)]
                      [(g/qname edge)
                       (tmp/make-wrapper m src (g/source edge))
                       (tmp/make-wrapper m trg (g/target edge))]))

         (and (ru/fresh? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
         (ccl/to-stream
          (->> (map (fn [ed]
                      (if (ru/ground? gt)
                        (ccl/unify a rel ed)
                        (ccl/unify a [rel t] [ed (g/qname ed)])))
                    (concat
                     (map (partial tmp/make-wrapper m rel)
                          (filter
                           #(= (.wrapped-element ^WrapperElement gtrg) (g/target %))
                           (g/incident-relationships (.wrapped-element ^WrapperElement gsrc)
                                                     (when (ru/ground? gt) gt) :out)))
                     [(let [tmp-rel (tmp/make-tmp-element m :relationship)]
                        (tmp/set-source tmp-rel src)
                        (tmp/set-target tmp-rel trg)
                        (when (ru/ground? gt)
                          (tmp/set-type grel gt))
                        tmp-rel)]))
               (remove not)))

         (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
         (if (and (tmp/set-source grel src)
                  (tmp/set-target grel trg)
                  (if (ru/ground? gt)
                    (tmp/set-type grel gt)
                    true))
           (ccl/succeed a)
           (ccl/fail a))

         (and (ru/fresh? grel) (tmp/wrapper-element? gsrc))
         (ccl/to-stream
          (->> (map (fn [ed-om-t]
                      (if (and (ru/fresh? gt) (nth ed-om-t 2))
                        (ccl/unify a [rel trg t] ed-om-t)
                        (ccl/unify a [rel trg] ed-om-t)))
                    (concat
                     (map (fn [ed]
                            [(tmp/make-wrapper m rel ed)
                             (tmp/make-wrapper m trg (g/target ed))
                             (g/qname ed)])
                          (g/incident-relationships (.wrapped-element ^WrapperElement gsrc)
                                                    (when (ru/ground? gt) gt) :out))
                     [(let [ed (tmp/make-tmp-element m :relationship)]
                        (tmp/set-source ed src)
                        (tmp/set-target ed trg)
                        [ed gtrg])]))
               (remove not)))

         (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc))
         (if (and (tmp/set-source grel src)
                  (tmp/set-target grel trg))
           (ccl/succeed a)
           (ccl/fail a))

         (and (ru/fresh? grel) (tmp/wrapper-element? gtrg))
         (ccl/to-stream
          (->> (map (fn [ed-al-t]
                      (if (and (ru/fresh? gt) (nth ed-al-t 2))
                        (ccl/unify a [rel src t] ed-al-t)
                        (ccl/unify a [rel src] ed-al-t)))
                    (concat
                     (map (fn [ed]
                            [(tmp/make-wrapper m rel ed)
                             (tmp/make-wrapper m src (g/source ed))
                             (g/qname ed)])
                          (g/incident-relationships (.wrapped-element ^WrapperElement gtrg)
                                                    (when (ru/ground? gt) gt) :in))
                     [(let [ed (tmp/make-tmp-element m :relationship)]
                        (tmp/set-source ed src)
                        (tmp/set-target ed trg)
                        [ed gsrc])]))
               (remove not)))

         (and (tmp/tmp-element? grel) (tmp/wrapper-element? gtrg))
         (if (and (tmp/set-source grel src)
                  (tmp/set-target grel trg))
           (ccl/succeed a)
           (ccl/fail a))

         :else (u/errorf "Can't handle (tmp-relationshipo %s %s %s %s)" m grel gsrc gtrg))))))

(defn relationshipo
  "A relation where `rel` is a relationship in model `m` starting at `src` and
  ending at `trg`. `m` has to be ground."
  ([m rel src trg]
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
                  (->> (for [edge (g/relationships m)]
                         (ccl/unify a [rel src trg]
                                    [edge (g/source edge) (g/target edge)]))
                       (remove not))))))))
  ([m rel t src trg]
   (if tmp/*make-tmp-elements*
     (tmp-relationshipo m rel t src trg)
     (fn [a]
       (let [grel (cclp/walk a rel)
             gt   (cclp/walk a t)
             gsrc (cclp/walk a src)
             gtrg (cclp/walk a trg)]
         (cond
           (or (and (ru/ground? grel) (ru/ground? gt)
                    (not (nil? gt))   (not (g/has-type? grel t)))
               (and (ru/ground? grel) (not (g/relationship? grel)))
               (and (ru/ground? gsrc) (not (g/element? gsrc)))
               (and (ru/ground? gtrg) (not (g/element? gtrg))))
           (ccl/fail a)

           (ru/ground? grel)
           (ccl/unify a [t src trg] [(u-or-qname grel) (g/source grel) (g/target grel)])

           (ru/ground? gsrc)
           (ccl/to-stream
            (->> (map #(ccl/unify a [rel t trg] [% (u-or-qname %) (g/target %)])
                      (g/incident-relationships gsrc (when (ru/ground? gt) gt) :out))
                 (remove not)))

           (ru/ground? gtrg)
           (ccl/to-stream
            (->> (map #(ccl/unify a [rel t src] [% (u-or-qname %) (g/source %)])
                      (g/incident-relationships gtrg (when (ru/ground? gt) gt) :in))
                 (remove not)))

           :else (ccl/to-stream
                  (->> (for [edge (g/relationships m (when (ru/ground? gt) gt))]
                         (ccl/unify a [rel t src trg]
                                    [edge
                                     (if (ru/ground? gt) gt (g/qname edge))
                                     (g/source edge)
                                     (g/target edge)]))
                       (remove not)))))))))

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

(defn tmp-avalo [m o at val may-override]
  (fn [a]
    (let [go  (cclp/walk a o)
          gat (cclp/walk a at)]
      (cond
       (not (tmp/tmp-or-wrapper-element? go))
       (u/errorf "tmp-avalo: o has to be a ground Tmp/WrapperElement but was %s."
                 go)

       (not (keyword? gat))
       (u/errorf "tmp-avalo: at must be a ground keyword but was %s." gat)

       :else (do (tmp/add-attr go gat val may-override)
                 (ccl/succeed a))))))

(defn avalo
  "A relation where element (or relationship) `el` has value `val` for its `at`
  attribute in the model `m`.

  The parameter `may-override` specifies that the attribute value may be
  overridden when transforming into the direction of `m` in a bidirectional
  transformation.  (You normally shouldn't use that parameter directly, but use
  the API generated by `generate-ecore-model-relations`.)" ;; FIXME: correct macro!
  ([m el at val]
     (avalo m el at val false))
  ([m el at val may-override]
     (if tmp/*make-tmp-elements*
       (tmp-avalo m el at val may-override)
       (fn [a]
         (let [gel  (cclp/walk a el)
               gat  (cclp/walk a at)
               gval (cclp/walk a val)]
           (cond
             (or (and (ru/ground? gel) (not (or (g/element? gel)
                                                (g/relationship? gel))))
                 (and (ru/ground? gat) (not (keyword? gat)))
                 (and (ru/ground? gel) (ru/ground? gat)
                      (not (q/member? gat (property-list gel g/mm-attributes)))))
             (ccl/fail a)

             (and (ru/ground? gel) (ru/ground? gat))
             (ccl/unify a val (g/aval gel gat))

             (ru/ground? gel)
             (ccl/to-stream
              (->> (for [an (property-list gel g/mm-attributes)]
                     (ccl/unify a [at val] [an (g/aval gel an)]))
                   (remove not)))

             :else (ccl/to-stream
                    (->> (for [elem (concat (g/elements m)
                                            (when (satisfies? g/IRelationships m)
                                              (g/relationships m)))
                               an (property-list elem g/mm-attributes)]
                           (ccl/unify a [el at val] [elem an (g/aval elem an)]))
                         (remove not)))))))))

;;## Adjacences



(defn tmp-adjo [m o ref ro may-override]
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
  "A relation where `el` references `refed` with its `ref` reference in the
  model `m`.

  The parameter `may-override` specifies that in case of a single-valued
  reference, the referenced object may be overridden when transforming into the
  direction of `m` in a bidirectional transformation.  (You normally shouldn't
  use that parameter directly, but use the API generated by
  `generate-ecore-model-relations`.)" ;; FIXME: Name correct macro!
  ([m el ref refed]
   (adjo m el ref refed false))
  ([m el ref refed may-override]
   (if tmp/*make-tmp-elements*
     (tmp-adjo m el ref refed may-override)
     (fn [a]
       (let [gel    (cclp/walk a el)
             gref   (cclp/walk a ref)
             grefed (cclp/walk a refed)]
         (cond
           (or (and (ru/ground? gel)    (not (g/element? gel)))
               (and (ru/ground? gref)   (not (keyword? gref)))
               (and (ru/ground? grefed) (not (g/element? grefed)))
               (and (ru/ground? gel)    (ru/ground? gref)
                    (not (q/member? gref (property-list gel g/mm-references)))))
           (ccl/fail a)

           (and (ru/ground? gel) (ru/ground? gref))
           (ccl/to-stream
            (->> (for [refed-el (funnyqt.generic/adjs* gel gref)]
                   (ccl/unify a [refed] [refed-el]))
                 (remove not)))

           (ru/ground? gel)
           (ccl/to-stream
            (->> (for [rn (property-list gel g/mm-references)
                       refed-el (funnyqt.generic/adjs* gel rn)]
                   (ccl/unify a [ref refed] [rn refed-el]))
                 (remove not)))

           :else (ccl/to-stream
                  (->> (for [elem (g/elements m)
                             rn (property-list elem g/mm-references)
                             refed-el (funnyqt.generic/adjs* elem rn)]
                         (ccl/unify a [el ref refed] [elem rn refed-el]))
                       (remove not)))))))))

;;## Metamodel Relation Generator

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the class `c`."
  [c prefix]
  (let [n (u-or-qname c)]
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
            (elemento ~'m ~'el '~na)))))

(defn ^:private create-relationship-relations
  "Creates relations for the given relationship class."
  [relc prefix]
  `(do
     ~@(for [na (class->rel-symbols relc prefix)]
         `(defn ~(:relation-name (meta na))
            ~(format "A relation where `rel` is a %s relationship from `src` to `trg` in model `m`." na)
            [~'m ~'rel ~'src ~'trg]
            (relationshipo ~'m ~'rel '~na ~'src ~'trg)))))

(defn ^:private create-attr-relations
  "Creates relations for the given attribute."
  [attr classes prefix]
  ;; attr is an attr name keyword, classes the set of classes having such an attr
  (let [ts (mapv #(u-or-qname %) classes)]
    `(do
       (defn ~(symbol (str prefix (clojure.string/replace (name attr) "_" "-")))
         ~(format "A relation where `el` has value `val` for its %s attribute in model `m`." attr)
         [~'m ~'el ~'val]
         (avalo ~'m ~'el ~attr ~'val false))
       (defn ~(symbol (str prefix (clojure.string/replace (name attr) "_" "-") "*"))
         ~(format "A relation where `el` has value `val` for its %s attribute in model `m`.
  When used in a bidirectional transformation executed in the direction of `g`
  the attribute value may be overridden." attr)
         [~'m ~'el ~'val]
         (valueo ~'m ~'el ~attr ~'val true)))))

(defn ^:private create-reference-relations
  "Creates a relation for the given role name."
  [role vcs prefix]
  (let [ts (mapv #(u-or-qname %) vcs)]
    `(do
       (defn ~(symbol (str prefix "->" (clojure.string/replace (name role) "_" "-")))
         ~(format "A relation where `el` references `refed` with its `%s` role in model `m`." (name role))
         [~'m ~'el ~'refed]
         (adjo ~'m ~'el ~role ~'refed false))
       ~(when (some #(not (g/mm-multi-valued-property? % role)) vcs)
          `(defn ~(symbol (str prefix "->" (clojure.string/replace (name role) "_" "-") "*"))
             ~(format "A relation where `el` references `refed` with its `%s` role in model `m`.
  When used in a bidirectional transformation executed in the direction of `m`
  the element in that single-valued role may be overridden." (name role))
             [~'m ~'el ~'refed]
             (adjo ~'m ~'el ~role ~'refed true))))))

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
