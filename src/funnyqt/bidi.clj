(ns funnyqt.bidi
  (:require [clojure.core.cache :as cache]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt.query :as q]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.relational.util :as ru]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]))

;; Either :left or :right
(def ^:dynamic *target-direction*)

(defn select-match [matches relation src-match]
  (when-not (seq matches)
    (u/errorf "Couldn't create a %s target match for source match: %s"
              relation src-match))
  (first matches))

(defn enforce-match [match]
  (doseq [el (vals match)
          :when (tmp/tmp-or-wrapper-element? el)]
    ;;(println "Enforcing" (tmp/as-map el))
    (tmp/manifest el)))

(defn ^:private make-kw-result-map [syms]
  (apply hash-map
         (mapcat (fn [qs]
                   [(keyword (name qs)) qs])
                 syms)))

(def ^:dynamic *target-model*)

(defn ^:private make-relation-binding-vector [syms]
  (vec (mapcat (fn [sym]
                 [sym `(or ~sym (ccl/lvar ~(name sym)))])
               syms)))

(defn ^:private make-destr-map
  ([syms]
     {:keys (vec (set syms))})
  ([syms as]
     {:keys (vec (set syms)) :as as}))

(defn replace-tmps-and-wrappers-with-manifestations [trg-match]
  (apply hash-map
         (mapcat (fn [[k v]]
                   [k (if (tmp/tmp-or-wrapper-element? v)
                        (tmp/manifestation v)
                        v)])
                 trg-match)))

(defn ^:private make-wrapper-bindings [syms]
  (vec (mapcat (fn [sym]
                 [sym `(if (p/model-object? ~sym)
                         (tmp/make-wrapper *target-model* ~sym)
                         ~sym)])
               syms)))

(defn ^:private do-rel-body [relsym trg map wsyms src-syms trg-syms]
  (let [src (if (= trg :right) :left :right)
        sm  (gensym "src-match")
        tm  (gensym "trg-match")
        etm (gensym "enforced-trg-match")]
    `(let [wfns#
           (doall
            (for [~(make-destr-map (concat wsyms src-syms) sm)
                  (ccl/run* [q#]
                    ~@(:when map)
                    ~@(get map src)
                    (ccl/== q# ~(make-kw-result-map (concat wsyms src-syms))))]
              (binding [tmp/*wrapper-cache* (or tmp/*wrapper-cache* (atom {}))]
                (let [~@(make-wrapper-bindings trg-syms)
                      ~(make-destr-map trg-syms tm)
                      (binding [tmp/*make-tmp-elements* true]
                        (select-match
                         (ccl/run* [q#]
                           ~@(get map trg)
                           (tmp/finalizeo ~@trg-syms)
                           (ccl/== q# ~(make-kw-result-map trg-syms)))
                         ~relsym ~sm))]
                  (enforce-match ~tm)
                  (let [~(make-destr-map trg-syms etm)
                        (replace-tmps-and-wrappers-with-manifestations ~tm)]
                    (swap! *relation-bindings* update-in [~relsym] conj (merge ~sm ~etm))
                    (fn [] ~@(:where map)))))))]
       (doseq [wfn# wfns#]
         (wfn#)))))

(defn ^:private adapt-subst-map
  "Adapt the current subst-map `sm` for the given included rel.
  Example:

    sm = {?entry1 ?org1, ?entry2 ?org2}
    incl-rel  = have-same-ids2
    rm        = (:?e1 ?entry1 :?e2 ?entry2)

  => {?e1 ?org1, ?e2 ?org2}"
  [sm [incl-rel & rm]]
  (let [rm (apply hash-map rm)]
    (apply hash-map
           (mapcat (fn [[k v]]
                     [(symbol (name k)) (get sm v)])
                   rm))))

(defn ^:private adapt-included-spec
  [all-rels subst-map [incl-rel & rm]]
  (let [rm (apply hash-map rm)
        update-fn (fn [subst-map spec]
                    (mapv #(replace subst-map %) spec))
        spec (get all-rels incl-rel)
        spec (update-in spec [:left] (partial update-fn subst-map))
        spec (update-in spec [:right] (partial update-fn subst-map))
        spec (if (:where spec)
               (update-in spec [:where] (partial update-fn subst-map))
               spec)
        spec (if (:when spec)
               (update-in spec [:when] (partial update-fn subst-map))
               spec)]
    (if (:includes spec)
      (apply merge-with concat
             (concat
              (map (fn [irel]
                     (adapt-included-spec all-rels (adapt-subst-map subst-map irel) irel))
                   (:includes spec))
              [spec]))
      spec)))

(defn ^:private embed-included-rels [all-rels m]
  (if-let [irels (:includes m)]
    (apply merge-with concat
           (concat
            (map (fn [irel]
                   (let [[_ & rm] irel
                         rm (apply hash-map rm)
                         subst-map (apply hash-map
                                          (mapcat (fn [[is ss]]
                                                    [(symbol (name is)) ss])
                                                  rm))]
                     (adapt-included-spec all-rels subst-map irel)))
                 irels)
            [m]))
    m))

(defn ^:private convert-relation [all-rels [relsym & more]]
  (let [m     (apply hash-map more)
        m     (embed-included-rels all-rels m)
        wsyms (distinct (filter ru/qmark-symbol? (flatten (:when m))))
        lsyms (distinct (filter ru/qmark-symbol? (flatten (:left m))))
        rsyms (distinct (filter ru/qmark-symbol? (flatten (:right m))))
        syms  (distinct (concat lsyms rsyms))]
    (when-let [unknown-keys (seq (disj (set (keys m))
                                       :left :right :when :where :includes))]
      (u/errorf "Relation contains unknown keys: %s" unknown-keys))
    `(~relsym [& ~(make-destr-map syms)]
              (let ~(make-relation-binding-vector syms)
                (if (= *target-direction* :right)
                  ~(do-rel-body relsym :right m wsyms lsyms rsyms)
                  ~(do-rel-body relsym :left  m wsyms rsyms lsyms))))))

(def ^{:dynamic true
       :doc "A map with the following structure:

    {relation1 bindings, relation2 bindings, ...}

  where bindings is:

    ({:?lsym1 val1, :?rsym1 rval2, ...}
     ...)"}
  *relation-bindings*)

(defn relateo [relation & keyvals]
  (when-not (fn? relation)
    (u/errorf "No relation (fn) given but %s %s."
              (class relation) relation))
  (let [m (apply hash-map keyvals)]
    (fn [a]
      (let [bindings (@*relation-bindings* relation)]
        (ccl/to-stream
         (->> (map (fn [b]
                     (let [vs (mapv #(get b % ::not-found) (keys m))]
                       (when (q/member? ::not-found vs)
                         (u/errorf "Unbound keys: %s"
                                   (pr-str
                                    (filter #(= (get b % ::not-found)
                                                ::not-found)
                                            (keys m)))))
                       (ccl/unify a (vec (vals m)) vs)))
                   bindings)
              (remove not)))))))

(defn ^:private mapify-relations [rels]
  (apply hash-map
         (mapcat (fn [rel]
                   [(first rel) (apply hash-map (rest rel))])
                 rels)))

(defmacro deftransformation
  "Creates a new bidirectional transformation with the given `name` on the
  models `left` and `right`.  `relations` is a sequence of relation
  definitions.  Every relation has the following form:

    (foo2bar
      :left [...]
      :right [...])

  The values of :left and :right are vectors of goals.  Logical variables with
  the same name in :left and :right define the correspondence between the left
  and right elements.  Usually, those are used for attribute values,
  e.g., (+name left left-el foo) in :left and (+id right right-el foo)
  in :right specifies that left-el's name and right-el's id values have to be
  equal.

  The semantics when transforming from left to right are: For every set of
  elements in the `left` model for which all :left goals succeed, there must be
  at least one set of elements in the `right` model for which all :right goals
  succeed.

  ^:top metadata can be added to relation names.  Such top-level relations are
  enforced automatically by the transformation in their declaration order.

  A relation spec may also contain a :when precondition:

    (foo2bar
      :when [...]
      :left [...]
      :right [...])

  It is also a vector of goals.  Usually, the goals are used to retrieve and
  bind elements created by previous relations using `relateo`.  The :left
  to :right semantics are: For every set of elements in the `left` model for
  which all :when and all :left goals succeed, there must be at least one set
  of elements in the `right` model for which all :when and :right goals
  succeed.

  A relation spec may also contain a :where postcondition:

    (foo2bar
      :left [...]
      :right [...]
      :where [...])

  It is a vector of relation-calls (not goals!).  The semantics is: For any set
  of elements in the `left` model for which a corresponding set of elements in
  the `right` model has been enforced, the relations in :where also need to be
  enforced afterwards.

  Note that the :where relations are not called until all enforcements of
  foo2bar have been applied, e.g., the evaluation is breadth-first, not
  depth-first.  That is, in the above definition, the relations in the :where
  clause may assume that foo2bar already holds for all matching elements.

  A relation spec may also contain an :includes clause:

    (foo2bar
      :includes [(a2b :?a ?foo :?b ?bar)]
      :left [(rel-with ?foo) ...]
      :right [(rel-with ?bar) ...])
    (^:abstract a2b
      :left [(rel-with ?a) ...]
      :right [(rel-with ?b) ...])

  You can think of it as a kind of relation inheritance: the foo2bar relation
  includes all :left/:right/:when/:where clauses of the a2b rule, where ?a is
  substituted by ?foo, and ?b is substituted by ?bar.  Usually, this feature is
  useful for refactoring relations on common attributes.

  A relation may include multiple other relations, and inclusion also works
  transitively.  Inclusions must not contain cycles.

  A relation that's not called explicitly in a :where clause and is only
  included by others should be declared ^:abstract like a2b above.  Then, no
  code is generated for it."

  [name [left right] & relations]
  (let [top-rels (filter #(:top (meta (first %))) relations)]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name [~left ~right dir#]
       (when-not (or (= dir# :left) (= dir# :right))
         (u/errorf "Direction parameter must either be :left or :right but was %s."
                   dir#))
       (letfn [~@(map (partial convert-relation (mapify-relations relations))
                   (remove #(:abstract (meta (first %))) relations))]
         (binding [*target-direction* dir#
                   *target-model* (if (= dir# :right) ~right ~left)
                   *relation-bindings* (atom {})]
           ~@(map (fn [r] `(~(first r))) top-rels)
           @*relation-bindings*)))))
