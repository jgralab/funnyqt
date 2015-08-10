(ns funnyqt.bidi
  (:require [clojure.core.logic :as ccl]
            [clojure.tools.macro :as tm]
            [clojure.walk :as cw]
            [flatland.ordered.map :as om]
            [funnyqt
             [generic :as g]
             [utils :as u]]
            [funnyqt.relational
             [tmp-elem :as tmp]
             [util :as ru]]))

(def ^{:dynamic true
       :doc "Only for internal use.
  The current direction of the transformation execution.
  Either :left or :right."}
  *target-direction*)

(def ^{:dynamic true
       :doc "Only for internal use.
  A map with the following structure:

    {:related   {relation1 bindings, relation2 bindings, ...}
     :unrelated {relation1 bindings, relation2 bindings, ...}}

  where relationN is a keyword denoting a t-relation and bindings is:

    ({:?lsym1 lval1, :?rsym1 rval2, ...}
     ...)

  Access this information with the relation `relateo`."}
  *relation-bindings*)

(defn select-match
  "Only for internal use.
  Simply returns the first match.  Throws an exception if there's none."
  [matches relation src-match]
  (when-not (seq matches)
    (u/errorf "Couldn't create a %s target match for source match: %s"
              (.getSimpleName (class relation)) src-match))
  (first matches))

(defn enforce-match
  "Only for internal use.
  Manifests the temporary and wrapper elements in `match`."
  [match]
  (doseq [el (vals match)
          :when (tmp/tmp-or-wrapper-element? el)]
    ;;(println "Enforcing" (tmp/as-map el))
    (tmp/manifest el)))

(defn ^:private make-kw-result-map [syms]
  (apply hash-map
         (mapcat (fn [qs]
                   [(keyword (name qs)) qs])
                 syms)))

(def ^{:dynamic true
       :doc "Only for internal use.
  The current target model of the transformation execution.
  Depends on `*target-direction*, so it's either the :right or the :left modes."}
  *target-model*)

(defn ^:private make-destr-map [syms as]
  {:keys (vec (set syms)) :as as})

(defn replace-tmps-and-wrappers-with-manifestations
  "Only for internal use."
  [trg-match]
  (apply hash-map
         (mapcat (fn [[k v]]
                   [k (if (tmp/tmp-or-wrapper-element? v)
                        (tmp/manifestation v)
                        v)])
                 trg-match)))

(defn ^:private valid-spec-vector? [spec goals]
  (or (nil? spec)
      (and (coll? spec)
           (let [f (first spec)]
             (or (seq? f)
                 (when goals
                   (and (symbol? f)
                        (or (= "succeed" (name f))
                            (= "fail"    (name f))))))))))

(defn ^:private insert-debug
  "If code is non-nil, wrap it in a vector for being spliced in."
  [code]
  (when code [code]))

(defn src-initializeo
  "Only for internal use."
  [args-map & lvars]
  (fn [a]
    (ccl/unify a (vec lvars)
               (mapv (fn [lv]
                       (let [val (get args-map (keyword (:oname lv)) ::unknown)]
                         (if (= val ::unknown) lv val)))
                     lvars))))

(defn ^:private maybe-wrap
  "Wraps `val` in bound to the logic variable `lv` in a WrapperElement if it is
  a model object.  Else returns `val` unchanged.
  Only for internal use."
  [lv val]
  (if (or (g/element? val) (g/relationship? val))
    (tmp/make-wrapper *target-model* lv val)
    val))

(defn trg-initializeo
  "Only for internal use."
  [enforcing src-match args-map & lvars]
  (fn [a]
    (ccl/unify a (vec lvars)
               (mapv (fn [lv]
                       (let [lv-kw    (keyword (:oname lv))
                             src-val  (get src-match lv-kw ::unknown)
                             args-val (get args-map  lv-kw ::unknown)]
                         (cond
                           (not= src-val  ::unknown)
                           (if enforcing
                             (maybe-wrap lv src-val)
                             src-val)

                           (not= args-val ::unknown)
                           (if enforcing
                             (maybe-wrap lv args-val)
                             args-val)

                           :else lv)))
                     lvars))))

(defn ^:private do-rel-body [relsym trg map wsyms src-syms trg-syms args-map]
  (let [src  (if (#{:right :right-checkonly} trg) :left :right)
        enforcing (#{:left :right} trg)
        trg  (if (#{:right :right-checkonly} trg) :right :left)
        sm   (gensym "src-match")
        tm   (gensym "trg-match")
        etm  (gensym "enforced-trg-match")
        wfns (gensym "where-fns")]
    (doseq [kw [:when :left :right]]
      (when-not (valid-spec-vector? (kw map) true)
        (u/errorf "Error in %s: %s has to be a vector of goals but was %s."
                  relsym kw (kw map))))
    (doseq [kw [:where :extends]]
      (when-not (valid-spec-vector? (kw map) true)
        (u/errorf "Error in %s: %s has to be a vector of relation calls but was %s."
                  relsym kw (kw map))))
    `(fn []
       (let [~wfns (doall
                    (remove nil?
                            (u/for-1 [~(make-destr-map (concat wsyms src-syms) sm)
                                      (ccl/run* [q#]
                                        (ccl/fresh [~@(set (concat wsyms src-syms))]
                                          (src-initializeo ~args-map ~@(set (concat wsyms src-syms)))
                                          ;; TODO: Sometimes it's faster if :when goals are after
                                          ;; source goals, and sometimes it's the other way round.
                                          ;; Maybe the user should be able to annotate the :when
                                          ;; clause with ^:last in order to force it to come after
                                          ;; the source goals.  Well, but for some relations,
                                          ;; changing the order is not semantically equivalent.
                                          ;; That's the case if :when binds ?foo, and the target
                                          ;; clause starts with (->role model ?foo ?bar).
                                          ~@(:when map)
                                          ~@(get map src)
                                          (ccl/== q# ~(make-kw-result-map (concat wsyms src-syms)))))]
                              ~(if enforcing
                                 ;; Enforcement mode
                                 `(binding [tmp/*wrapper-cache* (atom {})]
                                    ~@(insert-debug (:debug-src map))
                                    (let [~(make-destr-map trg-syms tm)
                                          (binding [tmp/*make-tmp-elements* true]
                                            (select-match
                                             (ccl/run 1 [q#]
                                               (ccl/fresh [~@trg-syms]
                                                 (trg-initializeo true ~sm ~args-map ~@trg-syms)
                                                 ~@(get map trg)
                                                 (tmp/finalizeo ~@trg-syms)
                                                 (ccl/== q# ~(make-kw-result-map trg-syms))))
                                             ~relsym ~sm))]
                                      ~@(insert-debug (:debug-trg map))
                                      (enforce-match ~tm)
                                      (let [~(make-destr-map trg-syms etm)
                                            (replace-tmps-and-wrappers-with-manifestations ~tm)]
                                        (swap! *relation-bindings* update-in [:related ~(keyword relsym)]
                                               (fn [current# new#]
                                                 (conj (or current# #{}) new#))
                                               (merge ~sm ~etm))
                                        ~@(insert-debug (:debug-enforced map))
                                        (fn [] ~@(:where map)))))
                                 ;; Checkonly mode
                                 `(do
                                    ~@(insert-debug (:debug-src map))
                                    (let [match# (first
                                                  (ccl/run 1 [q#]
                                                    (ccl/fresh [~@trg-syms]
                                                      (trg-initializeo false ~sm ~args-map ~@trg-syms)
                                                      ~@(get map trg)
                                                      (ccl/== q# ~(make-kw-result-map trg-syms)))))
                                          ~(make-destr-map trg-syms tm) match#]
                                      ~@(insert-debug (:debug-trg map))
                                      (if match#
                                        (do
                                          (swap! *relation-bindings* update-in [:related ~(keyword relsym)]
                                                 (fn [current# new#]
                                                   (conj (or current# #{}) new#))
                                                 (merge ~sm ~tm))
                                          (fn [] ~@(:where map)))
                                        (do
                                          (swap! *relation-bindings* update-in [:unrelated ~(keyword relsym)]
                                                 (fn [current# new#]
                                                   (conj (or current# #{}) new#))
                                                 ~sm)
                                          nil))))))))]
         (doseq [wfn# ~wfns]
           (wfn#))))))

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
                    (mapv #(cw/postwalk-replace subst-map %) spec))
        spec (get all-rels incl-rel)
        spec (update-in spec [:left] (partial update-fn subst-map))
        spec (update-in spec [:right] (partial update-fn subst-map))
        spec (if (:where spec)
               (update-in spec [:where] (partial update-fn subst-map))
               spec)
        spec (if (:when spec)
               (update-in spec [:when] (partial update-fn subst-map))
               spec)]
    (if (:extends spec)
      (apply merge-with concat
             (concat
              (map (fn [irel]
                     (adapt-included-spec all-rels (adapt-subst-map subst-map irel) irel))
                   (:extends spec))
              [spec]))
      spec)))

(defn ^:private embed-included-rels [all-rels m]
  (if-let [irels (:extends m)]
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

(defn check-args
  "Only for internal use.
  Check if the provided args in arg-map are valid (that is, in good-args)."
  [relsym arg-map good-args]
  (when-let [unbound-key (some #(when-not (good-args %) %) (keys arg-map))]
    (u/errorf "Unbound keyword arg %s when calling relation %s."
              unbound-key relsym)))

(defn ^:private convert-relation [all-rels [relsym m]]
  (let [m     (embed-included-rels all-rels m)
        wsyms (distinct (filter ru/qmark-symbol? (flatten (:when m))))
        lsyms (distinct (filter ru/qmark-symbol? (flatten (:left m))))
        rsyms (distinct (filter ru/qmark-symbol? (flatten (:right m))))
        syms  (distinct (concat lsyms rsyms wsyms))
        args-map (gensym "args-map")]
    (when-let [unknown-keys (seq (disj (set (keys m))
                                       :left :right :when :where :extends
                                       :debug-entry :debug-src :debug-trg
                                       :debug-enforced))]
      (u/errorf "Relation contains unknown keys: %s" unknown-keys))
    `(~relsym [& ~(make-destr-map syms args-map)]
              (check-args '~relsym ~args-map ~(set (map keyword syms)))
              ~@(insert-debug (:debug-entry m))
              (let [transform-fns# {:right ~(do-rel-body relsym :right m wsyms lsyms rsyms args-map)
                                    :right-checkonly ~(do-rel-body relsym :right-checkonly m wsyms lsyms rsyms args-map)
                                    :left  ~(do-rel-body relsym :left  m wsyms rsyms lsyms args-map)
                                    :left-checkonly  ~(do-rel-body relsym :left-checkonly  m wsyms rsyms lsyms args-map)}]
                ((transform-fns# *target-direction*))))))

(defn relateo
  "A relation that succeeds if there's a correspondence between `keyvals` in
  `relation` (given as keyword).  `keyvals` is a sequence of keywords with
  values that relate elements from the left and right domains of `relation`.

  Example:

    (relateo :class2table :?class ?subclass :?table ?subtable)"
  [relation & keyvals]
  (when-not (keyword? relation)
    (u/errorf "The relation has to be given as keyword but got %s."
              relation))
  (let [m (apply hash-map keyvals)]
    (fn [a]
      (let [bindings ((:related @*relation-bindings*) relation)]
        (ccl/to-stream
         (->> (map (fn [b]
                     (let [vs (mapv #(let [el (get b % ::not-found)]
                                       (if (= ::not-found el)
                                         (u/errorf "Unbound key: %s" %)
                                         el))
                                    (keys m))]
                       (ccl/unify a (vec (vals m)) vs)))
                   bindings)
              (remove not)))))))

(defn target-directiono
  "A relation that succeeds if the transformation direction is `dir`.
  Can be used for conditionalizing non-bijective transformations, e.g., when
  one wants some t-relation to be only enforced in one direction.  Then, just
  put (target-directiono :left) in a :when clause."
  [dir]
  (when-not (#{:right :left :right-checkonly :left-checkonly} dir)
    (u/errorf "dir must be :left or :right but was %s." dir))
  (fn [a]
    (if (= *target-direction* dir)
      (ccl/succeed a)
      (ccl/fail a))))

(defn ^:private mapify-trelations [rels]
  (into (om/ordered-map)
        (map (fn [rel]
               [(first rel) (apply hash-map (rest rel))])
             rels)))

(defn ^:private mapify-prelations [rels]
  (into (om/ordered-map)
        (map (fn [rel]
               [(first rel) rel])
             rels)))

(defmacro deftransformation
  "Creates a new bidirectional transformation with the given `name` on the
  models `left` and `right` with possibly additional `args`.  The signature of
  the defined transformation (a plain function) is

    (transformation-name left-model right-model direction & args)

  `direction` is the direction in which to execute the transformation,
  either :right or :left for enforcement mode, or :right-checkonly
  or :left-checkonly for checkonly mode.

  Defining Transformation Relations
  =================================

  In the transformation specification, `relations` is a sequence of
  transformation relation definitions.  Every such t-relation has the following
  form:

    (foo2bar
      :left [...]
      :right [...])

  The values of :left and :right are vectors of goals.  Logical variables with
  the same name in :left and :right define the correspondence between the left
  and right elements.  Logical variables have to start with a question mark.
  Those are used for elements and attribute values, e.g., (+name left ?left-el
  ?foo) in :left and (+id right ?right-el ?foo) in :right specifies that
  ?left-el's name and ?right-el's id values have to be equal.

  The semantics when transforming from left to right are: For every set of
  elements in the `left` model for which all :left goals succeed, there must be
  at least one set of elements in the `right` model for which all :right goals
  succeed.

  ^:top metadata can be added to t-relation names.  Such top-level t-relations
  are enforced automatically by the transformation in their declaration order.

  Preconditions
  =============

  A relation spec may also contain a :when precondition:

    (foo2bar
      :when [...]
      :left [...]
      :right [...])

  It is also a vector of goals.  Usually, the goals are used to retrieve and
  bind elements created by previous relations using `relateo`.  The :left
  to :right semantics are: For all solutions satisfying the conjunction
  of :left and :when goals, a corresponding solution of the :right goals has to
  exist, i.e., the :when goals are simply added to the clause of the current
  source direction.

  Postconditions
  ==============

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

  Both :left and :right are optional.  If omitted, they are equivalent to
  `:left [ccl/succeed]` and `:right [ccl/succeed]`, i.e., they simply succeed.
  Clearly, omitting those clauses makes only sense if there're :when and :where
  clauses.  For example, there might be a final ^:top relation that matches
  elements only in terms of `:when [(relateo ...)...]` and then invokes other
  relations using its :where clause.

  Transformation Relation Inheritance
  ===================================

  A t-relation spec may also contain an :extends clause:

    (foo2bar
      :extends [(a2b :?a ?foo :?b ?bar)]
      :left [(rel-with l ?foo) ...]
      :right [(rel-with r ?bar) ...])
    (^:abstract a2b
      :left [(rel-with l ?a) ...]
      :right [(rel-with r ?b) ...])

  The foo2bar relation includes all :left/:right/:when/:where clauses of the
  a2b rule, where ?a is substituted by ?foo, and ?b is substituted by ?bar.
  Usually, this feature is useful for refactoring relations on common
  attributes.

  A t-relation may extend multiple other relations, and extension works
  transitively.  The mustn't be extension cycles.

  A t-relation that's not called explicitly in a :where clause and is only
  extended by others should be declared ^:abstract like a2b above.  Then, no
  code is generated for it.

  Return Value and Traceability
  =============================

  The return value of the transformation is its complete traceability
  information represented as a map with the following structure:

    {:related   {:t-relation1 bindings, :t-relation2 bindings, ...}
     :unrelated {:t-relation1 bindings, :t-relation2 bindings, ...}}

  The map being the value of the :related key contains the actual traceability
  information, i.e., it is a map from the transformation's t-relations (as
  keywords) to the corresponding bindings in the left and right models.  This
  information can be accessed during the transformation in terms of `relateo`.

  The map being the value of the :unrelated key contains the bindings for which
  no corresponding target model match exists.  This map is only populated in
  checkonly mode since in enforcement mode, target elements are created or
  modified in order to guarantee the existence of a match.

  Debugging Transformation Relations
  ==================================

  For debugging purposes, relations may also contain the following clauses:

    :debug-entry    (println ...) ;; code being executed when a relation is invoked
    :debug-src      (println ...) ;; code being executed when the source domain
                                  ;; and :when clause has been matched
    :debug-trg      (println ...) ;; code being executed when the target domain has
                                  ;; been matched
    :debug-enforced (println ...) ;; code being executed when the target domain has
                                  ;; been enforced

  The value may be arbitrary forms that are inserted at the corresponding
  places.

  Plain Relations
  ===============

  A bidirectional transformation definition may also define plain, local
  relations which can then be used inside the :left, :right, and :when clauses
  of t-relations.  The syntax is that of local functions as per `letfn`.

    (my-relation [arg1 arg2 ...]
      (all
        (goal1 arg1)
        (goal2 arg2 arg1 ...)))

  Of course, plain relation may also be specified outside of the
  transformation.

  Transformation Inheritance
  ==========================

  A transformation may extend other transformations using an :extends clause
  following the argument vector.  Its value is a symbol or a vector of symbols
  denoting other bidirectional transformations.

    (deftransformation a2b [l r]
      :extends a2b-base
      ...)

  The transformation a2b extends a2b-base here.  This means that a2b contains
  its own relations plus the ones defined in a2b-base.  If a2b defines a
  relation that's already defined by a2b-base, then a2b's version overrides the
  version from a2b-base.  Likewise, a2b calls all top-level relations defined
  by itself and a2b-base.

  a2b is allowed to use different names for the left and right model.  However,
  when overriding a relation, the logical variables (those beginning with
  question mark) should have the same names as in the overridden relation, at
  least if some other inherited relation calls the overridden relation
  in :where or accesses its trace in terms of `relateo` in :when.  That's
  because the logical variables act as named parameters and are also
  represented in the transformation trace."

  {:arglists '([name [left right & args] extends-clause? & relations])}
  [name & more]
  (let [[name more] (tm/name-with-attributes name more)
        [left right & other-args] (first more)
        more (next more)
        [extended more] (if (= :extends (first more))
                          [(fnext more) (nnext more)]
                          [nil          more])
        relations more
        [trelations prelations] ((juxt remove filter) #(vector? (nth % 1)) relations)
        trelations (mapify-trelations trelations)
        prelations (mapify-prelations prelations)
        [trelations prelations] (if extended
                                  (do
                                    (when-not (or (symbol? extended)
                                                  (and (vector? extended)
                                                       (every? symbol? extended)))
                                      (u/errorf (str "The value of :extends must be a symbol or a "
                                                     "vector of symbols denoting bidi transformations: "
                                                     "%s")
                                                extended))
                                    (let [extended (if (coll? extended) extended [extended])]
                                      [(apply merge (conj (mapv (fn [ex]
                                                                  (let [m (meta (resolve ex))]
                                                                    (cw/prewalk-replace
                                                                     {(::left-model-name m) left
                                                                      (::right-model-name m) right}
                                                                     (::trelations m))))
                                                                extended)
                                                          trelations))
                                       (apply merge (conj (mapv (fn [ex]
                                                                  (::prelations (meta (resolve ex))))
                                                                extended)
                                                          prelations))]))
                                  [trelations prelations])
        top-rels (filter #(:top (meta %)) (keys trelations))]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name ~(merge (meta name)
                         {::left-model-name (list 'quote left)
                          ::right-model-name (list 'quote right)
                          ::trelations (list 'quote trelations)
                          ::prelations (list 'quote prelations)})
       [~left ~right dir# ~@other-args]
       (when-not (#{:right :left :right-checkonly :left-checkonly} dir#)
         (u/errorf "Direction parameter must either be :left or :right but was %s."
                   dir#))
       (letfn [~@(vals prelations)
               ~@(map (partial convert-relation trelations)
                      (remove #(:abstract (meta %)) trelations))]
         (binding [*target-direction* dir#
                   *target-model* (if (= dir# :right) ~right ~left)
                   *relation-bindings* (atom {:related {}
                                              :unrelated {}})]
           ~@(map (fn [r] `(~r)) top-rels)
           @*relation-bindings*)))))
