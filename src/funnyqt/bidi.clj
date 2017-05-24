(ns funnyqt.bidi
  "Bidirectional transformations (BX)."
  (:require [clojure.core.logic :as ccl]
            [clojure.tools.macro :as tm]
            [clojure.walk :as cw]
            [flatland.ordered.map :as om]
            [funnyqt.generic :as g]
            [funnyqt
             [relational :as r]
             [utils :as u]]
            [funnyqt.bidi.internal :as i]
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
  The current target model of the transformation execution.
  Depends on `*target-direction*, so it's either the :right or the :left modes."}
  *target-model*)

(def ^{:dynamic true
       :doc "Only for internal use.
  A map with the following structure:

    {:related   {t-relation1 bindings, t-relation2 bindings, ...}
     :unrelated {t-relation1 bindings, t-relation2 bindings, ...}}

  where t-relationN is a keyword denoting a t-relation and bindings is:

    #{{:?lsym1 lval1, :?rsym1 rval2, ...}
      ...}

  Access this information with the relation `relateo`."}
  *t-relation-bindings*)

(defn ^:private make-kw-result-map [syms]
  (apply hash-map
         (mapcat (fn [qs]
                   [(keyword (name qs)) qs])
                 syms)))

(defn ^:private make-destr-map [syms as]
  {:keys (vec (set syms)) :as as})

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
  [code what when wsyms src-syms trg-syms]
  (cond
    (true? code)
    `[(println (format "==> %s(%s)" '~what ~when))
      (clojure.pprint/pprint ~(into {} (map (fn [sym]
                                              [`'~sym sym]))
                                    (concat wsyms src-syms trg-syms)))]
    ;;--
    code [code]))

(defn new-elemento?
  "Succeeds iff `elem` is a ground model element to be created by the transformation.
  Throws an exception if `elem` is ground but no model element."
  [elem]
  (fn [a]
    (let [gelem (ccl/walk* a elem)]
      (when (ccl/lvar? gelem)
        (u/errorf "elem must be ground but was %s" gelem))
      (when-not (or (g/element? gelem)
                    (tmp/wrapper-element? gelem)
                    (tmp/tmp-element? gelem))
        (u/errorf "elem must be a ground model element but was %s" gelem))
      (if (tmp/tmp-element? gelem)
        (ccl/succeed a)
        (ccl/fail a)))))

(defn existing-elemento?
  "Succeeds iff `elem` is a ground and existing model element that has been
  matched by the transformation.  Throws an exception if `elem` is ground but
  no model element."
  [elem]
  (fn [a]
    (let [gelem (ccl/walk* a elem)]
      (when (ccl/lvar? gelem)
        (u/errorf "elem must be ground but was %s" gelem))
      (when-not (or (g/element? gelem)
                    (tmp/wrapper-element? gelem)
                    (tmp/tmp-element? gelem))
        (u/errorf "elem must be a ground model element but was %s" gelem))
      (if (or (g/element? gelem)
              (tmp/wrapper-element? gelem))
        (ccl/succeed a)
        (ccl/fail a)))))

(defn unseto?
  "Succeeds if `elem`s reference `ref` is unset or is set to `refed-elem`.
  All arguments must be ground.  `ref` may be a keyword or top-level reference
  relation.  Non-relational.

  Useful in conda-goals like these:

  ;; Prefer adding to the father role.  If that's already set, add to the sons
  ;; role.
  (conda
   [(all
     (unseto? family f/->father member)
     (f/->father f family member))]
   [(f/->sons f family member)])"
  [f elem ref refed-elem]
  (fn [a]
    (let [ref (cond
                (keyword? ref) ref
                (fn? ref) (keyword (clojure.string/replace-first (u/fn-name ref) "->" ""))
                :else (u/errorf "Cannot get ref from %s" ref))
          gelem (ccl/walk* a elem)
          relem (if (tmp/wrapper-element? gelem)
                  (tmp/manifestation gelem)
                  gelem)
          grefed-elem (ccl/walk* a refed-elem)
          rrefed-elem (if (tmp/wrapper-element? grefed-elem)
                        (tmp/manifestation grefed-elem)
                        grefed-elem)]
      (if (tmp/tmp-element? relem)
        a
        (do
          (when (ccl/lvar? relem)
            (u/errorf "elem must be ground but was %s" gelem))
          (when-not (g/element? relem)
            (u/errorf "elem must be a model element but was %s" relem))
          (if (g/unset? relem ref)
            a
            (let [adj (g/adj relem ref)]
              (ccl/to-stream
               (->> (for [el (concat [(i/maybe-wrap f adj)]
                                     (when tmp/*make-tmp-elements*
                                       [(tmp/make-tmp-element f :element)]))]
                      (ccl/unify a refed-elem el))
                    (remove not))))))))))

(defn ^:private do-rel-body [relsym trg map id-map-atom wsyms src-syms trg-syms args-map]
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
                                      (distinct
                                       (ccl/run* [q#]
                                         (r/with-fresh
                                           (i/src-initializeo ~args-map ~@(set (concat wsyms src-syms)))
                                           ~@(get map src)
                                           ~@(:when map)
                                           (ccl/== q# ~(make-kw-result-map (concat wsyms src-syms))))))]
                              ~(if enforcing
                                 ;; Enforcement mode
                                 `(binding [tmp/*wrapper-cache* (atom {})]
                                    ~@(insert-debug (:debug-src map) relsym "DEBUG-SRC"
                                                    wsyms src-syms trg-syms)
                                    (let [~(make-destr-map trg-syms tm)
                                          (binding [tmp/*make-tmp-elements* true]
                                            (i/select-match
                                             (ccl/run* [q#]
                                               (r/with-fresh
                                                 (i/trg-initializeo *target-model* true ~sm ~args-map ~@trg-syms)
                                                 ~@(get map trg)
                                                 ~@(get map :target)
                                                 (tmp/finalizeo ~@trg-syms)
                                                 (ccl/== q# ~(make-kw-result-map trg-syms))))
                                             '~relsym ~sm))]
                                      ~@(insert-debug (:debug-trg map) relsym "DEBUG-TRG"
                                                      wsyms src-syms trg-syms)
                                      (let [~(make-destr-map trg-syms etm)
                                            (i/enforce-match ~tm ~id-map-atom)]
                                        (swap! *t-relation-bindings* update-in [:related ~(keyword relsym)]
                                               (fn [current# new#]
                                                 (conj (or current# #{}) new#))
                                               (merge ~sm ~etm))
                                        ~@(insert-debug (:debug-enforced map) relsym
                                                        "DEBUG-ENFORCED"
                                                        wsyms src-syms trg-syms)
                                        (fn [] ~@(:where map)))))
                                 ;; Checkonly mode
                                 `(do
                                    ~@(insert-debug (:debug-src map) relsym "DEBUG-SRC"
                                                    wsyms src-syms trg-syms)
                                    (let [match# (first
                                                  (ccl/run 1 [q#]
                                                    (r/with-fresh
                                                      (i/trg-initializeo *target-model* false ~sm ~args-map ~@trg-syms)
                                                      ~@(get map trg)
                                                      ~@(get map :target)
                                                      (ccl/== q# ~(make-kw-result-map trg-syms)))))
                                          ~(make-destr-map trg-syms tm) match#]
                                      ~@(insert-debug (:debug-trg map) relsym "DEBUG-TRG"
                                                      wsyms src-syms trg-syms)
                                      (if match#
                                        (do
                                          (swap! *t-relation-bindings* update-in [:related ~(keyword relsym)]
                                                 (fn [current# new#]
                                                   (conj (or current# #{}) new#))
                                                 (merge ~sm ~tm))
                                          (fn [] ~@(:where map)))
                                        (do
                                          (swap! *t-relation-bindings* update-in [:unrelated ~(keyword relsym)]
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

(defn ^:private splice-in-at-succeed [clause & more]
  (if (empty? more)
    clause
    (let [[head tail] (split-with (complement (fn [x]
                                                (and (symbol? x)
                                                     (= (name x) "succeed"))))
                                  clause)]
      (if (seq head)
        ;; Da war ein succeed drin.  Hier einfügen!
        (apply concat head (concat more [(rest tail)]))
        ;; Und hier nicht.
        (apply concat clause more)))))

(defn ^:private embed-included-rels [all-rels m]
  (if-let [irels (:extends m)]
    (apply merge-with splice-in-at-succeed
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

(defn ^:private t-relation-goals-to-relateo-goals-1 [rel-syms clause]
  (u/prewalk (fn edit-fn [form]
               (if (and (list? form)
                        (contains? rel-syms (first form)))
                 `(relateo ~(keyword (first form)) ~@(rest form))
                 form))
             clause))

(defn ^:private t-relation-goals-to-relateo-goals [all-rels m]
  (let [rel-syms (into #{} (keys all-rels))
        do-it t-relation-goals-to-relateo-goals-1]
    (assoc m
           :left  (do-it rel-syms (:left m))
           :right (do-it rel-syms (:right m))
           :when  (do-it rel-syms (:when m)))))

(defn ^:private convert-t-relation [all-rels id-map-atom [relsym m]]
  (let [m     (embed-included-rels all-rels m)
        m     (t-relation-goals-to-relateo-goals all-rels m)
        wsyms (distinct (filter ru/qmark-symbol? (flatten (:when m))))
        lsyms (distinct (filter ru/qmark-symbol? (flatten (:left m))))
        rsyms (distinct (filter ru/qmark-symbol? (flatten (:right m))))
        tsyms (distinct (filter ru/qmark-symbol? (flatten (:target m))))
        syms  (distinct (concat lsyms rsyms wsyms))
        args-map (gensym "args-map")]
    (when-let [unknown-keys (seq (disj (set (keys m))
                                       :left :right :when :where :extends
                                       :debug-entry :debug-src :debug-trg
                                       :debug-enforced :target))]
      (u/errorf "Relation contains unknown keys: %s" unknown-keys))
    `(~relsym [& ~(make-destr-map syms args-map)]
      (i/check-t-relation-args '~relsym ~args-map ~(set (map keyword syms)))
      ~@(insert-debug (:debug-entry m) relsym "DEBUG-ENTRY" wsyms lsyms rsyms)
      (let [transform-fns# {:right ~(do-rel-body relsym :right m id-map-atom wsyms lsyms (concat rsyms tsyms) args-map)
                            :right-checkonly ~(do-rel-body relsym :right-checkonly m id-map-atom wsyms lsyms (concat rsyms tsyms) args-map)
                            :left  ~(do-rel-body relsym :left  m id-map-atom wsyms rsyms (concat lsyms tsyms) args-map)
                            :left-checkonly  ~(do-rel-body relsym :left-checkonly m id-map-atom wsyms rsyms (concat lsyms tsyms) args-map)}]
        ((transform-fns# *target-direction*))))))

(defn relateo
  "A relation that succeeds if there's a correspondence between `keyvals` in
  `t-relation` (given as keyword).  `keyvals` is a sequence of keywords with
  values that relate elements from the left and right domains of `t-relation`.

  Example:

    (relateo :class2table :?class ?subclass :?table ?subtable)"
  [t-relation & keyvals]
  (when-not (keyword? t-relation)
    (u/errorf "The t-relation has to be given as keyword but got %s."
              t-relation))
  (let [m (apply hash-map keyvals)]
    (fn [a]
      (let [bindings ((:related @*t-relation-bindings*) t-relation)]
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
  "A relation where the transformation direction is `dir`.
  Can be used for conditionalizing non-bijective transformations, e.g., when
  one wants some t-relation to be only enforced in one direction.  Then, just
  put (target-directiono :left) in a :when clause.

  target-directiono only unifies with :left or :right, i.e., even if the target
  direction argument of the transformation was :right-checkonly, the actual
  direction considered by target-directiono is :right."
  [dir]
  (when-not (#{:right :left :right-checkonly :left-checkonly} dir)
    (u/errorf "dir must be :left or :right but was %s." dir))
  (fn [a]
    (let [cdir (if (#{:right :right-checkonly} *target-direction*)
                 :right
                 :left)]
      (ccl/unify a dir cdir))))

(defn ^:private mapify-t-relations [rels]
  (into (om/ordered-map)
        (map (fn [rel]
               [(first rel) (apply hash-map (rest rel))])
             rels)))

(defn ^:private mapify-p-relations [rels]
  (into (om/ordered-map)
        (map (fn [rel]
               [(first rel) rel])
             rels)))

(defn number-all-source-model-elements
  "A simple :id-init-fn that simply numbers all source model elements so that
  every source element has a unique id (this number)."
  [left right dir & args]
  (->> (if (#{:right :right-checkonly} dir) left right)
       (g/elements)
       (map-indexed (fn [i el] [el i]))
       (mapcat identity)
       (apply hash-map)))

(defmacro deftransformation
  "Creates a new bidirectional transformation with the given `name` on the
  models `left` and `right` with possibly additional `args`.  The
  transformation may extend other transformation using an `extends-clause` (see
  Transformation Inheritance below).  If the transformation should delete
  target elements that aren't required, add a `target-deletion-clause` (see
  Target Deletion Clause below), and finally there can be synthetic ids (see
  Synthetic Identities below).

  The signature of the defined transformation (a plain function) is

    (transformation-name left-model right-model direction & args)

  `direction` is the direction in which to execute the transformation,
  either :right or :left for enforcement mode, or :right-checkonly
  or :left-checkonly for checkonly mode.

  Defining Transformation Relations
  =================================

  In the transformation specification, `t-relations` is a sequence of
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
  are enforced or checked automatically by the transformation in their
  declaration order.  Non-top-level t-relations have to be called explicitly
  from another t-relation using :where clauses (see Postconditions below).

  Target Deletion Clause
  ======================

  By default, FunnyQT won't delete elements in the target model which are not
  required by some source model element and some t-relation, i.e., unmatched
  target model elements.  That is, after the transformation has been enforced,
  for any element (considered by the transformation's t-relations) in the
  source model, there exists some element in the target model.  However, there
  still might be additional elements in the target model without some source
  model element correspondence.

  In general, this is good.  Given two models, if you enforce a transformation
  first in one and then the other direction, then both models will be in sync
  with missing elements created in either one.  This would not be possible if
  enforcing a transformation in a given direction would automatically delete
  unmatched target model elements.

  Anyway, since that's usefull nevertheless in some situations, you can enable
  automatic deleteion of unmatched target model elements with a target deletion
  clause of the form:

    :delete-unmatched-target-elements true

  Target deletion only happens in enforcement mode, i.e., not in the case of
  the direction being :right-checkonly or :left-checkonly.

  Return Value and Traceability
  =============================

  The return value of a bidirectional transformation is its complete
  traceability information represented as a map with the following structure:

    {:related   {:t-relation1 bindings, :t-relation2 bindings, ...}
     :unrelated {:t-relation1 bindings, :t-relation2 bindings, ...}}

  The inner map being the value of the :related key contains the actual
  traceability information, i.e., it is a map from the transformation's
  t-relations (as keywords) to the corresponding bindings in the left and right
  models.  Concretely, bindings is a set of maps of the form {:llvar1?
  lval1, :rlvar1? rval1, ...} where :llvar1 and :rlvar1 correspond to some
  logic variables in the :left or :right goals of the corresponding t-relation,
  and lval1 and rval1 are their values.

  This information can be accessed during the transformation.  E.g., a goal
  like

    (foo2bar :?foo ?foo :?bar ?bar)

  in a t-relation's :left, :right, or :when clause or in a plain relation
  unifies ?foo and ?bar with all possible bindings where ?foo and ?bar have
  been related previously by the foo2bar t-relation.  This is only a shorthand
  for the low-level traceability relation `relateo`, i.e., the above is
  equivalent to

    (relateo :foo2bar :?foo ?foo :?bar ?bar)

  The map being the value of the :unrelated key contains the bindings for which
  no corresponding target model match exists.  This map is only populated in
  checkonly mode since in enforcement mode, target elements are created or
  modified in order to guarantee the existence of a match.

  Preconditions
  =============

  A relation spec may also contain a :when precondition:

    (foo2bar
      :left  [...]
      :right [...]
      :when  [...])

  It is also a vector of goals.  Usually, the goals are used to retrieve and
  bind elements created by previous t-relations using traceability goals such
  as (other-t-rel :?foo ?f :?bar ?b) which use `relateo` internally.  The :left
  to :right semantics are: For all solutions satisfying the conjunction
  of :left and :when goals, a corresponding solution of the :right goals has to
  exist, i.e., the :when goals are simply added to the clause of the current
  source model.

  Target Clauses
  ==============

  T-relations may have a :target clause:

    (foo2bar
      :left   [...]
      :right  [...]
      :target [...])

  As said above, the goals of :when clauses are simply added to the current
  source clause, e.g., :left when transforming into the direction of the right
  model.  The :target clause is essentially the inverse, i.e., its goals are
  added to the current target clause, e.g., to the :right clause when
  transforming into the :right direction.

  The use-case this clause serves is the handling of non-bijective mappings in
  attribute value goals which may change the value (i.e., the generated attr*
  relations).  For example, in a hypothetical class diagram to database schema
  transformation, the attribute types INT and LONG both correspond to the
  database type INTEGER, and the database types VARCHAR and TEXT both
  correspond to the class diagram type STRING.  Thus, there are choices how to
  map STRING attributes and INTEGER columns.  In such cases, the mapping can be
  defined by a plain relation (see below)

    (cd-type2db-type [cdt dbt]
      (conda
        [(all (== cdt \"LONG\")   (== dbt \"INTEGER\"))]
        [(all (== cdt \"INT\")    (== dbt \"INTEGER\"))]
        [(all (== cdt \"STRING\") (== dbt \"TEXT\"))]
        [(all (== cdt \"STRING\") (== dbt \"VARCHAR\"))]))

  where the order of clauses defines that we prefer the larger types LONG and
  TEXT here.  This should be used in the :target clause like so.

     (attr2col
       :left   [...
                (cd/type* cd ?attr ?atype)]
       :right  [...
                (db/type* db ?col ?ctype)]
       :target [(cd-type2db-type ?atype ?ctype)])

  The problem the :target clause solves is the following: if it were placed in
  the :when clause (or either :left or :right), then (depending on direction)
  either cdt or dbt would be fresh.  Thus, when synchronizing between existing
  models, always the first of the two conda-clauses which could match will
  match because unifying a fresh variable with some literal always works.  The
  effect is that, e.g., all INT attributes will be changed to LONG.

  If the goal is in the :target clause, then both cdt and dbt are ground if the
  corresponding attribute and column have their type set already.  Thus, the
  INT/INTEGER and STRING/VARCHAR clauses will match for the respective
  attribute types, and the corresponding attribute and column types won't be
  changed.

  Postconditions
  ==============

  A relation spec may also contain a :where postcondition:

    (foo2bar
      :left  [...]
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

  Hint: The clauses of the relation that is extended may have
  `clojure.core.logic/succeed` in their :left/:right/:when clauses.  This is a
  no-op but gives a hint where the corresponding clauses of the extending
  t-relation should be filled in.  If no succeed goal is in the clauses, the
  extending t-relation's clauses are simply appended.

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
  represented in the transformation trace.

  Synthetic Identities
  ====================

  FunnyQT bidirectional transformations have forall-there-exists semantics.
  This also means that if in one model there are two completely equal elements
  (same type, attribute values, references), FunnyQT won't see a need to create
  two different elements in the other model.  If you wanted to change that,
  you'd need to add some kind of identity attribute to your element's metamodel
  class and assign unique values to your model elements.  Of course, extending
  a metamodel just for the sake of a transformation tool is cumbersome, so
  FunnyQT bidirectional transformations provide synthetic ids for you.

  Every bidirectional transformation implicitly declares a plain relation `id`.
  Goals have the form (id ?elem ?id) where the element ?elem has the given ?id.
  The ids can be defined before the start of the transformation using
  a :id-init-fn which is a function that receives the same arguments as the
  transformation itself (the left model, the right model, the direction, and
  possibly additional arguments) and returns a map from elements to their
  id (which may be chosen freely).  There is a  default :id-init-fn
  (`number-all-source-model-elements`) which simply numbers all elements in the
  current source model and uses that number as id.

  Here is an example: If we don't assume that the name of a Family in
  combination with the first name of a FamilyMember, and the name of a Person
  uniquely determine an element, then we need to use synthetic ids.

  (bx/deftransformation families2persons [f p]
    :id-init-fn number-all-source-model-elements
    (^:abstract member2person
     :left  [(f/->families f ?fam-reg ?family)
             (f/Family f ?family)
             (f/name f ?family ?last-name)
             (f/FamilyMember f ?member)
             (f/name f ?member ?first-name)
             (id ?member ?id)]
     :right [(p/->persons p ?per-reg ?person)
             (p/Person p ?person)
             (rel/stro ?last-name \", \" ?first-name ?n)
             (p/name p ?person ?n)
             (id ?person ?id)])
"

  {:arglists '([name [left right & args] extends-clause? target-deletion-clause? id-init-clause? & t-relations])}
  [name & more]
  (let [[name more] (tm/name-with-attributes name more)
        [left right & other-args] (first more)
        more (next more)
        [extended more] (if (= :extends (first more))
                          [(fnext more) (nnext more)]
                          [nil          more])
        [delete-unmatched-target-els more] (if (= :delete-unmatched-target-elements (first more))
                                             [(fnext more) (nnext more)]
                                             [false          more])
        [id-init-fn more] (if (= :id-init-fn (first more))
                            [(fnext more) (nnext more)]
                            [nil          more])
        relations more
        [trelations prelations] ((juxt remove filter) #(vector? (nth % 1)) relations)
        trelations (mapify-t-relations trelations)
        prelations (mapify-p-relations prelations)
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
        top-rels (filter #(:top (meta %)) (keys trelations))
        id-map-atom (gensym "id-map-atom")
        dir (gensym "dir")]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name ~(merge (meta name)
                         {::left-model-name (list 'quote left)
                          ::right-model-name (list 'quote right)
                          ::trelations (list 'quote trelations)
                          ::prelations (list 'quote prelations)})
       [~left ~right ~dir ~@other-args]
       (when-not (#{:right :left :right-checkonly :left-checkonly} ~dir)
         (u/errorf "Direction parameter must either be :left, :left-checkonly, :right, or :right-checkonly but was %s."
                   ~dir))
       (let [~id-map-atom (atom ~(if id-init-fn
                                   `(let [id-map# (~id-init-fn ~left ~right ~dir ~@other-args)]
                                      (when-not (map? id-map#)
                                        (u/errorf "The %s must result in a map but resulted in %s" :id-init-fn id-map#))
                                      id-map#)
                                   {}))]
         (letfn [(~'id [elem# val#]
                  (i/id ~id-map-atom elem# val#))
                 ~@(map (partial t-relation-goals-to-relateo-goals-1 trelations)
                     (vals prelations))
                 ~@(map (partial convert-t-relation trelations id-map-atom)
                     (remove #(:abstract (meta %)) trelations))]
           (binding [*target-direction* ~dir
                     *target-model* (if (= ~dir :right) ~right ~left)
                     *t-relation-bindings* (atom {:related {}
                                                  :unrelated {}})]
             ~@(map (fn [r] `(~r)) top-rels)
             ~@(when delete-unmatched-target-els
                 `((when (#{:left :right} ~dir) ;; Not in checkonly mode
                     (i/delete-unmatched-target-elements ~left ~right ~dir @*t-relation-bindings*))))
             (assoc @*t-relation-bindings*
                    :id-map @~id-map-atom)))))))
