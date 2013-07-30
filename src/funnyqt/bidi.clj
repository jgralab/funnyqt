(ns funnyqt.bidi
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:use funnyqt.relational.util)
  (:use [clojure.core.logic.protocols :only [walk]])
  (:require [clojure.core.cache :as cache]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]
            [funnyqt.protocols :as p]))

;; Either :left or :right
(def ^:dynamic *target-direction*)

(defn select-match [matches]
  (when-not (seq matches)
    (u/errorf "Couldn't create a target match!"))
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
                 [sym `(or ~sym (lvar ~(name sym)))])
               syms)))

(defn ^:private make-destr-map
  ([syms]
     {:keys (vec (set syms))})
  ([syms as]
     {:keys (vec (set syms)) :as as}))

(defn untempify-trg-match [trg-match]
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

(defn ^:private do-rel-body [relkw trg map wsyms src-syms trg-syms]
  (let [src (if (= trg :right) :left :right)
        sm  (gensym "src-match")
        tm  (gensym "trg-match")
        etm (gensym "enforced-trg-match")]
    `(let [wfns#
           (doall
            (for [~(make-destr-map (concat wsyms src-syms) sm)
                  (run* [q#]
                    ~@(:when map)
                    ~@(get map src)
                    (== q# ~(make-kw-result-map (concat wsyms src-syms))))]
              (binding [tmp/*wrapper-cache* (or tmp/*wrapper-cache* (cache/basic-cache-factory {}))]
                (let [~@(make-wrapper-bindings trg-syms)
                      ~(make-destr-map trg-syms tm)
                      (binding [tmp/*make-tmp-elements* true]
                        (select-match
                         (run* [q#]
                           ~@(get map trg)
                           (tmp/finalizeo ~@trg-syms)
                           (== q# ~(make-kw-result-map trg-syms)))))]
                  (enforce-match ~tm)
                  (let [~(make-destr-map trg-syms etm) (untempify-trg-match ~tm)]
                    (swap! *relation-bindings* update-in [~relkw] conj (merge ~sm ~etm))
                    (fn [] ~@(:where map)))))))]
       (doseq [wfn# wfns#]
         (wfn#)))))

(defn convert-relation [[name & more]]
  (let [relkw (keyword (clojure.core/name name))
        map   (apply hash-map more)
        body  (concat (:left map) (:right map))
        wsyms (distinct (filter qmark-symbol? (flatten (:when map))))
        lsyms (distinct (filter qmark-symbol? (flatten (:left map))))
        rsyms (distinct (filter qmark-symbol? (flatten (:right map))))
        syms  (distinct (concat lsyms rsyms))]
    (when-let [unknown-keys (seq (disj (set (keys map))
                                       :left :right :when :where))]
      (u/errorf "Relation contains unknown keys: %s" unknown-keys))
    `(~name [& ~(make-destr-map syms)]
            (let ~(make-relation-binding-vector syms)
              (if (= *target-direction* :right)
                ~(do-rel-body relkw :right map wsyms lsyms rsyms)
                ~(do-rel-body relkw :left  map wsyms rsyms lsyms))))))

(def ^{:dynamic true
       :doc "A map with the following structure:

    {:relname1 bindings, :relname2 bindings, ...}

  where bindings is:

    ({:?lsym1 val1, :?rsym1 rval2, ...}
     ...)"}
  *relation-bindings*)

(defn relateo [relation & keyvals]
  (let [m (apply hash-map keyvals)]
    (fn [a]
      (let [bindings (@*relation-bindings* relation)]
        (to-stream
         (->> (map (fn [b]
                     (let [vs (mapv #(get b % ::not-found) (keys m))]
                       (when (funnyqt.query/member? ::not-found vs)
                         (u/errorf "Unbound keys: %s"
                                   (pr-str
                                    (filter #(= (get b % ::not-found)
                                                ::not-found)
                                            (keys m)))))
                       (unify a (vec (vals m)) vs)))
                   bindings)
              (remove not)))))))

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
  clause may assume that foo2bar already holds for all matching elements."
  [name [left right] & relations]
  (let [top-rels (filter #(:top (meta (first %))) relations)]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name [~left ~right dir#]
       (when-not (or (= dir# :left) (= dir# :right))
         (u/errorf "Direction parameter must either be :left or :right but was %s."
                   dir#))
       (letfn [~@(map convert-relation relations)]
         (binding [*target-direction* dir#
                   *target-model* (if (= dir# :right) ~right ~left)
                   *relation-bindings* (atom {})]
           ~@(map (fn [r] `(~(first r))) top-rels)
           @*relation-bindings*)))))

