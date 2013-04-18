(ns funnyqt.bidi
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:use funnyqt.relational.util)
  (:require [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]))

;; Either :left or :right
(def ^:dynamic *target-direction*)

(defn sort-matches [matches]
  (into (sorted-set-by
         (fn [a b]
           (let [diff (- (count (filter tmp/tmp-element? (vals a)))
                         (count (filter tmp/tmp-element? (vals b))))]
             (if (zero? diff)
               (compare (vec a) (vec b))
               diff))))
        matches))

(defn select-best-match [matches]
  (first (sort-matches (filter tmp/valid-match? matches))))

(defn enforce-match [match]
  (let [tmps (filter tmp/tmp-element? (vals match))]
    (doseq [el tmps]
      (tmp/manifest el))))

(defmacro checkonly
  ([] `clojure.core.logic/s#)
  ([& goals] `(fn [a#]
                (binding [tmp/*make-tmp-elements* false]
                  (bind* a# ~@goals)))))

(defn ^:private make-kw-result-map [syms]
  (apply hash-map
         (mapcat (fn [qs]
                   [(keyword (name qs)) qs])
                 syms)))

(defn ^:private make-relation-binding-vector [syms]
  (vec (mapcat (fn [sym]
                 [sym `(or ~sym (lvar ~(name sym)))])
               syms)))

(defn ^:private make-destr-map
  ([syms]
     {:keys (vec (set syms))})
  ([syms as]
     {:keys (vec (set syms)) :as as}))

(defn ^:private qmark-sym? [sym]
  (and
   (symbol? sym)
   (= (first (clojure.core/name sym)) \?)))

(defn ^:private do-rel-body [trg map wsyms src-syms trg-syms]
  (let [src (if (= trg :right) :left :right)
        tm (gensym "trg-match")]
    `(doseq [~(make-destr-map (concat wsyms src-syms))
             (doall (run* [q#]
                      ~@(:when map)
                      ~@(get map src)
                      (== q# ~(make-kw-result-map src-syms))))]
       (let [~(make-destr-map trg-syms tm)
             (binding [tmp/*make-tmp-elements* true]
               (select-best-match
                (run* [q#]
                  ~@(:when map)
                  ~@(get map trg)
                  (== q# ~(make-kw-result-map trg-syms)))))]
         ~@(:optional map)
         (enforce-match ~tm)
         ~@(:where map)))))

(defn convert-relation [[name & more]]
  (let [map (apply hash-map more)
        body (concat (:left map) (:right map))
        wsyms (distinct (filter qmark-sym? (flatten (:when map))))
        lsyms (distinct (filter qmark-sym? (flatten (:left map))))
        rsyms (distinct (filter qmark-sym? (flatten (:right map))))
        syms (distinct (concat lsyms rsyms))]
    (when-let [unknown-keys (seq (disj (set (keys map))
                                       :left :right :when :where
                                       :optional))]
      (u/errorf "Relation contains unknown keys: %s" unknown-keys))
    `(~name [& ~(make-destr-map syms)]
            (let ~(make-relation-binding-vector syms)
              (if (= *target-direction* :right)
                ~(do-rel-body :right map wsyms lsyms rsyms)
                ~(do-rel-body :left  map wsyms rsyms lsyms))))))

(defmacro deftransformation [name [left right] & relations]
  (let [top-rels (filter #(:top (meta (first %))) relations)]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name [~left ~right dir#]
       (when-not (or (= dir# :left) (= dir# :right))
         (u/errorf "Direction parameter must either be :left or :right but was %s."
                   dir#))
       (letfn [~@(map convert-relation relations)]
         (binding [*target-direction* dir#]
           ~@(map (fn [r] `(~(first r))) top-rels))))))

