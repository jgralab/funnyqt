(ns funnyqt.bidi
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:use funnyqt.relational.util)
  (:use [clojure.core.logic.protocols :only [walk]])
  (:require [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.utils :as u]
            [funnyqt.tg :as tg]))

;; Either :left or :right
(def ^:dynamic *target-direction*)

(defn sort-matches [matches]
  (sort (fn [a b]
          (let [diff (- (count (filter tmp/tmp-element? (vals a)))
                        (count (filter tmp/tmp-element? (vals b))))]
            (if (zero? diff)
              (compare (vec a) (vec b))
              diff)))
        matches))

(defn select-best-match [matches]
  (or
   ;; Do we have an optimal match?
   (first (filter #(every? (complement tmp/tmp-element?) (vals %)) matches))
   ;; If not, select the match with the fewest temporary elements.
   (first (sort-matches (filter tmp/valid-match? matches)))))

(defn enforce-match [match]
  (doseq [el (filter tmp/tmp-element? (vals match))]
    (tmp/manifest el)))

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

(defn untempify-trg-match [trg-match]
  (apply hash-map
         (mapcat (fn [[k v]]
                   [k (if (tmp/tmp-element? v)
                        (tmp/get-manifestation v)
                        v)])
                 trg-match)))

(defn ^:private do-rel-body [relkw trg map wsyms src-syms trg-syms]
  (let [src (if (= trg :right) :left :right)
        sm  (gensym "src-match")
        tm  (gensym "trg-match")
        etm (gensym "enforced-trg-match")]
    `(doseq [~(make-destr-map (concat wsyms src-syms) sm)
             (run* [q#]
               ~@(:when map)
               ~@(get map src)
               (== q# ~(make-kw-result-map (concat wsyms src-syms))))]
       (let [~(make-destr-map trg-syms tm)
             (binding [tmp/*make-tmp-elements* true]
               (select-best-match
                (run* [q#]
                  ~@(get map trg)
                  (== q# ~(make-kw-result-map trg-syms)))))]
         ~@(:optional map)
         (enforce-match ~tm)
         (let [~(make-destr-map trg-syms etm) (untempify-trg-match ~tm)]
           (swap! *relation-bindings* update-in [~relkw] conj (merge ~sm ~etm))
           ~@(:where map))))))

(defn convert-relation [[name & more]]
  (let [relkw (keyword (clojure.core/name name))
        map (apply hash-map more)
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
                ~(do-rel-body relkw :right map wsyms lsyms rsyms)
                ~(do-rel-body relkw :left  map wsyms rsyms lsyms))))))

(def ^:dynamic *relation-bindings*)

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

(defmacro deftransformation [name [left right] & relations]
  (let [top-rels (filter #(:top (meta (first %))) relations)]
    (when (empty? top-rels)
      (u/error "There has to be at least one :top rule!"))
    `(defn ~name [~left ~right dir#]
       (when-not (or (= dir# :left) (= dir# :right))
         (u/errorf "Direction parameter must either be :left or :right but was %s."
                   dir#))
       (letfn [~@(map convert-relation relations)]
         (binding [*target-direction* dir#
                   *relation-bindings* (atom {})]
           ~@(map (fn [r] `(~(first r))) top-rels)
           @*relation-bindings*)))))

