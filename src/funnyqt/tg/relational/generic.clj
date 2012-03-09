(ns funnyqt.tg.relational.generic
  "Generic relations"
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [funnyqt.utils :only [error]])
  (:require [funnyqt.tg.core :as core])
  (:require [funnyqt.tg.query :as funql])
  (:import (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)))


(defn qmark-symbol?
  "Returns true, if sym is a symbol with name starting with a question mark."
  [sym]
  (and (symbol? sym)
       (= (first (name sym)) \?)))

(defmacro with-fresh
  "Replace all symbols with a leading question mark with fresh lvars.
  In addition, all occurences of `_' are replaced with fresh lvars, one per
  occurence.  That means, that in `forms` all occurences of ?foo will be
  unified, but all occurences of `_' are not."
  [& forms]
  (let [fs (clojure.walk/postwalk #(if (= '_ %) (gensym "?") %) forms)
        qsyms (vec (distinct (filter qmark-symbol? (flatten fs))))]
    `(fresh ~qsyms
       ~@fs)))

(defn fresh?
  "Returns true, if `x` is fresh.
  `x` must have been `walk`ed before!"
  [x]
  (lvar? x))

(defn ground?
  "Returns true, if `x` is ground.
  `x` must have been `walk`ed before!"
  [x]
  (not (lvar? x)))

(defn printlno
  [s]
  (println s)
  succeed)

(defn fulfillso
  "Succeeds if `x` fulfills `pred`.
  Non-relational: both parameters have to be ground."
  [x pred]
  (fn [a]
    (let [e (walk a x)
          p (walk a pred)]
      (if (or (fresh? e) (fresh? p))
        (error "Element and predicate must be ground.")
        (if (p e) a (fail a))))))

(defmacro condx
  "Expands into a `conda` checking if all `vars` are ground.
  If so, then use a (conda ~@clauses), else use (conde ~@clauses).
  Thus, condx can be used as a generator just like conde, but if everything
  is ground, then the conda improves the performance."
  [[& vars] & clauses]
  `(conda
    [(all ~@(map (fn [v] `(nonlvaro ~v)) vars)) (printlno "all ground") (conda ~@clauses)]
    [succeed (printlno "not all ground") (conde ~@clauses)]))
