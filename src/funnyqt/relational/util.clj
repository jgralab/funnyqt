(ns funnyqt.relational.util
  "(Internal) Relation writing utilities."
  (:require [clojure.core.logic :as ccl]))

(defn qmark-symbol? [sym]
  (and
   (symbol? sym)
   (= (first (name sym)) \?)))

(defn fresh?
  "Returns true, if `x` is fresh.
  `x` must have been `walk`ed before!"
  [x]
  (ccl/lvar? x))

(defn ground?
  "Returns true, if `x` is ground.
  `x` must have been `walk`ed before!"
  [x]
  (not (ccl/lvar? x)))

(defn printo
  "Prints `txt` and the (walked) values of `lvars`."
  [txt & lvars]
  (fn [a]
    (println txt (map (partial ccl/walk* a) lvars))
    a))
