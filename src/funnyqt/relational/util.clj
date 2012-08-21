(ns funnyqt.relational.util
  "Relation writing utilities."
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))


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
