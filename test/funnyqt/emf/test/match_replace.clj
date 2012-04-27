(ns funnyqt.emf.test.query
  (:refer-clojure :exclude [parents])
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.in-place)
  (:use funnyqt.emf)
  ;;(:use funnyqt.query.emf)
  (:use [funnyqt.emf.test.core :only [family-model]])
  (:use clojure.test))


