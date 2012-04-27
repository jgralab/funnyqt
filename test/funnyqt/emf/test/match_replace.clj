(ns funnyqt.emf.test.query
  (:refer-clojure :exclude [parents])
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.in-place)
  (:use funnyqt.emf.core)
  ;;(:use funnyqt.emf.query)
  (:use [funnyqt.emf.test.core :only [family-model]])
  (:use clojure.test))


