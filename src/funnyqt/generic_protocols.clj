(ns funnyqt.generic-protocols
  "Generic functions like quantified expressions."
  (:use [funnyqt.utils :only [add-long-doc!]]))

(add-long-doc! "TODO")

;;* Code

;;** Describing Elements

(defprotocol Describable
  "A protocol for elements supporting describe."
  (describe [this]
    "Describes `this' attributed element or attributed element class."))

