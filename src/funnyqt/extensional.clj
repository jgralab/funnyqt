(ns funnyqt.extensional
  "Specify models extensionally."
  (:use [funnyqt.utils :only [error errorf]])
  (:require clojure.set)
  (:require [clojure.tools.macro :as m]))

;;# Dynamic vars

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {Archetype TargetInstance}}."}
  *img*)

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {TargetInstance Archetype}}."}
  *arch*)

;;# Utilities

(defn into-trace-map
  "Internal helper: Update `trace-map` of `cls` with `new` mappings.
  Earlier mappings get overridden by `new` mappings."
  [trace-map cls new]
  (update-in trace-map [cls] merge new))

;;# User fns and macros

(defmacro with-trace-mappings
  "Establishes new, empty traceability maps (`*arch*` and `*img*`), executes
  `body`, and then re-establishes the previous traceability maps."
  [& body]
  `(binding [*arch* (atom {})
             *img*  (atom {})]
     ~@body))

(defmacro without-trace-mappings
  "Executes `body` without recording traceability mappings, then re-establishes
  the previous traceability maps."
  [& body]
  `(binding [*arch* nil
             *img*  nil]
     ~@body))

;;# The transformation macro itself

(defmacro deftransformation
  "Create a new transformation named `name` with optional `doc-string` and
  optional `attr-map`, the given `params` (input graph args), and the given
  `body`."
  ;; Nicer arglist in doc
  {:arglists '([name doc-string? attr-map? [params*] & body])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        args (vec (first more))
        body (next more)]
    `(defn ~name
       ~(meta name)
       ~args
       (with-trace-mappings
         ~@body))))
