(ns funnyqt.operational
  "Stuff for writing QVT Operational Mappings like transformations."
  (:use [funnyqt.utils :only [error errorf]])
  (:use funnyqt.macro-utils)
  (:use [funnyqt.query :only [the]])
  (:require [clojure.tools.macro :as m]))

;;# Mappings

(def ^{:dynamic true
       :doc "A map from mapping rule to map from source to target objects."}
  *traceability-mappings*)

(def ^{:dynamic true
       :doc "Actions deferred to the end of the transformation."}
  *deferred-actions*)

;; TODO: Man will vermutlich ne Variante, bei der man bestimmen kann, für
;; welche Parameter Trace-Links erzeugt werden.
(defmacro defmapping
  "Defines a mapping function named `name` with optional `doc-string?',
  an argument vector `args`, and a `body`.  The syntax is the same as for
  `defn`, except that currently overloading is not supported.

  Every mapping function must return the object it has created, and a
  traceability mapping from the value of the mapping's first argument to that
  new object is persisted and can later be resolved using `resolve-in` and
  `resolve-all-in`.  If the mapping has no arguments, then no traceability
  mapping is established."

  {:arglists '([name doc-string? [args] & body]
                 ;; FIXME: Also support overloading.
                 ;; [name doc-string? ([args] & body)+]
                 )}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        args (first more)
        body (next more)]
    (if (vector? args)
      `(defn ~name ~(meta name)
        ~args
        ~(if (seq args)
           `(let [result# (do ~@body)]
              (swap! *traceability-mappings* assoc-in [~name ~(first args)] result#)
              result#)
           `(do ~@body)))
      (errorf "Invalid defmapping form: expected arg vector but got %s."
              args))))

;;# Deferring

(defmacro deferred
  "Captures a thunk (closure) that evaluates `body` as the last step of the
  transformation."
  [& body]
  `(swap! *deferred-actions* conj (fn [] ~@body)))

;;# Resolving

(defn resolve-in
  "Returns the target object created for `obj` in `mapping`."
  [mapping obj]
  (get (get @*traceability-mappings* mapping) obj))

(defn resolve-all-in
  "Returns a seq of target objects created for `objs` in `mapping`."
  [mapping objs]
  (into (empty objs)
        (map #(resolve-in mapping %)
             objs)))

;;# Transformations

(defmacro deftransformation
  "Defines an operational transformation named `name` with optional
  `doc-string?', optional `meta-map?, a mandatory `args` vector, and a `body`."
  {:arglists '([name doc-string? meta-map? [args] & body])}
  [tname & more]
  (let [[tname more] (m/name-with-attributes tname more)
        args (first more)
        body (next more)]
    ;; Validate
    (when-not (vector? args)
      (errorf "No args vector specified for transformation %s." args))
    `(defn ~tname ~(meta tname)
       ~args
       (binding [*traceability-mappings* (atom {})
                 *deferred-actions* (atom [])]
         (let [r# (do ~@body)]
           (doseq [da# @*deferred-actions*]
             (da#))
           r#)))))

