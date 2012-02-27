(ns funnyqt.operational
  "Stuff for writing QVT Operational Mappings like transformations."
  (:use [funnyqt.utils :only [error add-long-doc!]])
  (:require [clojure.tools.macro :as m]))

(add-long-doc! "TODO")

;;* Code

(def ^{:dynamic true
       :doc "A map from mapping rule to map from source to target objects."}
  *mappings*)

(def ^{:dynamic true
       :doc ""}
  *deferred-actions*)

(defmacro defmapping
  {:arglists '([name doc-string? attr-map? [args] & body]
                 [name doc-string? attr-map? ([args] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        args (first more)
        body (next more)]
    (if (vector? args)
      `(defn ~name
         ~(meta name)
         ~args
         (let [result# (do ~@body)
               current# (get @*mappings* ~name)]
           (swap! *mappings* #(assoc % ~name
                                     (assoc (get % ~name)
                                       ~(first args) result#)))
           result#))
      (error (format "Invalid defmapping: expected arg vector but got %s."
                     args)))))

(defmacro deftransformation [name args & mappings]
  `(defn ~name ~args
     ~@(butlast mappings)

     (binding [*mappings* (atom {})
               *deferred-actions* (atom [])]
       ~(last mappings)
       (clojure.pprint/pprint @*mappings*))))


