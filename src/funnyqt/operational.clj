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
       :doc "Actions deferred to the end of the transformation."}
  *deferred-actions*)

(defmacro defmapping
  {:arglists '([name [args] & body]
                 [name ([args] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        args (first more)
        body (next more)]
    (if (vector? args)
      `(~name
        ~args
        (let [result# (do ~@body)
              current# (get @*mappings* ~name)]
          (swap! *mappings* #(assoc % ~name
                                    (assoc current#
                                      ~(first args) result#)))
          result#))
      (error (format "Invalid defmapping form: expected arg vector but got %s."
                     args)))))

(defmacro defhelper
  {:arglists '([name [args] & body]
                 [name ([args] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        args (first more)
        body (next more)]
    (if (vector? args)
      `(~name
        ~args
        ~@body)
      (error (format "Invalid defhelper form: expected arg vector but got %s."
                     args)))))

(defmacro deferred
  [& body]
  `(swap! *deferred-actions* conj (fn [] ~@body)))

(defn resolve-in
  "Returns the target object created for `obj' in `mapping'."
  [mapping obj]
  (get (get @*mappings* mapping) obj))

(defn resolve-all-in
  "Returns a collection of target objects created for `objs' in `mapping'."
  [mapping objs]
  (into (empty objs)
        (map #(resolve-in mapping %)
             objs)))

;; TODO: Revamp to use name-with-attributes
(defmacro deftransformation [name args & mappings]
  `(defn ~name ~args
     (letfn [~@(map macroexpand-1 (butlast mappings))]
       (binding [*mappings* (atom {})
                 *deferred-actions* (atom [])]
         ~(last mappings)
         (doseq [da# @*deferred-actions*]
           (da#))
         ~(last args)
         ;; TODO: remove debugging code.
         ;;(clojure.pprint/pprint @*mappings*)
         ))))


