(ns funnyqt.extensional
  "Specify models extensionally."
  (:require clojure.set
            [funnyqt.utils       :as u]
            [funnyqt.generic     :as g]
            [clojure.tools.macro :as m]))

;;# Dynamic vars

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {Archetype TargetInstance}}."}
  *img*)

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {TargetInstance Archetype}}."}
  *arch*)

;;# Img/Arch accessors

(defn image
  "Returns the image of `arch` for element or relationship class `cls`.
  Can only be called inside a `deftransformation`."
  [cls arch]
  (let [m (@*img* cls)]
    (or (and m (m arch))
        (loop [subs (g/mm-all-subclasses cls)]
          (when (seq subs)
            (or (get (@*img* (first subs)) arch)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve image of %s in img fn of %s: %s"
                  arch cls @*img*))))

(defn archetype
  "Returns the archetype of `img` for element or relationship class `cls`.
  Can only be called inside a `deftransformation`."
  [cls img]
  (let [m (@*arch* cls)]
    (or (and m (m img))
        (loop [subs (g/mm-all-subclasses cls)]
          (when (seq subs)
            (or (get (@*arch* (first subs)) img)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
                  img cls @*arch*))))


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
