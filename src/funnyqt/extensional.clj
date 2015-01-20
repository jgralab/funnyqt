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

(defn ^:private into-trace-map
  "Internal helper: Update `trace-map` of `cls` with `new` mappings.
  Earlier mappings get overridden by `new` mappings."
  [trace-map cls new]
  (update-in trace-map [cls] merge new))

(defn add-trace-mappings! [cls img]
  (when *img*
    (swap! *img* into-trace-map cls img))
  (when *arch*
    (swap! *arch* into-trace-map cls (clojure.set/map-invert img))))

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

;;# Resolution fns

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the source element class of the current relationship class.
  This function is only bound in `create-relationships!`."}
  resolve-source)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the target element class of the current relationship class
  or reference.  This function is only bound in `create-relationships!` and
  `set-adjs!`."}
  resolve-target)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Returns the images of the given collection of `archetypes` in the
  image function of the current reference's target class.  This function is
  only bound inside `set-adjs!`."}
  resolve-all-targets)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the metamodel class for which the current attribute is
  declared.  This function is only bound in `set-avals!`."}
  resolve-element)


;;# Creating Elements/Rels & setting Attrs

(defn create-elements!
  "In model `m` create one element of metamodel class `cls` for every archetype
  returned by `archfn`.  `archfn` must return a collection of arbitrary
  objects.  It's value is taken as a set.  Traceability mappings are
  established implicitly.  Returns the sequence of new elements."
  [m cls archfn]
  (let [mm-cls (g/mm-class m cls)]
    (loop [as (set (archfn))
           im (transient {})]
      (if (seq as)
        (let [elem (g/create-element! m cls)
              a (first as)]
          ;;(println "Created" elem "for" a)
          (recur (rest as)
                 (assoc! im a elem)))
        (let [img (persistent! im)]
          (add-trace-mappings! mm-cls img)
          (vals img))))))

(defn create-relationships!
  "In model `m` create one relationship of class `cls` for every archetype
  returned by `archfn`.  `cls` is a symbol denoting the qualified name of the
  relationship class.

  `archfn` must return a collection of triples [arch src trg].  arch is an
  arbitrary object taken as archetype for the new relationship, and src and trg
  are the new relationship's source and target element.  Traceability mappings
  are established implicitly.

  In `archfn`, `resolve-source` and `resolve-target` are bound to functions
  that return the image of the given archetype in the image-mapping of the new
  edge's source/target element class.

  Returns the sequence of new relationship."
  [m cls archfn]
  (let [rel-cls (g/mm-class m cls)
        src-elem-cls (g/mm-relationship-class-source rel-cls)
        trg-elem-cls (g/mm-relationship-class-target rel-cls)]
    (loop [as (binding [resolve-source (partial image src-elem-cls)
                        resolve-target (partial image trg-elem-cls)]
                (set (archfn)))
           im (transient {})]
      (if (seq as)
        (let [[a s t] (first as)
              e (g/create-relationship! m cls s t)]
          (recur (rest as)
                 (assoc! im a e)))
        (let [img (persistent! im)]
          (add-trace-mappings! rel-cls img)
          (vals img))))))

(defn set-avals!
  "In model `m` set the `attr` values for all `cls` elements according to
  `valfn`, i.e., `valfn` has to return a map {attr-elem attr-value} or a
  collection of pairs.

  In `valfn`, `resolve-element` is bound to a function that given an archetype
  of the class defining the attribute returns its image, that is, the instance
  of the defining class (or subclass) that has been created for the given
  archetype."
  [m cls attr valfn]
  (let [mm-cls (g/mm-class m cls)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (image mm-cls arch))]
                         (doall (valfn)))]
      (g/set-aval! elem attr val))))

(defn set-adjs!
  [m cls ref reffn]
  (let [mm-cls (g/mm-class m cls)
        resolve-target-fn (partial image (g/mm-referenced-element-class mm-cls ref))
        resolve-all-targets-fn (partial map resolve-target-fn)
        multi-valued? (g/mm-multi-valued-property? mm-cls ref)]
    (doseq [[elem val]
            (binding [resolve-element     (partial image mm-cls)
                      resolve-target      resolve-target-fn
                      resolve-all-targets resolve-all-targets-fn]
              (doall (reffn)))]
      ((if multi-valued? g/set-adjs! g/set-adj!) elem ref val))))

(defn add-adjs!
  [m cls ref reffn]
  (let [mm-cls (g/mm-class m cls)
        resolve-target-fn (partial image (g/mm-referenced-element-class mm-cls ref))
        resolve-all-targets-fn (partial map resolve-target-fn)
        multi-valued? (g/mm-multi-valued-property? mm-cls ref)]
    (doseq [[elem val]
            (binding [resolve-element     (partial image mm-cls)
                      resolve-target      resolve-target-fn
                      resolve-all-targets resolve-all-targets-fn]
              (doall (reffn)))]
      (g/add-adjs! elem ref val))))

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
