(ns funnyqt.extensional
  "Specify models extensionally."
  (:require clojure.set
            [funnyqt.utils       :as u]
            [funnyqt.query       :as q]
            [funnyqt.generic     :as g]
            [clojure.tools.macro :as m]))

;;# Dynamic vars

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {Archetype TargetInstance}}.
  The inner maps from archetypes to images only contain the concrete
  TargetMetaclass instances, not subclass instances.  So a lookup has to
  consider the class hierarchy.

  Don't use that directly but use the special resolving functions

    - `funnyqt.extensional/element-image`
    - `funnyqt.extensional/element-archetype`
    - `funnyqt.extensional/source-image`
    - `funnyqt.extensional/target-image`
    - `funnyqt.extensional/target-images`

  or the general resolving functions

    - `funnyqt.extensional/image`
    - `funnyqt.extensional/archetype`"}
  *img*)

(def ^{:dynamic true
       :doc "A map of the form {TargetMetaClass {TargetInstance Archetype}}.
  The inner maps from images to archetypes only contain the concrete
  TargetMetaclass instances, not subclass instances.  So a lookup has to
  consider the class hierarchy.

  Don't use this map directly but use the general resolving functions

    - `funnyqt.extensional/image`
    - `funnyqt.extensional/archetype`"}
  *arch*)

(def ^{:dynamic true
       :doc "If true, recording of traceability mappings is omitted.  Don't
  this directly but use the `without-trace-recording` macro."}
  *omit-trace-recording*
  false)

;;# Img/Arch accessors

(defn ^:private image-internal
  "Returns the image of `arch` for element or relationship class `cls`."
  [error-if-not-found cls arch]
  (when-not (bound? #'*img*)
    (u/errorf "No trace mappings in scope!"))
  (let [m (@*img* cls)]
    (or (and m (m arch))
        (loop [subs (g/mm-all-subclasses cls)]
          (when (seq subs)
            (or (get (@*img* (first subs)) arch)
                (recur (rest subs)))))
        (when error-if-not-found
          (u/errorf "No image of %s in image function of %s."
                    arch (g/qname cls))))))

(defn ^:private archetype-internal
  "Returns the archetype of `img` for element or relationship class `cls`."
  [error-if-not-found cls img]
  (when-not (bound? #'*arch*)
    (u/errorf "No trace mappings in scope!"))
  (let [m (@*arch* cls)]
    (or (and m (m img))
        (loop [subs (g/mm-all-subclasses cls)]
          (when (seq subs)
            (or (get (@*arch* (first subs)) img)
                (recur (rest subs)))))
        (when error-if-not-found
          (u/errorf "No archetype of %s in archetype function of %s."
                    img (g/qname cls))))))

(defn image
  "Returns the image of `arch` for element or relationship class `cls`.
  If there is no image, returns nil.  `cls` may either by a metamodel class or
  a symbol denoting the name of the metamodel class.  In the latter case, a
  model `m` is required to resolve the metamodel class."
  ([cls arch]
   (when-not (g/mm-class? cls)
     (u/errorf "Use the arity-3 version of image if `cls` is no metamodel class."))
   (image-internal false cls arch))
  ([m cls arch]
   (image (if (g/mm-class? cls) cls (g/mm-class m cls))
          arch)))

(defn image-map
  "Returns a map of image traceability mappings of element or relationship
  class `cls` and its subclasses."
  ([cls]
   (when-not (g/mm-class? cls)
     (u/errorf "Use the arity-2 version of image-map if `cls` is no metamodel class."))
   (when-not (bound? #'*img*)
     (u/errorf "No trace mappings in scope!"))
   (apply merge (@*img* cls) (map image-map (g/mm-all-subclasses cls))))
  ([m cls]
   (image-map (if (g/mm-class? cls) cls (g/mm-class m cls)))))

(defn archetype
  "Returns the archetype of `img` for element or relationship class `cls`.
  If there is no archetype, returns nil.  `cls` may either by a metamodel class
  or a symbol denoting the name of the metamodel class.  In the latter case, a
  model `m` is required to resolve the metamodel class."
  ([cls img]
   (when-not (g/mm-class? cls)
     (u/errorf "Use the arity-3 version of archetype if `cls` is no metamodel class."))
   (archetype-internal false cls img))
  ([m cls img]
   (archetype (if (g/mm-class? cls) cls (g/mm-class m cls))
              img)))

(defn archetype-map
  "Returns a map of archetype traceability mappings of element or relationship
  class `cls` and its subclasses."
  ([cls]
   (when-not (g/mm-class? cls)
     (u/errorf "Use the arity-2 version of archetype-map if `cls` is no metamodel class."))
   (when-not (bound? #'*arch*)
     (u/errorf "No trace mappings in scope!"))
   (apply merge (@*arch* cls) (map archetype-map (g/mm-all-subclasses cls))))
  ([m cls]
   (archetype-map (if (g/mm-class? cls) cls (g/mm-class m cls)))))

;;# Utilities

(defn ^:private top-superclasses
  "Returns the set of top-level superclasses of mm-cls."
  [mm-cls]
  (loop [cs [mm-cls], tops #{}]
    (if (seq cs)
      (if-let [ncs (seq (g/mm-direct-superclasses (first cs)))]
        (recur (into (set (rest cs)) ncs) tops)
        (recur (rest cs) (conj tops (first cs))))
      tops)))

(defn ^:private into-trace-map
  "Internal helper: Update `trace-map` of `cls` with `new` mappings.
  Earlier mappings get overridden by `new` mappings."
  [trace-map cls new]
  (update-in trace-map [cls] merge new))

(defn ^:private check-trace-mappings [mm-cls new-archs]
  (if (bound? #'*img*)
    (let [top-classes (top-superclasses mm-cls)]
      (when-let [dups (seq (filter (apply some-fn (map #(partial image-internal false %)
                                                       top-classes))
                                   new-archs))]
        (u/errorf
         "Bijectivity violation: the archetypes %s are already contained in *img* for class %s or a sub- or superclass thereof."
         dups (g/qname mm-cls))))
    (u/errorf "No trace mappings in scope!")))

(defn ^:private check-valid-arch [a]
  (if (nil? a)
    (u/errorf "nil is no valid archetype!")
    a))

(defn ^:private add-trace-mappings! [mm-cls img]
  (when-not *omit-trace-recording*
    (swap! *img* into-trace-map mm-cls img)
    (swap! *arch* into-trace-map mm-cls (clojure.set/map-invert img))))

;;# User fns and macros

(defmacro with-trace-mappings
  "Establishes new, empty traceability maps (`*arch*` and `*img*`), executes
  `body`, and then re-establishes the previous traceability maps."
  [& body]
  `(binding [*img*  (atom {})
             *arch* (atom {})]
     ~@body
     [@*img* @*arch*]))

(defmacro ensure-trace-mappings
  "Executes `body` with the current trace mappings in place, i.e., the current
  ones are altered by `body`.  If there are no mappings in
  place, (ensure-trace-mappings body) is equivalent to (with-trace-mappings
  body)."
  [& body]
  `(if (bound? #'*img*)
     (do ~@body
         [@*img* @*arch*])
     (with-trace-mappings ~@body)))

(defmacro without-trace-recording
  "Executes `body` without recording traceability mappings, then re-establishes
  the previous traceability maps."
  [& body]
  `(binding [*omit-trace-recording* true]
     ~@body))

;;# Resolution fns

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the source element class of the current relationship class."}
  source-image)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the target element class of the current relationship class
  or reference."}
  target-image)

(def ^{:dynamic true
       :arglists '([archetypes])
       :doc "Returns the images of the given collection of `archetypes` in the
  image function of the current reference's target class."}
  target-images)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the image function
  corresponding to the metamodel class for which the current attribute or
  reference is declared."}
  element-image)

(def ^{:dynamic true
       :arglists '([image])
       :doc "Resolves the archetype of the given `image` in the archetype
  function corresponding to the metamodel class for which the current attribute
  or reference is declared."}
  element-archetype)


;;# Creating Elements/Rels & setting Attrs

(defn create-elements!
  "In model `m` creates one element of metamodel class `cls` for every
  archetype returned by `archfn`.  `archfn` must return a collection of
  arbitrary objects.  It's value is taken as a set.  Traceability mappings are
  established implicitly in `funnyqt.extensional/*img*` and
  `funnyqt.extensional/*arch*`.  Returns the sequence of new elements."
  [m cls archfn]
  (let [mm-cls (g/mm-class m cls)
        archs (set (archfn))]
    (check-trace-mappings mm-cls archs)
    (loop [as archs
           im (transient {})]
      (if (seq as)
        (let [a (check-valid-arch (first as))
              elem (g/create-element! m cls)]
          ;;(println "Created" elem "for" a)
          (recur (rest as)
                 (assoc! im a elem)))
        (let [img (persistent! im)]
          (add-trace-mappings! mm-cls img)
          (vals img))))))

(defn create-relationships!
  "In model `m` creates one relationship of metamodel class `cls` for every
  archetype returned by `archfn`.  `cls` is a symbol denoting the qualified
  name of the relationship class.

  `archfn` must return a collection of triples [arch src trg].  arch is an
  arbitrary object taken as archetype for the new relationship, and src and trg
  are the new relationship's source and target element.  Traceability mappings
  are established implicitly in `funnyqt.extensional/*img*` and
  `funnyqt.extensional/*arch*`.

  In `archfn`, `funnyqt.extensional/source-image` and
  `funnyqt.extensional/target-image` are bound to functions that return the
  image of the given archetype in the image-mapping of the new edge's
  source/target element class.

  Returns the sequence of new relationships."
  [m cls archfn]
  (let [rel-cls (g/mm-class m cls)
        src-elem-cls (g/mm-relationship-class-source rel-cls)
        trg-elem-cls (g/mm-relationship-class-target rel-cls)
        archs (binding [source-image (partial image-internal :error src-elem-cls)
                        target-image (partial image-internal :error trg-elem-cls)]
                (set (archfn)))]
    (check-trace-mappings rel-cls archs)
    (loop [as archs
           im (transient {})]
      (if (seq as)
        (let [[a s t] (first as)
              a (check-valid-arch a)
              e (g/create-relationship! m cls s t)]
          (recur (rest as)
                 (assoc! im a e)))
        (let [img (persistent! im)]
          (add-trace-mappings! rel-cls img)
          (vals img))))))

(defn set-avals!
  "In model `m` sets the `attr` values for all `cls` elements according to
  `valfn`.  `valfn` is a function of zero arguments which has to return either
  a map {elem attr-value...}, a collection of pairs [elem attr-value], or a
  function of one argument which is applied to every instance of the class
  declaring the attribute and has to return the value to be set for the
  attribute of that instance.

  In `valfn`, `funnyqt.extensional/element-image` is bound to a function that
  given an archetype of the class defining `attr` (i.e., `cls`) returns its
  image, that is, the instance of the defining class (or subclass) that has
  been created for the given archetype.  Likewise, the reverse function
  `funnyqt.extensional/element-archetype` is also bound."
  [m cls attr valfn]
  (let [mm-cls (g/mm-class m cls)]
    (binding [element-image (fn [arch]
                              (image-internal :error mm-cls arch))
              element-archetype (fn [img]
                                  (archetype-internal :error mm-cls img))]
      (let [vals (valfn)]
        (cond
          (coll? vals)
          (doseq [[elem val] vals]
            (g/set-aval! elem attr val))
          ;;---
          (fn? vals)
          (doseq [el (if (g/mm-relationship-class? mm-cls)
                       (g/relationships m mm-cls)
                       (g/elements m mm-cls))]
            (g/set-aval! el attr (vals el)))
          ;;---
          :else (u/errorf "Unknown valfn result: %s" valfn))))))

(defn ^:private add-or-set-adjs [m cls ref reffn set-or-add]
  (let [mm-cls (g/mm-class m cls)
        resolve-target-fn (partial image-internal :error (g/mm-referenced-element-class mm-cls ref))
        resolve-all-targets-fn (partial map resolve-target-fn)
        multi-valued? (g/mm-multi-valued-property? mm-cls ref)]
    (binding [element-image     (fn [arch]
                                  (image-internal :error mm-cls arch))
              element-archetype (fn [img]
                                  (archetype-internal :error mm-cls img))
              target-image      resolve-target-fn
              target-images     resolve-all-targets-fn]
      (let [refs (reffn)]
        (cond
          (coll? refs)
          (doseq [[elem val] refs]
            ((if multi-valued? (first set-or-add) (second set-or-add))
             elem ref val))
          ;;---
          (fn? refs)
          (doseq [elem (g/elements m mm-cls)]
            ((if multi-valued? (first set-or-add) (second set-or-add))
             elem ref (refs elem))))))))

(defn set-adjs!
  "In model `m`, sets the `ref` reference of `cls` elements according to
  `reffn`.

  `reffn` must be a function of zero args and has to return either a map {elem
  refed...}, a collection of tuples ([elem refed]...), or a function of one
  argument which is applied to every instance of the class declaring the
  reference and has to return the refed value to be set for the reference of
  that instance.  In all three cases, refed must be a collection of referenced
  elements.

  In `reffn`, `funnyqt.extensional/element-image` is bound to a function that
  given an archetype of the metamodel class defining the `ref` (i.e., `cls`)
  returns its image, that is, the instance of the defining class (or subclass)
  that has been created for the given archetype.  The reverse lookup function
  `funnyqt.extensional/element-archetype` is bound, too.  Likewise,
  `funnyqt.extensional/target-image` is bound to a resolving function for
  `ref`s target metamodel class, and `funnyqt.extensional/target-images` is
  bound to a function that resolves all archetypes in a collection according to
  `ref`s target metamodel class.

  In certain modeling environments, setting adjacencies implies the creation of
  explicit relationships (e.g., in JGraLab).  When using this function, no
  trace mappings are created for those."
  [m cls ref reffn]
  (add-or-set-adjs m cls ref reffn [g/set-adjs! g/set-adj!]))

(defn add-adjs!
  "In model `m`, adds to the multi-valued `ref` reference of `cls` elements
  according to `reffn`.

  `reffn` must be a function of zero args and has to return either a map {elem
  refed...}, a collection of tuples ([elem refed]...), or a function of one
  argument which is applied to every instance of the class declaring the
  reference and has to return the refed value to be set for the reference of
  that instance.  In all three cases, refed must be a collection of referenced
  elements.

  In `reffn`, `funnyqt.extensional/element-image` is bound to a function that
  given an archetype of the metamodel class defining the `ref` (i.e., `cls`)
  returns its image, that is, the instance of the defining class (or subclass)
  that has been created for the given archetype.  The reverse lookup function
  `funnyqt.extensional/element-archetype` is bound, too.  Likewise,
  `funnyqt.extensional/target-image` is bound to a resolving function for
  `ref`s target metamodel class, and `funnyqt.extensional/target-images` is
  bound to a function that resolves all archetypes in a collection according to
  `ref`s target metamodel class.

  In certain modeling environments, setting adjacencies implies the creation of
  explicit relationships (e.g., in JGraLab).  When using this function, no
  trace mappings are created for those."
  [m cls ref reffn]
  (add-or-set-adjs m cls ref reffn [g/add-adjs!
                                    (fn [& _]
                                      (u/errorf "You must not use add-adjs! for single-valued references!"))]))
