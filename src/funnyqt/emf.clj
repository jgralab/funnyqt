(ns funnyqt.emf
  "Core functions for accessing and manipulating EMF models."
  (:use funnyqt.protocols
        funnyqt.emf-protocols
        funnyqt.utils
        funnyqt.query
        flatland.ordered.set
        flatland.ordered.map)
  (:require [clojure.core.cache :as cache]
            [clojure.core.reducers :as r])
  (:import
   (funnyqt.emf_protocols EMFModel EcoreModel)
   (org.eclipse.emf.ecore.xmi.impl XMIResourceImpl)
   (org.eclipse.emf.ecore.util EcoreUtil)
   (org.eclipse.emf.common.util URI EList UniqueEList EMap)
   (org.eclipse.emf.ecore.resource Resource ResourceSet)
   (org.eclipse.emf.ecore
    EcorePackage EPackage EPackage$Registry EObject EModelElement EClassifier
    EClass EDataType EEnumLiteral EEnum EFactory ETypedElement EAnnotation
    EAttribute EReference EStructuralFeature)))

;;# Simple type predicates

(definline eobject?
  "Returns true if `eo` is an EObject."
  [eo]
  `(instance? EObject ~eo))

(definline eclass?
  "Returns true if `eo` is an EClass."
  [eo]
  `(instance? EClass ~eo))

(extend-protocol IModelObject
  EObject
  (model-object? [this] true))

;;# Metamodel Access

(def ^:private eclassifier-cache
  "A cache from EClassifier names to EClassifiers."
  (cache/soft-cache-factory (hash-map)))

(def ^:private type-matcher-cache
  "A cache from type-specs to type-matchers."
  (cache/soft-cache-factory (hash-map)))

(defn reset-all-emf-caches
  "Resets all EMF specific caches:

    1. the eclassifier-cache
    2. the type-matcher-cache"
  []
  (alter-var-root #'eclassifier-cache
                  (constantly (cache/soft-cache-factory (hash-map))))
  (alter-var-root #'type-matcher-cache
                  (constantly (cache/soft-cache-factory (hash-map)))))

(defn load-metamodel
  "Loads the EcoreModel from the ecore file `f`.
  All EPackages are registered."
  [f]
  ;; Reset the caches, since now the names might not be unique anymore.
  (reset-all-emf-caches)

  (let [f (if (instance? java.io.File f)
            (.getPath ^java.io.File f)
            f)
        uri (URI/createFileURI f)
        res (XMIResourceImpl. uri)]
    (doto (->EcoreModel res)
      load-and-register-internal)))

(def ^{:arglists '([ecore-model file])
       :doc "Saves the metamodel `ecore-model` to `file`."}
  save-metamodel save-metamodel-internal)

(def ^{:arglists '([ecore-model])
       :doc "Returns a seq of the metamodel `ecore-model`s EPackages."}
  metamodel-epackages metamodel-epackages-internal)

(def ^:dynamic *ns-uris* nil)
(defmacro with-ns-uris
  "Restricts the EClassifier lookup in the dynamic scope of `body` to those
  contained in top-level EPackages registered with the given URIs at the
  EPackage registry and subpackages thereof."
  [uris & body]
  `(binding [*ns-uris* ~uris]
     ~@body))

(defn epackages
  "The lazy seq of all registered EPackages."
  []
  (with-system-class-loader
    (map #(.getEPackage EPackage$Registry/INSTANCE %)
         (or *ns-uris* (keys EPackage$Registry/INSTANCE)))))

(defn epackage
  "Returns the EPackage with the given (simple or qualified) `name`."
  [name]
  (let [name (clojure.core/name name)
        ffn (if (.contains name ".")
              (fn [^EPackage p] (= (clojure.core/name (qname p)) name))
              (fn [^EPackage p] (= (.getName p) name)))
        qkgs (filter ffn (epackages))]
    (when-not (seq qkgs)
      (errorf "No such package %s." name))
    (when (nnext qkgs)
      (errorf "Multiple packages named %s: %s\n%s" name qkgs
              "Restrict the search space using `with-ns-uris`."))
    (first qkgs)))

(extend-protocol IAbstractness
  EClass
  (abstract? [this]
    (.isAbstract this)))

(defn eclassifiers
  "The lazy seq of EClassifiers."
  []
  (mapcat (fn [^EPackage ep]
            (.getEClassifiers ep))
          (epackages)))

(defn eclass
  "Returns the EClass of the given EObject `eo`."
  [^EObject eo]
  (.eClass eo))

(defn eclasses
  "The lazy seq of EClasses."
  []
  (filter (partial instance? EClass) (eclassifiers)))

(defn eclassifier
  "Returns the eclassifier with the given `name`.
  `name` may be a simple or qualified name.  Throws an exception if no such
  classifier could be found, or if the given simple name is ambiguous."
  [name]
  (if-let [ecls (cache/lookup eclassifier-cache name)]
    (do (cache/hit eclassifier-cache name) ecls)
    (let [^String n (clojure.core/name name)
          ld (.lastIndexOf n ".")]
      (if (>= ld 0)
        (if-let [^EPackage ep (epackage (subs n 0 ld))]
          (or (.getEClassifier ep (subs n (inc ld)))
              (errorf "No such EClassifier %s in %s." n (print-str ep)))
          (errorf "No such EPackage %s." (subs n 0 ld)))
        (let [classifiers (filter (fn [^EClassifier ec]
                                    (= (.getName ec) n))
                                  (eclassifiers))]
          (cond
           (empty? classifiers) (errorf "No such EClassifier %s." n)
           (next classifiers)   (errorf "EClassifier %s is ambiguous: %s\n%s"
                                        n (print-str classifiers)
                                        "Restrict the search space using `with-ns-uris`.")
           :else (let [ecls (first classifiers)]
                   (cache/miss eclassifier-cache name ecls)
                   ecls)))))))

(defn eallsubclasses
  "Returns the (direct and indirect) sub-EClasses of the given EClass."
  [^EClass ecls]
  (filter #(and (not= ecls %) (.isSuperTypeOf ecls %)) (eclasses)))

(defn eenum-literal
  "Returns the EEnumLiteral specified by its `qname`."
  [qname]
  (let [[eenum elit] (split-qname qname)]
    (if-let [^EEnum enum-cls (eclassifier eenum)]
      (or (.getEEnumLiteral enum-cls ^String elit)
          (errorf "%s has no EEnumLiteral with name %s."
                  (print-str enum-cls) elit))
      (errorf "No such EEnum %s." eenum))))

;;# Generic Metamodel Access

(extend-protocol IMetaModelObject
  EClass
  (meta-model-object? [this] true))

(extend-protocol IMMClasses
  EClass
  (mm-classes [cls]
    (filter eclass? (eclassifiers))))

(extend-protocol IMMClass
  EObject
  (mm-class
    ([this]
       (.eClass this))
    ([this qn]
       (eclassifier qn)))
  EMFModel
  (mm-class
    ([this qn]
       (eclassifier qn)))
  EcoreModel
  (mm-class
    ([this qn]
       (eclassifier qn))))

(extend-protocol IMMDirectSuperClasses
  EClass
  (mm-direct-super-classes [this]
    (seq (.getESuperTypes this))))

(extend-protocol IMMSuperClassOf
  EClass
  (mm-super-class? [this sub]
    (and (not (identical? this sub))
         (.isSuperTypeOf this sub))))

;;# Model

(def ^{:arglists '([model] [model file])}
  save-model
  "In the arity 1 version, saves `model` which has to be associated with a file
  already.  In the arity 2 version, saves `model` to `file` (a string denoting
  a file name)."
  save-model-internal)

;;## Qualified Names

(extend-protocol IQualifiedName
  EClassifier
  (qname [this]
    (symbol (str (qname (.getEPackage this))
                 "." (.getName this))))

  EPackage
  (qname [this]
    (loop [p (.getESuperPackage this), n (.getName this)]
      (if p
        (recur (.getESuperPackage p) (str (.getName p) "." n))
        (symbol n))))

  EObject
  (qname [o]
    (qname (.eClass o))))

;;## EMF Model

(definline emf-model? [m]
  "Returns true if `m` is an EMFModel."
  `(instance? EMFModel ~m))

(defn new-model
  "Creates and returns a new, empty EMFModel.
  If `xmifile` is given (a file name as string), set it as file URI used by
  `save-model`."
  ([]
     (->EMFModel (XMIResourceImpl.)))
  ([^String xmifile]
     (->EMFModel (XMIResourceImpl. xmifile))))

(defn load-model
  "Loads an EMFModel from the XMI file `f`."
  [f]
  (let [f (if (instance? java.io.File f)
            (.getPath ^java.io.File f)
            f)
        uri (URI/createFileURI f)
        res (XMIResourceImpl. uri)]
    (doto (->EMFModel res)
      init-model-internal)))

;;## Type Checks

(defn ^:private type-matcher-emf-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        ^EClassifier type (eclassifier qname)]
    (if neg
      (if exact
        (fn [^EClass x] (not (identical? type (.eClass x))))
        (fn [^EClass x] (not (.isInstance type x))))
      (if exact
        (fn [^EClass x] (identical? type (.eClass x)))
        (fn [^EClass x] (.isInstance type x))))))

(defn ^:private type-matcher-emf
  [ts]
  (cond
   (nil? ts)   identity
   (fn? ts)    ts
   (qname? ts) (type-matcher-emf-1 ts)
   (eclass? ts) (fn [e] (.isInstance ^EClass ts e))
   (coll? ts)  (if (seq ts)
                 (let [f (first ts)
                       [op r] (case f
                                :and  [and-fn  (next ts)]
                                :nand [nand-fn (next ts)]
                                :or   [or-fn   (next ts)]
                                :nor  [nor-fn  (next ts)]
                                :xor  [xor-fn  (next ts)]
                                [or-fn ts])
                       t-matchers (map #(type-matcher-emf %) r)]
                   (apply op t-matchers))
                   ;; Empty collection given: (), [], that's also ok
                   identity)
     :else (errorf "Don't know how to create an EMF type-matcher for %s" ts)))

(extend-protocol ITypeMatcher
  EObject
  (type-matcher [m ts]
    (if-let [tm (cache/lookup type-matcher-cache ts)]
      (do (cache/hit type-matcher-cache ts) tm)
      (let [tm (type-matcher-emf ts)]
        (cache/miss type-matcher-cache ts tm)
        tm)))
  EMFModel
  (type-matcher [m ts]
    (if-let [tm (cache/lookup type-matcher-cache ts)]
      (do (cache/hit type-matcher-cache ts) tm)
      (let [tm (type-matcher-emf ts)]
        (cache/miss type-matcher-cache ts tm)
        tm))))

(extend-protocol IInstanceOf
  EObject
  (is-instance? [object class]
    (and (instance? EClass class)
         (.isInstance ^EClass class object)))
  (has-type? [obj spec]
    ((type-matcher obj spec) obj)))

;;## Traversal Stuff

(extend-protocol IEContents
  EObject
  (econtents-internal [this ts]
    (filter (type-matcher this ts)
            (seq (.eContents this))))
  (eallcontents-internal [this ts]
    (filter (type-matcher this ts)
            (iterator-seq (.eAllContents this))))
  (econtainer-internal [this]
    (.eContainer this))

  EMFModel
  (econtents-internal [this ts]
    (filter (type-matcher this ts)
            (seq (.getContents ^Resource (.resource this)))))
  (eallcontents-internal [this ts]
    (filter (type-matcher this ts)
            (iterator-seq (EcoreUtil/getAllProperContents
                           ^Resource (.resource this) true))))
  (eallobjects-internal [this ts]
    (eallcontents-internal this ts))

  clojure.lang.IPersistentCollection
  (econtents-internal [this tm]
    (mapcat #(econtents-internal % tm) this))
  (eallcontents-internal [this tm]
    (mapcat #(eallcontents-internal % tm) this)))

(defn eallcontents
  "Returns a seq of `x`s direct and indirect contents matching the type spec
`ts`."
  ([x]
     (eallcontents-internal x identity))
  ([x ts]
     (eallcontents-internal x ts)))

(defn econtents
  "Returns a seq of `x`s direct contents matching the type spec `ts`."
  ([x]
     (econtents-internal x identity))
  ([x ts]
     (econtents-internal x ts)))

(defn eallobjects
  "Returns a seq of all objects in `m` that match the type spec `ts`."
  ([m] (eallobjects-internal m identity))
  ([m ts] (eallobjects-internal m ts)))

(extend-protocol IElements
  EMFModel
  (elements
    ([this]
       (eallobjects this))
    ([this ts]
       (eallobjects this ts))))

(def ^{:doc "Returns the EObject containing `eo`."
       :arglists '([eo])}
  econtainer econtainer-internal)

(extend-protocol IContainer
  EObject
  (container [this]
    (econtainer-internal this)))

(defn eref-matcher
  "Returns a reference matcher for the reference spec `rs`.
  A reference matcher is a function of arity one that gets an EReference and
  returns logical true if that ref should be accepted, false otherwise.

  Semantics depend on `rs`:

    nil           => accept all references
    someERef      => accept only this EReference
    :foo          => accept only references named foo
    [:foo :bar]   => accept both foo and bar refs
    (fn [r] ...)   => simply use that"
  [rs]
  (cond
   (nil? rs)        identity
   (fn? rs)         rs
   (prop-name? rs)  (let [n (name rs)]
                      (fn [^EReference ref]
                        (= n (.getName ref))))
   (instance? EReference rs) (fn [r] (= rs r))
   (coll? rs)       (if (seq rs)
                      (apply some-fn (map eref-matcher rs))
                      ;; Empty collection given: (), [], that's also ok
                      identity)
   :else (errorf "Don't know how to create a reference matcher for %s" rs)))

(defn ^:private eopposite-refs
  "Returns the seq of `eo`s EClass' references whose opposites match `src-rm`.

  Example:

           [Foo] f --- b [Bar]
              f \\
                 `---- c [Car]

  Given a Foo object and a eref-matcher matching f, returns a seq of the
  IEReferences b and c, because those are the opposites of the matched f.  Of
  course, if `src-rm` matches only one specific EReference, i.e., it was
  constructed by (eref-matcher fERef) and not (eref-matcher :f)."
  [^EObject eo src-rm]
  (seq (remove nil? (map (fn [^EReference r]
                           (when-let [o (.getEOpposite r)]
                             (when (src-rm o) r)))
                         (seq (-> eo .eClass .getEAllReferences))))))

(defn ^:private search-ereferencers
  "Returns the seq of objects referencing `refed` by a reference matching `rm`
  that are contained in `container`.  `reffn` is either erefs-internal or
  ecrossrefs-internal."
  [refed reffn rm container]
  (filter (fn [o] (member? refed (reffn o rm)))
          (cond
           (instance? EMFModel container) (eallobjects container)
           (coll? container)              container
           :else (errorf "container is neither an EMFModel nor a collection: %s"
                         container))))

(extend-protocol IEReferences
  EMFModel
  (epairs-internal [this reffn src-rs trg-rs src-ts trg-ts]
    (let [done (atom #{})
          src-rm (eref-matcher src-rs)
          trg-rm (eref-matcher trg-rs)]
      (for [^EObject src (eallobjects this src-ts)
            ^EReference ref (seq (-> src .eClass .getEAllReferences))
            :when (not (member? ref @done))
            :when (trg-rm ref)
            :let [nthere-rm (eref-matcher ref)
                  oref (.getEOpposite ref)]
            :when (if oref
                    (src-rm oref)
                    true)
            trg (reffn src nthere-rm)
            :when (or (nil? trg-ts) (has-type? trg trg-ts))]
        (do
          (when oref (swap! done conj oref))
          [src trg]))))
  EObject
  (ecrossrefs-internal [this rm]
    (mapcat (fn [^EReference r]
              (if-let [x (.eGet this r)]
                (if (.isMany r)
                  x
                  [x])))
            (for [^EReference ref (seq (-> this .eClass .getEAllReferences))
                  :when (and (not (.isContainment ref))
                             (not (.isContainer ref))
                             (rm ref))]
              ref)))
  (erefs-internal [this rm]
    (mapcat (fn [^EReference r]
              (if-let [x (.eGet this r)]
                (if (.isMany r)
                  x
                  [x])))
            (for [^EReference ref (seq (-> this .eClass .getEAllReferences))
                  :when (rm ref)]
              ref)))
  (inv-erefs-internal [this rm container]
    (if container
      (search-ereferencers this erefs-internal rm container)
      (if-let [opposites (eopposite-refs this rm)]
        (erefs-internal this (eref-matcher opposites))
        (error "No opposite IEReferences found."))))
  (inv-ecrossrefs-internal [this rm container]
    (if container
      (search-ereferencers this ecrossrefs-internal rm container)
      (if-let [opposites (eopposite-refs this rm)]
        (ecrossrefs-internal this (eref-matcher opposites))
        (error "No opposite IEReferences found.")))))

(defn ecrossrefs
  "Returns a seq of EObjects cross-referenced by EObject`eo`, possibly
  restricted by the reference spec `rs`.  For the syntax and semantics of `rs`,
  see `eref-matcher`.  In EMF, crossrefs are all non-containment refs."
  ([eo]
     (ecrossrefs-internal eo identity))
  ([eo rs]
     (ecrossrefs-internal eo (eref-matcher rs))))

(defn erefs
  "Returns a seq of EObjects referenced by EObject `eo`, possibly restricted by
  the reference spec `rs`.  For the syntax and semantics of `rs`, see
  `eref-matcher`.  In contrast to `ecrossrefs`, this function doesn't ignore
  containment refs."
  ([eo]
     (erefs-internal eo identity))
  ([eo rs]
     (erefs-internal eo (eref-matcher rs))))

(defn inv-erefs
  "Returns the seq of EOjects that reference EObject `eo` with an EReference
  matching `rs` (see `eref-matcher`).  If no `container` is given, then only
  check the opposite refs of `eo`.  Else, all objects in `container` are tested
  if they reference `eo`.  `container` may be either an EMFModel or a
  collection of EObjects."
  ([eo]
     (inv-erefs-internal eo identity nil))
  ([eo rs]
     (inv-erefs-internal eo (eref-matcher rs) nil))
  ([eo rs container]
     (inv-erefs-internal eo (eref-matcher rs) container)))

(defn inv-ecrossrefs
  "Returns the seq of EOjects that cross-reference EObject `eo` with an
  EReference matching `rs` (see `eref-matcher`).  If no `container` is given,
  then only check the opposite refs of `eo`.  Else, all objects in `container`
  are tested if they cross-reference `eo`. `container` may be either an
  EMFModel or a collection of EObjects."
  ([eo]
     (inv-ecrossrefs-internal eo identity nil))
  ([eo rs]
     (inv-ecrossrefs-internal eo (eref-matcher rs) nil))
  ([eo rs container]
     (inv-ecrossrefs-internal eo (eref-matcher rs) container)))

(extend-protocol IEMFValues2ClojureValues
  UniqueEList
  (emf2clj-internal [this] (into (ordered-set) (seq this)))
  EMap
  (emf2clj-internal [this] (into (ordered-map) (seq this)))
  EList
  (emf2clj-internal [this] (into (vector) this))
  EObject
  (emf2clj-internal [this] this)
  Number
  (emf2clj-internal [this] this)
  String
  (emf2clj-internal [this] this)
  Boolean
  (emf2clj-internal [this] this)
  nil
  (emf2clj-internal [_] nil))

(defn emf2clj
  "Converts an EMF value (e.g., an EList) to an appropriate clojure value."
  [val]
  (emf2clj-internal val))

(defn eget-raw
  "Returns the value of `eo`s structural feature `sf`.
  Throws an exception, if there's no EStructuralFeature `sf`.

  The value is kept as-is, i.e., not converted to some immutable clojure data
  structure as `eget` does.  So if you eget-raw an EList, you can mutate it
  in-place.  That's totally not stylish, but it might be a last resort when
  optimizing for performance.  You've been warned!"
  [^EObject eo sf]
  (if-let [sfeat (if (instance? EStructuralFeature sf)
                   sf
                   (.getEStructuralFeature (.eClass eo) (name sf)))]
    (.eGet eo sfeat)
    (errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eget
  "Returns the value of `eo`s structural feature `sf`.
  The value is converted to some clojure type (see IEMFValues2ClojureValues protocol).
  Throws an exception, if there's no EStructuralFeature `sf`."
  [^EObject eo sf]
  (emf2clj-internal (eget-raw eo sf)))

(defn eset!
  "Sets `eo`s structural feature `sf` to `value` and returns `eo`.
  Throws an exception, if there's no EStructuralFeature `sf`."
  [^EObject eo sf value]
  (if-let [sfeat (.getEStructuralFeature (.eClass eo) (name sf))]
    (if (.isMany sfeat)
      (do
        (.eUnset eo sfeat)
        (.addAll ^EList (.eGet eo sfeat) value)
        eo)
      (doto eo
        (.eSet sfeat value)))
    (errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eunset!
  "Unsets `eo`s structural feature `sf` and returns `eo`.
  Throws an exception, if there's no EStructuralFeature `sf`."
  [^EObject eo sf]
  (if-let [sfeat (if (instance? EStructuralFeature sf)
                   sf
                   (.getEStructuralFeature (.eClass eo) (name sf)))]
    (doto eo
      (.eUnset sfeat))
    (errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eadd!
  "Adds `value` and `more` values to `eo`s list of attribute/reference values
  denoted by `sf` and returns `eo`.  Throws an exception, if there's no
  EStructuralFeature `sf`.

  In the arity-2 version, adds `obj` to `model` and returns `obj`."
  ([eo sf value & more]
     (let [^EList l (eget-raw eo sf)]
       (.add l value)
       (when (seq more)
         (.addAll l more))
       eo))
  ([^EMFModel model obj]
     (.add (.getContents ^Resource (.resource model))
           obj)
     obj))

(defn eaddall!
  "Adds all values in `coll` to `eo`s `sf` structural feature.
  In the arity 2 variant, adds all EObjects in `coll` to `model`."
  ([eo sf coll]
     (let [^EList l (eget-raw eo sf)]
       (.addAll l coll)
       eo))
  ([^EMFModel model coll]
     (.addAll (.getContents ^Resource (.resource model))
              coll)))

(defn eremove!
  "Removes `value` and `more` values from `eo`s list of attribute/reference
  values denoted by `sf` and returns `eo`.  Throws an exception, if there's no
  EStructuralFeature `sf`.

  In the arity-2 version, removes `obj` from `model` and returns `model`.
  Note that it won't delete `obj` or remove references to it."
  ([eo sf value & more]
     (let [^EList l (eget-raw eo sf)]
       (.remove l value)
       (when (seq more)
         (.removeAll l more))
       eo))
  ([^EMFModel model obj]
     (.remove (.getContents ^Resource (.resource model))
              obj)))

;;### Generic attribute access

(extend-protocol IAttributeValueAccess
  EObject
  (aval [this attr]
    (let [^EStructuralFeature sf (.getEStructuralFeature (.eClass this) (name attr))]
      (if (instance? EAttribute sf)
        (emf2clj-internal (.eGet this sf))
        (if (nil? sf)
          (errorf "No such attribute %s at object %s." attr this)
          (errorf "%s is no attribute of object %s." sf this)))))
  (set-aval! [this attr val]
    (let [^EStructuralFeature sf (.getEStructuralFeature (.eClass this) (name attr))]
      (cond
       (nil? sf) (errorf "No such attribute %s at object %s." attr this)
       (instance? EReference sf) (errorf "%s is no attribute of object %s but a reference." sf this)))
    (eset! this attr val)))


;;## Edges, i.e., src/trg tuples

(defn eallpairs
  "Returns the seq of all edges in terms of [src trg] pairs.
  This includes both containment as well as crossreferences.  Restrictions may
  be defined in terms of reference specs `src-rs` and `trg-rs`, and reference
  specs plus type specs `src-ts` and `trg-ts`."
  ([m]
     (epairs-internal m erefs-internal identity identity nil nil))
  ([m src-rs trg-rs]
     (epairs-internal m erefs-internal src-rs trg-rs nil nil))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m erefs-internal src-rs trg-rs src-ts trg-ts)))

(defn ecrosspairs
  "Returns the seq of all cross-reference edges in terms of [src trg] pairs.
  Restrictions may be defined in terms of reference specs `src-rs` and
  `trg-rs`, and reference specs plus type specs `src-ts` and `trg-ts`."
  ([m]
     (epairs-internal m ecrossrefs-internal identity identity nil nil))
  ([m src-rs trg-rs]
     (epairs-internal m ecrossrefs-internal src-rs trg-rs nil nil))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m ecrossrefs-internal src-rs trg-rs src-ts trg-ts)))

(defn ^:private econtents-by-ref
  [^EObject eo rm]
  (mapcat #(when-let [o (eget eo %)]
             (if (coll? o) o [o]))
          (for [^EReference r (-> eo .eClass .getEAllReferences)
                :when (and (.isContainment r) (rm r))]
            r)))

(defn econtentpairs
  "Returns the seq of all containment edges in terms of [src trg] pairs.
  src is the parent, trg is the child.
  Restrictions may be defined in terms of reference specs `src-rs` and
  `trg-rs`, and reference specs plus type specs `src-ts` and `trg-ts`."
  ([m]
     (epairs-internal m econtents-by-ref identity identity nil nil))
  ([m src-rs trg-rs]
     (epairs-internal m econtents-by-ref src-rs trg-rs nil nil))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m ecrossrefs-internal src-rs trg-rs src-ts trg-ts)))


;;## EObject Creation

(defn ecreate!
  "Creates an EObject of EClass `ecls`.
  `ecls` may be either an EClass or just an EClass name given as symbol.
  `props` are optional property-value pairs to be set, where
  properties (attributes and references) are represented as keywords.  Since
  props are set using `eset!`, the value of a multi-valued reference must be a
  collection of EObjects."
  [ecls & props]
  (let [eo (EcoreUtil/create (if (instance? EClass ecls)
                               ecls
                               (eclassifier ecls)))]
    (doseq [[prop val] (partition 2 2 (repeatedly #(errorf "attr-vals not paired: %s" props))
                                  props)]
      (eset! eo prop val))
    eo))

(extend-protocol ICreateElement
  EMFModel
  (create-element! [model cls]
    (let [e (ecreate! cls)]
      (eadd! model e)
      e)))

;;## Generic setting of props

(extend-protocol IModifyAdjacencies
  EObject
  (set-adjs! [o role os]
    (eset! o role os))
  (set-adj! [o1 role o2]
    (eset! o1 role o2))
  (add-adjs! [o role os]
    (apply eadd! o role (first os) (rest os)))
  (add-adj! [o1 role o2]
    (eadd! o1 (name role) o2)))

;;## EObject Deletion

(extend-protocol IDeletable
  EObject
  (delete!
    ([this]
       (EcoreUtil/delete this true)
       this)
    ([this recursive]
       ;; Gotta provide a real boolean, not just a truthy thingy
       (EcoreUtil/delete this (boolean recursive))
       this)))

(defn edelete!
  "Unsets all references of `eo` and removes it from its containing resource
  and containing EObject.  If `recursively` is true, first edelete! all
  contents of `eo`.

  If `eo` isn't cross-referenced unidirectional, this is equivalent
  to `(delete! eo)` but faster.  If `eo` is cross-referenced unidirectional,
  these objects will still reference `eo` after the call, so use `delete!`
  instead of `edelete!` in that case."
  ([^EObject eo]
     (edelete! eo true))
  ([^EObject eo recursively]
     (when recursively
       (doseq [ceo (.eContents eo)]
         (edelete! ceo)))
     (when-let [^EReference cf (.eContainmentFeature eo)]
       (eremove! (.eContainer eo) cf eo))
     (doseq [ref (.getEAllReferences (.eClass eo))]
       (eunset! eo ref))
     (when-let [^Resource res (.eResource eo)]
       (.remove (.getContents res) eo))))

;;# Adjancencies

(defn ^:private eget-ref ^EReference [^EObject eo ref allow-unknown-ref single-valued]
  (if-let [^EStructuralFeature sf (.getEStructuralFeature (.eClass eo) (name ref))]
    (if (instance? EReference sf)
      (if single-valued
        (let [ub (.getUpperBound sf)]
          (if (== 1 ub)
            (.eGet eo sf)
            (errorf "Must not call adj on EReference '%s' with upper bound %s."
                    sf ub)))
        (.eGet eo sf))
      (errorf "'%s' at %s is no EReference." sf eo))
    (when-not allow-unknown-ref
      (errorf "No such structural feature '%s' at %s." ref eo))))

(extend-protocol IAdjacencies
  EObject
  (adj-internal [this roles]
    (if (seq roles)
      (when-let [a (emf2clj (eget-ref this (first roles) false true))]
        (recur a (rest roles)))
      this))
  (adj*-internal [this roles]
    (if (seq roles)
      (when-let [a (emf2clj (eget-ref this (first roles) true true))]
        (recur a (rest roles)))
      this))
  (adjs-internal [this roles]
    (if (seq roles)
      (when-let [a (eget-ref this (first roles) false false)]
        (r/mapcat #(adjs-internal % (rest roles))
                  (if (instance? java.util.Collection a) a [a])))
      [this]))
  (adjs*-internal [this roles]
    (if (seq roles)
      (when-let [a (eget-ref this (first roles) true false)]
        (r/mapcat #(adjs*-internal % (rest roles))
                  (if (instance? java.util.Collection a) a [a])))
      [this])))

;;# Describing EObjects and EClasses

(extend-protocol IDescribable
  org.eclipse.emf.ecore.EClass
  (describe [this]
    {:name (qname this)
     :abstract (.isAbstract this)
     :interface (.isInterface this)
     :superclasses (seq (.getESuperTypes this))
     :attributes (into {}
                       (map (fn [^org.eclipse.emf.ecore.EAttribute attr]
                              [(keyword (.getName attr)) (.getEType attr)])
                            (seq (.getEAttributes this))))
     :references (into {}
                       (map (fn [^org.eclipse.emf.ecore.EReference ref]
                              [(keyword (.getName ref)) (.getEReferenceType ref)])
                            (seq (.getEReferences this))))})
  org.eclipse.emf.ecore.EObject
  (describe [this]
    {:eclass (qname this)
     :container (econtainer this)
     :attr-slots (into {}
                       (map (fn [^org.eclipse.emf.ecore.EAttribute attr]
                              (let [kw (keyword (.getName attr))]
                                [kw (eget this kw)]))
                            (seq (.getEAllAttributes (.eClass this)))))
     :ref-slots (into {}
                       (map (fn [^org.eclipse.emf.ecore.EReference ref]
                              (let [kw (keyword (.getName ref))]
                                [kw (eget this kw)]))
                            (seq (.getEAllReferences (.eClass this)))))}))

;;# Printing

;; TODO: We don't handle EFactories, ETypedElements, and EAnnotations yet.

(defn ^:private feature-str
  "Returns a description of enabled features `fs`.
  fs => [test-val desc-str]*"
  ([fs]
     (feature-str [] fs))
  ([s fs]
     (if (seq fs)
       (let [[f n] (first fs)]
         (recur (if f (conj s n) s)
                (rest fs)))
       (when-let [r (seq s)]
         (str " " r)))))

(defmethod print-method EClass
  [^EClass ec ^java.io.Writer out]
  (.write out
          (str "#<EClass "
               (qname ec)
               (feature-str
                [[(.isAbstract ec)  :abstract]
                 [(.isInterface ec) :interface]])
               ">")))

(defmethod print-method EEnum
  [^EEnum en ^java.io.Writer out]
  (.write out
          (str "#<EEnum " (qname en)
               #_(feature-str
                  [[(.isSerializable en) :serializable]])
               ">")))

(defmethod print-method EDataType
  [^EDataType edt ^java.io.Writer out]
  (.write out
          (str "#<EDataType " (qname edt)
               #_(feature-str
                  [[(.isSerializable edt) :serializable]])
               ">")))

(defmethod print-method EPackage
  [^EPackage ep ^java.io.Writer out]
  (.write out
          (str "#<"
               (if (instance? EcorePackage ep)
                 "EcorePackage "
                 "EPackage ")
               (qname ep)
               (let [m (into {}
                             (map (fn [[v k]]
                                    (when v
                                      [k v]))
                                  [[(.getNsPrefix ep) :nsPrefix]
                                   [(.getNsURI ep)    :nsURI]]))]
                 (when (seq m)
                   (str " " m)))
               ">")))

(defmethod print-method EEnumLiteral
  [^EEnumLiteral el ^java.io.Writer out]
  (.write out
          (str "#<EEnumLiteral"
               (-> el .getEEnum .getName)
               "/"
               (.getLiteral el)
               ">")))

(defmethod print-method EObject
  [^EObject eo ^java.io.Writer out]
  (.write out
          (if (or (instance? EFactory      eo)
                  (instance? ETypedElement eo)
                  (instance? EAnnotation   eo))
            ;; Usual toString() for those
            (str eo)
            ;; Custom toString() for the others
            (str "#<"
                 (.getName (.eClass eo))
                 "@" (or (EcoreUtil/getID eo)
                         (Integer/toHexString (hash eo)))
                 ">"))))


