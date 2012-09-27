(ns funnyqt.emf
  "Core functions for accessing and manipulating EMF models."
  (:use funnyqt.protocols)
  (:use funnyqt.emf-protocols)
  (:use funnyqt.utils)
  (:use funnyqt.query)
  (:use ordered.set)
  (:use ordered.map)
  (:require clojure.java.shell)
  (:import
   [funnyqt.emf_protocols EMFModel]
   [org.eclipse.emf.ecore.xmi XMLResource]
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl XMIResourceFactoryImpl]
   [org.eclipse.emf.ecore.util EcoreUtil]
   [org.eclipse.emf.common.util URI EList UniqueEList EMap]
   [org.eclipse.emf.ecore.resource Resource ResourceSet]
   [org.eclipse.emf.ecore.resource.impl ResourceImpl]
   [org.eclipse.emf.ecore
    EcorePackage EPackage EPackage$Registry EObject EModelElement EClassifier EClass
    EDataType EEnumLiteral EEnum EFactory ETypedElement EAnnotation EAttribute EReference
    EStructuralFeature]))

;;# Metamodel

(defn load-metamodel
  "Loads the EcoreModel from the ecore file `f`.
  All EPackages are registered."
  [f]
  (let [f (if (instance? java.io.File f)
            (.getPath ^java.io.File f)
            f)
        uri (URI/createFileURI f)
        res (XMIResourceImpl. uri)]
    (doto (->EcoreModel res)
      load-and-register-internal)))

(def save-metamodel save-metamodel-internal)

(def metamodel-epackages metamodel-epackages-internal)

(def ^:dynamic *ns-uris* nil)
(defmacro with-ns-uris
  "Restricts the EClassifier lookup in the dynamic scope of `body` to those
  contained in top-level EPackages registered with the given URIs at the
  EPackage registry and subpackages thereof."
  [uris & body]
  `(binding [*ns-uris* ~uris]
     ~@body))

(defn epackages
  "The lazy seq of all registered EPackages.
  If a `root-ns-uri' is given, returns the this EPackages and all contained
  packages."
  ([]
     (with-system-class-loader
       (map #(.getEPackage EPackage$Registry/INSTANCE %)
            (or *ns-uris* (keys EPackage$Registry/INSTANCE)))))
  ([root-ns-uri]
     (vals (@uri->qname->pkg-map root-ns-uri))))

(defn epackage
  "Returns the EPackage with the given (simple or qualified) `name`.
  If `root-ns-uri` is given, look only in this package and subpackages."
  ([name]
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
  ([root-ns-uri name]
     (let [^String n (clojure.core/name name)]
       (if (.contains n ".")
         ((@uri->qname->pkg-map root-ns-uri) n)
         (let [pkgs (filter (fn [^EPackage p]
                              (= n (.getName p)))
                            (epackages root-ns-uri))]
           (when-not (seq pkgs)
             (errorf "No such package %s in %s." n root-ns-uri))
           (when (nnext pkgs)
             (errorf "Multiple packages named %s in %s: %s" n root-ns-uri pkgs))
           (first pkgs))))))

(extend-protocol Abstractness
  EClass
  (abstract? [this]
    (.isAbstract this)))

(defn eclassifiers
  "The lazy seq of EClassifiers.
  May be restricted to all EClassifiers contained below the root package with
  the given nsURI."
  ([]
     (mapcat (fn [^EPackage ep]
               (.getEClassifiers ep))
             (epackages)))
  ([root-ns-uri]
     (mapcat (fn [^EPackage ep]
               (.getEClassifiers ep))
             (epackages root-ns-uri))))

(defn eclassifier
  "Returns the eclassifier with the given `name`.
  `name` may be a simple or qualified name.  Throws an exception if no such
  classifier could be found, or if the given simple name is ambiguous."
  ([name]
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
            :else (first classifiers))))))
  ([root-ns-uri name]
     (let [^String n (clojure.core/name name)
           ld (.lastIndexOf n ".")]
       (if (>= ld 0)
         (if-let [^EPackage ep (epackage root-ns-uri (subs n 0 ld))]
           (or (.getEClassifier ep (subs n (inc ld)))
               (errorf "No such EClassifier %s in %s." n (print-str ep)))
           (errorf "No such EPackage %s in %s." (subs n 0 ld) root-ns-uri))
         (let [classifiers (filter (fn [^EClassifier ec]
                                     (= (.getName ec) n))
                                   (eclassifiers root-ns-uri))]
           (cond
            (empty? classifiers) (errorf "No such EClassifier %s in %s." n root-ns-uri)
            (next classifiers)   (errorf "EClassifier %s is ambiguous in %s: %s"
                                         n root-ns-uri classifiers)
            :else (first classifiers)))))))

(defn eenum-literal
  "Returns the EEnumLiteral specified by its `qname`."
  ([qname]
     (let [[eenum elit] (split-qname qname)]
       (if-let [^EEnum enum-cls (eclassifier eenum)]
         (or (.getEEnumLiteral enum-cls ^String elit)
             (errorf "%s has no EEnumLiteral with name %s."
                     (print-str enum-cls) elit))
         (errorf "No such EEnum %s." eenum))))
  ([root-ns-uri qname]
     (let [[eenum elit] (split-qname qname)]
       (if-let [^EEnum enum-cls (eclassifier root-ns-uri eenum)]
         (or (.getEEnumLiteral enum-cls ^String elit)
             (errorf "%s has no EEnumLiteral with name %s."
                     (print-str enum-cls) elit))
         (errorf "No such EEnum %s in %s." eenum root-ns-uri)))))

;;# Model

;; TODO: Add arglists and docs
(def add-eobject! add-eobject!-internal)
(def add-eobjects! add-eobjects!-internal)
(def clone-model clone-model-internal)
(def save-model save-model-internal)

;;# Simple type predicates

(definline eobject?
  "Returns true if `eo` is an EObject."
  [eo]
  `(instance? EObject ~eo))

;;## Qualified Names

(extend-protocol QualifiedName
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

(defn new-model
  "Creates and returns a new, empty EMFModel."
  []
  (->EMFModel (ResourceImpl.)))

(defn load-model
  "Loads an EMF model from the XMI file `f`.
  Returns a seq of the models top-level elements."
  [f]
  (let [f (if (instance? java.io.File f)
            (.getPath ^java.io.File f)
            f)
        uri (URI/createFileURI f)
        res (XMIResourceImpl. uri)]
    (doto (->EMFModel res)
      init-model-internal)))

;;## Traversal stuff

(defn ^:private eclass-matcher-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [root-ns-uri c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        ^EClassifier type (if root-ns-uri
                            (eclassifier root-ns-uri qname)
                            (eclassifier qname))]
    (if neg
      (if exact
        (fn [^EClass x] (not (identical? type (.eClass x))))
        (fn [^EClass x] (not (.isInstance type x))))
      (if exact
        (fn [^EClass x] (identical? type (.eClass x)))
        (fn [^EClass x] (.isInstance type x))))))

(defn eclass-matcher
  "Returns a matcher for either nil, !Foo!, [Foo Bar! !Baz], [:and 'Foo 'Bar],
  or [:or 'Foo 'Bar].  In a collection spec, the first element may be one of
  the keywords :or (default), :nor, :and, :nand, or :xor with the usual logic
  semantics."
  ([ts]
     (eclass-matcher nil ts))
  ([root-ns-uri ts]
     (cond
      (nil? ts)   identity
      (fn? ts)    ts
      (qname? ts) (eclass-matcher-1 root-ns-uri ts)
      (coll? ts)  (if (seq ts)
                    (let [f (first ts)
                          [op r] (case f
                                   :and  [every-pred (next ts)]
                                   :nand [nand-fn    (next ts)]
                                   :or   [some-fn    (next ts)]
                                   :nor  [nor-fn     (next ts)]
                                   :xor  [xor-fn     (next ts)]
                                   [some-fn    ts])]
                      (apply op (map #(eclass-matcher root-ns-uri %) r)))
                    ;; Empty collection given: (), [], that's also ok
                    identity)
      :else (errorf "Don't know how to create a type matcher for %s" ts))))

(extend-protocol InstanceOf
  EObject
  (is-instance? [object class]
    (and (instance? EClass class)
         (.isInstance ^EClass class object)))
  (has-type? [obj spec]
    ;; TODO: Isn't yet worth it, the bottle-neck is `eclassifier`.
    (let [root-ns (eroot-package-nsuri (.getEPackage (.eClass obj)))]
      (if (qname? spec)
        (let [[neg cls ex] (type-with-modifiers (name spec))
              ^EClass ec (eclassifier root-ns cls)
              r (if ex
                  (identical? (.eClass obj) ec)
                  (.isInstance ec obj))]
          (if neg (not r) r))
        ((eclass-matcher root-ns spec) obj)))
    #_((eclass-matcher (eroot-package-nsuri (.getEPackage (.eClass obj))) spec) obj)))

(extend-protocol EContents
  EObject
  (econtents-internal [this tm]
    (filter tm (seq (.eContents this))))
  (eallcontents-internal [this tm]
    (filter tm (iterator-seq (.eAllContents this))))
  (econtainer-internal [this]
    (.eContainer this))

  EMFModel
  (econtents-internal [this tm]
    (filter tm (seq (.getContents ^Resource (.resource this)))))
  (eallcontents-internal [this tm]
    (filter tm (iterator-seq (.getAllContents ^Resource (.resource this)))))
  (eallobjects-internal [this tm]
    (eallcontents-internal this tm))

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
     (if (eobject? x)
       (eallcontents-internal x (eclass-matcher (eroot-package-nsuri
                                                 (.getEPackage (.eClass ^EObject x)))
                                                ts))
       (eallcontents-internal x (eclass-matcher ts)))))

(defn econtents
  "Returns a seq of `x`s direct contents matching the type spec `ts`."
  ([x]
     (econtents-internal x identity))
  ([x ts]
     (if (eobject? x)
       (econtents-internal x (eclass-matcher (eroot-package-nsuri
                                              (.getEPackage (.eClass ^EObject x)))
                                             ts))
       (econtents-internal x (eclass-matcher ts)))))

(defn eallobjects
  "Returns a seq of all objects in `m` that match the type spec `ts`."
  ([m] (eallobjects-internal m identity))
  ([m ts] (eallobjects-internal m (eclass-matcher ts))))

(def econtainer econtainer-internal)

(defn eref-matcher
  "Returns a reference matcher for the reference spec `rs`.
  A reference matcher is a function of arity one that gets an EReference and
  returns logical true if that ref should be accepted, false otherwise.

  Semantics depend on `rs`:

    nil           => accept all references
    someERef      => accept only this EReference
    :foo          => accept only references named foo
    [:foo :bar]   => accept both foo and bar refs
    (fn [r] ...)  => simply use that"
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
  EReferences b and c, because those are the opposites of the matched f.  Of
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

(extend-protocol EReferences
  EMFModel
  (epairs-internal [this reffn src-rm trg-rm src-tm trg-tm]
    (let [done (atom #{})]
      (for [^EObject src (eallobjects this src-tm)
            ^EReference ref (seq (-> src .eClass .getEAllReferences))
            :when (not (member? ref @done))
            :when (trg-rm ref)
            :let [nthere-rm (eref-matcher ref)
                  oref (.getEOpposite ref)]
            :when (if oref
                    (src-rm oref)
                    true)
            trg (reffn src nthere-rm)
            :when (trg-tm trg)]
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
        (error "No opposite EReferences found."))))
  (inv-ecrossrefs-internal [this rm container]
    (if container
      (search-ereferencers this ecrossrefs-internal rm container)
      (if-let [opposites (eopposite-refs this rm)]
        (ecrossrefs-internal this (eref-matcher opposites))
        (error "No opposite EReferences found."))))

  clojure.lang.IPersistentCollection
  (ecrossrefs-internal [this rm]
    (mapcat #(ecrossrefs-internal % rm) this))
  (erefs-internal [this rm]
    (mapcat #(erefs-internal % rm) this))
  (inv-erefs-internal [this rm container]
    (mapcat #(inv-erefs-internal % rm container) this))
  (inv-ecrossrefs-internal [this rm container]
    (mapcat #(inv-ecrossrefs-internal % rm container) this)))

(defn ecrossrefs
  "Returns a seq of EObjects cross-referenced by `eo`, possibly restricted by
  the reference spec `rs`.  `eo` may be an EObject or a collection of EObjects.
  For the syntax and semantics of `rs`, see `eref-matcher`.  In EMF, crossrefs
  are all non-containment refs."
  ([eo]
     (ecrossrefs-internal eo identity))
  ([eo rs]
     (ecrossrefs-internal eo (eref-matcher rs))))

(defn erefs
  "Returns a seq of EObjects referenced by `eo`, possibly restricted by the
  reference spec `rs`.  `eo` may be an EObject or a collection of EObjects.
  For the syntax and semantics of `rs`, see `eref-matcher`.  In contrast to
  `ecrossrefs`, this function doesn't ignore containment refs."
  ([eo]
     (erefs-internal eo identity))
  ([eo rs]
     (erefs-internal eo (eref-matcher rs))))

(defn inv-erefs
  "Returns the seq of EOjects that reference `eo` with an EReference matching
  `rs` (see `eref-matcher`).  `eo` may also be a collection of eobjects.  If no
  `container` is given, then only check the opposite refs of `eo`.  Else, all
  objects in `container` are tested if they reference `eo`.  `container` may be
  either an EMFModel or a collection of EObjects."
  ([eo]
     (inv-erefs-internal eo identity nil))
  ([eo rs]
     (inv-erefs-internal eo (eref-matcher rs) nil))
  ([eo rs container]
     (inv-erefs-internal eo (eref-matcher rs) container)))

(defn inv-ecrossrefs
  "Returns the seq of EOjects that cross-reference `eo` with an EReference
  matching `rs` (see `eref-matcher`).  `eo` may also be a collection of
  eobjects.  If no `container` is given, then only check the opposite refs of
  `eo`.  Else, all objects in `container` are tested if they cross-reference
  `eo`. `container` may be either an EMFModel or a collection of EObjects. "
  ([eo]
     (inv-ecrossrefs-internal eo identity nil))
  ([eo rs]
     (inv-ecrossrefs-internal eo (eref-matcher rs) nil))
  ([eo rs container]
     (inv-ecrossrefs-internal eo (eref-matcher rs) container)))

(extend-protocol EmfToClj
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
  The value is converted to some clojure type (see EmfToClj protocol).
  Throws an exception, if there's no EStructuralFeature `sf`."
  [^EObject eo sf]
  (emf2clj-internal (eget-raw eo sf)))

(defn eset!
  "Sets `eo`s structural feature `sf` to `value` and returns `eo`.
  The value is converted to some EMF type (see CljToEmf protocol).
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
  (if-let [sfeat (.getEStructuralFeature (.eClass eo) (name sf))]
    (doto eo
      (.eUnset sfeat))
    (errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eadd!
  "Adds `value` and `more` values to `eo`s list of attribute/reference values
  denoted by `sf` and returns `eo`.  Throws an exception, if there's no
  EStructuralFeature `sf`.

  In the arity-2 version, adds `obj` to `model` and returns `model`."
  ([eo sf value & more]
     (let [^EList l (eget-raw eo sf)]
       (.add l value)
       (when (seq more)
         (.addAll l more))
       eo))
  ([^EMFModel model obj]
     (.add (.getContents ^Resource (.resource model))
           obj)))

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

;;## Edges, i.e., src/trg tuples

(defn eallpairs
  "Returns the seq of all edges in terms of [src trg] pairs.
  This includes both containment as well as crossreferences.  Restrictions may
  be defined in terms of reference specs `src-rs` and `trg-rs`, and reference
  specs plus type specs `src-ts` and `trg-ts`."
  ([m]
     (epairs-internal m erefs-internal
                      identity identity
                      identity identity))
  ([m src-rs trg-rs]
     (epairs-internal m erefs-internal
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      identity identity))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m erefs-internal
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      (eclass-matcher src-ts) (eclass-matcher trg-ts))))

(defn ecrosspairs
  "Returns the seq of all cross-reference edges in terms of [src trg] pairs.
  Restrictions may be defined in terms of reference specs `src-rs` and
  `trg-rs`, and reference specs plus type specs `src-ts` and `trg-ts`."
  ([m]
     (epairs-internal m ecrossrefs-internal
                      identity identity
                      identity identity))
  ([m src-rs trg-rs]
     (epairs-internal m ecrossrefs-internal
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      identity identity))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m ecrossrefs-internal
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      (eclass-matcher src-ts) (eclass-matcher trg-ts))))

(defn ^:private econtents-by-ref
  [^EObject eo rm]
  (mapcat #(let [o (eget eo %)]
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
     (epairs-internal m econtents-by-ref
                      identity identity
                      identity identity))
  ([m src-rs trg-rs]
     (epairs-internal m econtents-by-ref
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      identity identity))
  ([m src-rs trg-rs src-ts trg-ts]
     (epairs-internal m ecrossrefs-internal
                      (eref-matcher src-rs) (eref-matcher trg-rs)
                      (eclass-matcher src-ts) (eclass-matcher trg-ts))))


;;## EObject Creation

(defn ecreate!
  "Creates an EObject of EClass `ecls`.
  `ecls` may be either an EClass or just an EClass name given as symbol,
  string, or keyword.  If a `model` is provided, then add the new EObject to
  it."
  ([ecls]
     (EcoreUtil/create (if (instance? EClass ecls)
                         ecls
                         (eclassifier ecls))))
  ([model ecls]
     (add-eobject! model (ecreate! ecls))))

;;## EObject Deletion

(extend-protocol Deletable
  EObject
  (delete!
    ([this]
       (EcoreUtil/delete this true)
       this)
    ([this recursive]
       ;; Gotta provide a real boolean, not just a truthy thingy
       (EcoreUtil/delete this (if recursive true false))
       this)))

;;## Visualization

(def ^{:private true, :dynamic true
       :doc "Opposite refs: those are not dotted, cause we already
  printed them from the other direction."}
  *opposite-refs*)

(def ^{:private true, :dynamic true
       :doc "Only these objects (minus excluded ones) are printed."}
  *included-eobjects*)

(def ^{:private true, :dynamic true
       :doc "Objects to be skipped from printing."}
  *excluded-eobjects*)

(def ^{:private true, :dynamic true
       :doc "These objects are printed in color."}
  *marked-eobjects*)

(defn ^:private dot-included? [eo]
  (and (or (not *included-eobjects*) ;; Not set ==> all are included
           (*included-eobjects* eo))
       (not (*excluded-eobjects* eo))))

(defn ^:private dot-id [eo]
  (str "O" (Integer/toString (hash eo) (Character/MAX_RADIX))))

(defn ^:private dot-attributes [^EObject eo]
  (reduce str
          (for [^EAttribute attr (.getEAllAttributes (.eClass eo))
                :let [n (.getName attr)]]
            (str n " = \\\"" (eget eo n) "\\\"\\l"))))

(defn ^:private dot-eobject [eo]
  (when-not (*excluded-eobjects* eo)
    (let [h (dot-id eo)]
      (str "  " h
           " [label=\"{{" (qname eo) "}|"
           (dot-attributes eo)
           "}\", shape=record, fontname=Sans, fontsize=14, "
           "color=" (if (*marked-eobjects* eo)
                      "red" "black")
           "];\n"))))

(defn ^:private dot-contentrefs [^EObject eo]
  (let [h (dot-id eo)
        dist (atom [2.0 4.0])]
    (reduce str
            (for [^EReference ref (.getEAllContainments (.eClass eo))
                  :let [oref (.getEOpposite ref)
                        n (.getName ref)]
                  t (eget eo ref)
                  :when (dot-included? t)]
              (do
                (swap! dist (fn [[x y]] [y x]))
                (str "  " h " -> " (dot-id t)
                     " [dir=both, arrowtail=diamond, fontname=Sans, "
                     "labelangle=0, labeldistance= " (first @dist) ", "
                     "label=\"                    \", "
                     "headlabel=\"" n "\""
                     (when oref
                       (str ", taillabel=\"" (.getName oref) "\""))
                     "];\n"))))))

(defn ^:private dot-crossrefs [^EObject eo]
  (let [h (dot-id eo)
        dist (atom  [2.0 4.0])]
    (reduce str
            (for [^EReference ref (.getEAllReferences (.eClass eo))
                  :when (not (member? ref @*opposite-refs*))
                  :when (not (or (.isContainment ref)
                                 (.isContainer ref)))
                  :let [oref (.getEOpposite ref)]
                  t (ecrossrefs eo ref)
                  :when (dot-included? t)
                  :let [h2 (dot-id t)]]
              (do
                (when oref
                  (swap! *opposite-refs* conj oref))
                (swap! dist (fn [[x y]] [y x]))
                (str "  " h " -> " h2
                     " [dir="
                     (if oref "none" "forward")
                     ", fontname=Sans, "
                     "labelangle=0, labeldistance= " (first @dist) ", "
                     "label=\"                    \", "
                     "headlabel=\"" (.getName ref) "\""
                     (when oref
                       (str ", taillabel=\"" (.getName oref) "\""))
                     "];\n"))))))

(defn ^:private dot-ereferences [eo]
  (when (dot-included? eo)
    (str (dot-contentrefs eo)
         (dot-crossrefs eo))))

(defn ^:private dot-options [opts]
  (letfn [(update [m k v]
            (if (get m k)
              m
              (assoc m k v)))]
    (let [m (apply hash-map opts)
          ;; :name is special and no DOT attr, so remove it
          gname (or (:name m) "EMFModel")
          m (dissoc m :name)
          ;; ditto for :include
          include (:include m)
          m (dissoc m :include)
          ;; ditto for :exclude
          exclude (:exclude m)
          m (dissoc m :exclude)
          ;; ditto for :mark
          mark (:mark m)
          m (dissoc m :mark)
          ;; Add default values
          m (update m :ranksep 1.5)]
      (with-meta m
        {:name gname, :include include, :exclude exclude, :mark mark}))))

(defn ^:private dot-model [m opts]
  (let [opts (dot-options opts)]
    (binding [*included-eobjects* (when-let [i (:include (meta opts))]
                                    (set i))
              *excluded-eobjects* (set (:exclude (meta opts)))
              *marked-eobjects* (set (:mark (meta opts)))]
      (str "digraph " (:name (meta opts)) " {"
           (clojure.string/join
            \,
            (for [[k v] opts]
              (str (name k) "=" v)))
           ";\n\n"
           (reduce str
                   (map dot-eobject
                        (eallobjects m)))
           (binding [*opposite-refs* (atom #{})]
             (reduce str
                     (map dot-ereferences
                          (eallobjects m))))
           "}"))))

(defn print-model
  "Prints a visualization of EMFModel `m` to the file `f`.
  The file type is determined by its extension (dot, xdot, ps, svg, svgz, png,
  gif, pdf) and defaults to PDF.  The extension `gtk` has a special meaning: in
  that case, no file is actually printed, but instead a GTK+ window showing the
  model is created.

  Additional `opts` may be specified.  Those are usually DOT Graph
  Attributes (http://www.graphviz.org/content/attrs), e.g.,

    (print-model m \"test.pdf\" :ranksep 2.2)

  Additionally, the non-DOT :name option may be used to give a name to the
  model, which affecs the title of the generated PDF for example:

    (print-model m \"test.pdf\" :ranksep 2.2 :name \"MyModel\")

  The :name must be a valid DOT ID.

  Furthermore, an :include and an :exclude option may be given, both being seqs
  of EObjects to include/exclude from printing.  If :include is nil, everything
  is included.  :exclude overrides :include."
  [m f & opts]
  (let [ds (dot-model m opts)
        suffix (second (re-matches #".*\.([^.]+)$" f))
        ;; Fallback to pdf on unknown extensions.
        lang (get #{"dot" "xdot" "ps" "svg" "svgz" "png" "gif" "pdf" "eps" "gtk"}
                  suffix "pdf")]
    (if (= lang "gtk")
      (println "Showing model in a GTK+ window.")
      (println "Printing model to" f))
    (let [r (clojure.java.shell/sh "dot" (str "-T" lang) "-o" f :in ds)]
      (when-not (zero? (:exit r))
        (errorf "Dotting failed: %s" (:err r))))))

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
               (let [m (into {}
                             (map (fn [^EAttribute attr]
                                    [(keyword (.getName attr)) (.getEType attr)])
                                  (seq (.getEAttributes ec))))]
                 (when (seq m)
                   (str " " m)))
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
                 (qname eo)
                 ">"))))


