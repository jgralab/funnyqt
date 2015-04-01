(ns funnyqt.emf
  "Core functions for accessing and manipulating EMF models."
  (:require [clojure.core.cache    :as cache]
            [clojure.core.reducers :as r]
            [clojure.string        :as str]
            [funnyqt.generic       :as g]
            [funnyqt.internal      :as i]
            [funnyqt.utils         :as u]
            [funnyqt.query         :as q]
            [flatland.ordered.set  :as os]
            [flatland.ordered.map  :as om]
            inflections.core)
  (:import
   (org.eclipse.emf.ecore.xmi.impl XMIResourceImpl)
   (org.eclipse.emf.ecore.util EcoreUtil)
   (org.eclipse.emf.common.util URI EList UniqueEList EMap)
   (org.eclipse.emf.ecore.resource Resource ResourceSet)
   (org.eclipse.emf.ecore
    EcorePackage EPackage EPackage$Registry EObject EModelElement EClassifier
    EClass EDataType EEnumLiteral EEnum EFactory ETypedElement EAnnotation
    EAttribute EReference EStructuralFeature)))

;;# Simple type predicates

(defn eobject?
  "Returns true if `eo` is an EObject."
  {:inline (fn [x] `(instance? EObject ~x))}
  [eo]
  (instance? EObject eo))

(extend-protocol g/IElement
  EObject
  (element? [this] true))

(defn eclass?
  "Returns true if `ec` is an EClass."
  {:inline (fn [x] `(instance? EClass ~x))}
  [ec]
  (instance? EClass ec))

(defn epackage?
  "Returns true if `ep` is an EPackage."
  {:inline (fn [x] `(instance? EPackage ~x))}
  [ep]
  (instance? EPackage ep))

(defn ereference?
  "Returns true if `er` is an EReference."
  {:inline (fn [x] `(instance? EReference ~x))}
  [er]
  (instance? EReference er))

(defn eattribute?
  "Returns true if `ea` is an EAttribute."
  {:inline (fn [x] `(instance? EAttribute ~x))}
  [ea]
  (instance? EAttribute ea))

;;# Metamodel Access

(def ^:dynamic ^EPackage$Registry *epackage-registry*
  "The current EPackage Registry which is used for EClassifier registration and
  lookup.  The default value is the global registry.  Use

    (with-epackage-registry (.getPackageRegistry my-resource-set)
      ...)

  to use a ResourceSet's local registry instead."
  EPackage$Registry/INSTANCE)

(defmacro with-epackage-registry
  "Evaluate `body` with the current value of *epackage-registry* set to
  `registry`."
  [registry & body]
  `(binding [*epackage-registry* ~registry]
     ~@body))

(def ^:dynamic *ns-uris*
  "A set of namespace URIs to which the classifier lookup should be restricted.
  Also see `with-ns-uris'."
  nil)

(defmacro with-ns-uris
  "Restricts the EClassifier lookup in the dynamic scope of `body` to those
  contained in top-level EPackages registered with the given namespace `uris`
  at the EPackage registry and subpackages thereof."
  {:arglists '([[uris] & body])}
  [uris & body]
  `(binding [*ns-uris* ~uris]
     ~@body))

(defmacro ^:private with-system-class-loader [& body]
  `(let [^Thread curt# (Thread/currentThread)
         curcl# (.getContextClassLoader curt#)
         syscl# (ClassLoader/getSystemClassLoader)]
     (if (= curcl# syscl#)
       ~@body
       (do
         (.setContextClassLoader curt# syscl#)
         (try
           ~@body
           (finally (.setContextClassLoader curt# curcl#)))))))

;; Caches

(defrecord ^:private CacheEntry [ns-uris spec])
(alter-meta! #'->CacheEntry assoc :private true)
(alter-meta! #'map->CacheEntry assoc :private true)

(defmacro ^:private make-cache-entry [ns-uris nm]
  `(CacheEntry. (or ~ns-uris *ns-uris*) ~nm))

(def ^:private +eclassifier-cache+
  "A cache from EClassifier names to EClassifiers."
  (cache/soft-cache-factory (hash-map)))

(def ^:private +type-matcher-cache+
  "A cache from type-specs to type-matchers."
  (cache/soft-cache-factory (hash-map)))

(defn ^:private reset-all-emf-caches
  "Resets all EMF specific caches:

    1. the +eclassifier-cache+
    2. the +type-matcher-cache+"
  []
  (alter-var-root #'+eclassifier-cache+
                  (constantly (cache/soft-cache-factory (hash-map))))
  (alter-var-root #'+type-matcher-cache+
                  (constantly (cache/soft-cache-factory (hash-map)))))

(defn esubpackages
  "Returns all direct subpackages of EPackage `ep`."
  [^EPackage ep]
  (seq (.getESubpackages ep)))

(defn eallsubpackages
  "Returns all direct and indirect subpackages of EPackage `ep`."
  [^EPackage ep]
  (let [subs (esubpackages ep)]
    (concat subs (mapcat esubpackages subs))))

(defn epackages
  "The lazy seq of all registered EPackages and their subpackages."
  []
  (with-system-class-loader
    (map (fn [uri]
           (if-let [p (.getEPackage *epackage-registry* uri)]
             p
             (u/errorf "No such EPackage nsURI: %s" uri)))
         (or *ns-uris* (keys *epackage-registry*)))))

(defn ^:private ns-uris-and-type-spec [name]
  (if (map? name)
    (if (== (count name) 1)
      (let [[ns-uris n :as tup] (first name)]
        (if (coll? ns-uris)
          tup
          [[ns-uris] n]))
      (u/errorf "Broken type spec: %s" name))
    [nil name]))

(defn epackage
  "Returns the EPackage with the given (simple or qualified) `name`.
  In case there are several packages with the same (qualified) name, you can
  also disambiguate using {\"http://ns/uri\" pkg-name}, or by using
  `with-ns-uris`."
  [name]
  (let [[ns-uris name] (ns-uris-and-type-spec name)]
    (binding [*ns-uris* (if ns-uris ns-uris *ns-uris*)]
      (let [name (clojure.core/name name)
            ffn (if (.contains name ".")
                  (fn [^EPackage p] (= (clojure.core/name (g/qname p)) name))
                  (fn [^EPackage p] (= (.getName p) name)))
            qkgs (filter ffn (epackages))]
        (when-not (seq qkgs)
          (u/errorf "No such package %s." name))
        (when (nnext qkgs)
          (u/errorf "Multiple packages named %s: %s\n%s%s" name qkgs
                    "Restrict the search space using `with-ns-uris` "
                    "or use {\"http://ns/uri\" pkg-name}."))
        (first qkgs)))))

(extend-protocol g/IMMAbstract
  EClass
  (mm-abstract? [this]
    (.isAbstract this)))

(extend-protocol g/IUnset
  EObject
  (unset? [this attr]
    (not (.eIsSet this (.getEStructuralFeature (.eClass this) (name attr))))))

(defn eclassifiers
  "Returns the lazy seq of EClassifiers known by *epackage-registry*.
  Also see: `with-ns-uris` and `with-epackage-registry`."
  []
  (sequence (mapcat (fn [^EPackage ep]
                      (.getEClassifiers ep)))
            (epackages)))

(defn eclass
  "Returns the EClass of the given EObject `eo`."
  ^org.eclipse.emf.ecore.EObject [^EObject eo]
  (.eClass eo))

(declare eallcontents)
(defn eclasses
  "Returns the lazy seq of EClasses known by *epackage-registry*.
  Also see: `with-ns-uris` and `with-epackage-registry`"
  ([]
   (filter eclass? (eclassifiers)))
  ([ecore-resource]
   (filter eclass? (eallcontents ecore-resource))))

(defn eclassifier
  "Returns the eclassifier with the given `name`.
  `name` may be a simple, qualified name, or a map of the form {nsURI name}.
  In the latter case, the lookup is restricted to the package with the given
  nsURI (and its subpackages).
  Throws an exception if no such classifier could be found, or if the given
  simple name is ambiguous.
  Also see: `with-ns-uris` and `with-epackage-registry`"
  [name]
  (let [[ns-uris nm] (ns-uris-and-type-spec name)
        cache-entry (make-cache-entry ns-uris nm)]
    (if-let [ec (cache/lookup +eclassifier-cache+ cache-entry)]
      (do (cache/hit +eclassifier-cache+ cache-entry) ec)
      (binding [*ns-uris* (if ns-uris ns-uris *ns-uris*)]
        (let [^String n (clojure.core/name nm)
              ld (.lastIndexOf n ".")]
          (if (>= ld 0)
            (let [^EPackage ep (epackage (subs n 0 ld))]
              (or (.getEClassifier ep (subs n (inc ld)))
                  (u/errorf "No such EClassifier %s in %s." n (print-str ep))))
            (let [classifiers (filter (fn [^EClassifier ec]
                                        (= (.getName ec) n))
                                      (eclassifiers))]
              (cond
                (empty? classifiers) (u/errorf "No such EClassifier %s." n)
                (next classifiers)   (u/errorf "EClassifier %s is ambiguous: %s\n%s%s"
                                               n (print-str classifiers)
                                               "Restrict the search space using `with-ns-uris` "
                                               "or by using {\"http://ns/uri\" qname}.")
                :else (let [ec (first classifiers)]
                        (cache/miss +eclassifier-cache+ cache-entry ec)
                        ec)))))))))

(defn esuperclasses
  "Returns the direct super classes of the given EClass `ec`."
  [^EClass ec]
  (into #{} (.getESuperTypes ec)))

(defn eallsuperclasses
  "Returns the direct and indirect super classes of the given EClass `ec`."
  [^EClass ec]
  (into #{} (.getEAllSuperTypes ec)))

(defn esubclasses
  "Returns the direct sub-EClasses of the given EClass `ec`."
  [^EClass ec]
  (into #{} (filter #(contains? (esuperclasses %) ec) (eclasses))))

(defn eallsubclasses
  "Returns the direct and indirect sub-EClasses of the given EClass `ec`."
  [^EClass ec]
  (into #{} (filter #(and (not= ec %) (.isSuperTypeOf ec %)) (eclasses))))

(defn eenum-literal
  "Returns the EEnumLiteral specified by its `qname`."
  [qname]
  (let [[eenum elit] (u/split-qname qname)]
    (if-let [^EEnum enum-cls (eclassifier eenum)]
      (if-let [^EEnumLiteral lit (.getEEnumLiteral enum-cls ^String elit)]
        (or (.getInstance lit) lit)
        (u/errorf "%s has no EEnumLiteral with name %s."
                  (print-str enum-cls) elit))
      (u/errorf "No such EEnum %s." eenum))))

(extend-protocol g/IMMAllSubclasses
  EClass
  (mm-all-subclasses [this]
    (eallsubclasses this)))

(extend-protocol g/IEnumConstant
  EObject
  (enum-constant [el const]
    (eenum-literal const))
  Resource
  (enum-constant [el const]
    (eenum-literal const))
  nil
  (enum-constant [el const]
    (eenum-literal const)))

;;# Generic Metamodel Access

(extend-protocol g/IMMElementClasses
  EClass
  (mm-element-classes [cls]
    (if-let [r (.eResource cls)]
      (eallcontents r 'EClass)
      (let [top-pkg (loop [^EPackage p (.getEPackage cls)]
                      (if-let [sup (.getESuperPackage p)]
                        (recur sup)
                        p))
            subs (eallsubpackages top-pkg)
            uris (into [] (comp (map #(.getNsURI ^EPackage %))
                                (remove nil?))
                       (cons top-pkg subs))]
        (with-ns-uris uris
          (eclasses)))))
  Resource
  (mm-element-classes [res]
    (eclasses res))
  ResourceSet
  (mm-element-classes [rs]
    (eclasses rs)))

(extend-protocol g/IMMClass
  EClass
  (mm-class? [this] true)
  EObject
  (mm-class? [this] false)
  (mm-class
    ([this]
     (.eClass this))
    ([this qn]
     (eclassifier qn)))
  ResourceSet
  (mm-class? [this] false)
  (mm-class
    ([this qn]
     (eclassifier qn)))
  Resource
  (mm-class? [this] false)
  (mm-class
    ([this qn]
     (eclassifier qn))))

(extend-protocol g/IMMDirectSuperclasses
  EClass
  (mm-direct-superclasses [this]
    (seq (.getESuperTypes this))))

(extend-protocol g/IMMSuperclass
  EClass
  (mm-superclass? [this sub]
    (and (not (identical? this sub))
         (.isSuperTypeOf this sub))))

(extend-protocol g/IMMAttributes
  EClass
  (mm-attributes [cls]
    (map (fn [^EAttribute attr]
           (keyword (.getName attr)))
         (.getEAttributes cls))))

(extend-protocol g/IMMReferences
  EClass
  (mm-references [cls]
    (map (fn [^EReference ref]
           (keyword (.getName ref)))
         (.getEReferences cls))))

(extend-protocol g/IMMReferencedElementClass
  EClass
  (mm-referenced-element-class [this ref]
    (if-let [^EStructuralFeature sf (.getEStructuralFeature this (name ref))]
      (if (instance? EReference sf)
        (.getEReferenceType ^EReference sf)
        (u/errorf "%s is no EReference." sf))
      (u/errorf "No such structural feature %s at EClass %s." ref this))))

(extend-protocol g/IMMBooleanAttribute
  EClass
  (mm-boolean-attribute? [ec attr]
    (let [^EAttribute ea (.getEStructuralFeature ec (name attr))]
      (= "EBoolean" (.getName (.getEAttributeType ea))))))

(extend-protocol g/IMMMultiValuedProperty
  EClass
  (mm-multi-valued-property? [cls prop]
    (.isMany (.getEStructuralFeature cls (name prop)))))

(extend-protocol g/IMMContainmentReference
  EClass
  (mm-containment-reference? [this ref-kw]
    (if-let [^org.eclipse.emf.ecore.EReference
             er (.getEStructuralFeature this (name ref-kw))]
      (.isContainment er)
      (u/errorf "No such reference %s at metamodel class %s." ref-kw this))))

;;# Model

;;## Qualified Names

(extend-protocol g/IQualifiedName
  EClassifier
  (qname [this]
    (if-let [pkg (.getEPackage this)]
      (symbol (str (g/qname pkg)
                   "." (.getName this)))
      (symbol (.getName this))))

  EPackage
  (qname [this]
    (loop [p (.getESuperPackage this), n (.getName this)]
      (if p
        (recur (.getESuperPackage p) (str (.getName p) "." n))
        (symbol n))))

  EObject
  (qname [o]
    (g/qname (.eClass o))))

(extend-protocol g/IUniqueName
  EClassifier
  (uname [ec]
    (let [n (.getName ec)
          ecs (filter (fn [^EClass e]
                        (= n (.getName e)))
                      (eclassifiers))]
      (if (> (count ecs) 1)
        (g/qname ec)
        (symbol n))))
  EObject
  (uname [eo]
    (g/uname (.eClass eo))))

;;## EMF Resources

(defn new-resource
  "Creates and returns a new, empty Resource."
  ^org.eclipse.emf.ecore.resource.Resource
  []
  (XMIResourceImpl.))

(defn ^:private create-uri [f]
  (cond
    (instance? URI f)
    f

    (instance? java.io.File f)
    (URI/createURI (.getPath ^java.io.File f))

    (instance? java.net.URL f)
    (URI/createURI (.toString ^java.net.URL f))

    (and (string? f)
         (.exists (clojure.java.io/file f)))
    (URI/createURI f)

    (clojure.java.io/resource f)
    (URI/createURI (.toString (clojure.java.io/resource f)))

    :else (u/errorf "Cannot load %s." f)))

(defn load-resource
  "Loads an EMF resource from the XMI file `f`.
  `f` may be a file name given as string, a java.io.File, an URI, or a
  java.net.URL.  `additional-opts` may be additional many option-value pairs
  that are added to the default load options.
  Also see `load-ecore-resource`."
  [f & additional-opts]
  (let [uri (create-uri f)
        res (XMIResourceImpl. uri)
        opts (.getDefaultLoadOptions res)]
    (doseq [[opt val] (apply hash-map additional-opts)]
      (.put opts opt val))
    (.load res opts)
    res))

(defn ^:private register-epackages
  "Registeres the given packages at the EPackage$Registry by their nsURI.
  Skips packages that are already registered."
  [pkgs]
  (with-system-class-loader
    (doseq [^EPackage p pkgs]
      (when-let [uri (.getNsURI p)]
        (when (seq uri)
          (when-not (.containsKey *epackage-registry* uri)
            (.put *epackage-registry* uri p)))))))

(defn ^:private all-epackages-in-resource [^Resource r]
  (let [ps (filter #(instance? EPackage %) (.getContents r))]
    (concat ps (mapcat eallsubpackages ps))))

(defn load-ecore-resource
  "Loads an Ecore model from the ecore file `f`.
  All EPackages are registered at the *epackage-registry* which defaults to the
  global registry.  The Ecore model is returned as a Resource.  `f` may be a
  file name given as string, a java.io.File, an URI, or a java.net.URL.  Also
  see `with-epackage-registry`."
  [f]
  ;; Reset the caches, since now the names might not be unique anymore.
  (reset-all-emf-caches)
  (let [res (load-resource f)]
    (register-epackages
     (all-epackages-in-resource res))
    res))

(alter-var-root #'g/mm-load-handlers assoc #".*\.ecore$" load-ecore-resource)

;; FIXME: That's actual a workaround for a misfeature of EMF.  See
;; http://www.eclipse.org/forums/index.php/m/405881/
(defn ^:private fixup-resource [^Resource resource]
  (let [l (.getContents resource)
        ^java.util.ListIterator li (.listIterator l)]
    (while (.hasNext li)
      (let [^EObject o (.next li)]
        (when (.eContainer o)
          (.remove li))))
    resource))

(defn save-resource
  "Saves the given `resource`.  If given a file `f`, saves to it.
  `f` may be a file name given as string, a java.io.File, an URI, or a
  java.net.URL."
  ([^Resource resource]
   (if-let [uri (.getURI resource)]
     (do
       (fixup-resource  resource)
       (println "Saving resource to" (.toFileString uri))
       (.save resource nil))
     (u/error (str "You tried to call save-resource on a Resource not associated "
                   "with a file!\n"))))
  ([^Resource resource f]
   (let [uri (create-uri f)]
     (.setURI resource uri)
     (save-resource resource))))

;;## Type Checks

(defn ^:private type-matcher-emf-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [c]
  (let [v     (u/type-with-modifiers (name c))
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
    (nil? ts)     identity
    (u/qname? ts) (type-matcher-emf-1 ts)
    (vector? ts)  (if (seq ts)
                    (let [f (first ts)
                          [op r] (case f
                                   :and  [q/and-fn  (next ts)]
                                   :nand [q/nand-fn (next ts)]
                                   :or   [q/or-fn   (next ts)]
                                   :nor  [q/nor-fn  (next ts)]
                                   :xor  [q/xor-fn  (next ts)]
                                   [q/or-fn ts])
                          t-matchers (map #(type-matcher-emf %) r)]
                      (apply op t-matchers))
                    ;; Empty collection given: (), [], that's also ok
                    identity)
    (fn? ts)      ts
    ;; {"http://my.nsuri/1.0" ts}
    (map? ts)     (let [[ns-uris ts] (first ts)]
                    (binding [*ns-uris* (if ns-uris [ns-uris] *ns-uris*)]
                      (type-matcher-emf ts)))
    (eclass? ts)  (fn [e] (.isInstance ^EClass ts e))
    :else (u/errorf "Don't know how to create an EMF type-matcher for %s" ts)))

(defn ^:private type-matcher-cached [_ ts]
  (let [[ns-uris ts-1] (ns-uris-and-type-spec ts)
        cache-entry (make-cache-entry ns-uris ts-1)]
    (if-let [tm (cache/lookup +type-matcher-cache+ cache-entry)]
      (do (cache/hit +type-matcher-cache+ cache-entry) tm)
      (let [tm (type-matcher-emf ts)]
        (cache/miss +type-matcher-cache+ cache-entry tm)
        tm))))

(extend-protocol g/ITypeMatcher
  EObject
  (type-matcher [m ts]
    (type-matcher-cached m ts))
  Resource
  (type-matcher [m ts]
    (type-matcher-cached m ts))
  ResourceSet
  (type-matcher [m ts]
    (type-matcher-cached m ts)))

(extend-protocol g/IInstanceOf
  EObject
  (is-instance? [object class]
    (and (eclass? class)
         (.isInstance ^EClass class object))))

;;## Traversal Stuff

(defprotocol ^:private IEContents
             "A protocol for getting the contents of Resources, ResourceSets and EObjects."
             (^:private eallcontents-internal [this tm]
               "Returns a seq of all directly and indirectly contained EObjects whose type
  matches the type spec `ts` (see `funnyqt.generic/type-matcher`).")
             (^:private econtents-internal [this tm]
               "Returns a seq of all directly contained EObjects whose type matches the
  type spec `ts` (see `funnyqt.generic/type-matcher`)."))

(extend-protocol IEContents
  EObject
  (econtents-internal [this ts]
    (sequence (filter (g/type-matcher this ts))
              (seq (.eContents this))))
  (eallcontents-internal [this ts]
    (sequence (filter (g/type-matcher this ts))
              (iterator-seq
               (EcoreUtil/getAllProperContents this true))))

  Resource
  (econtents-internal [this ts]
    (sequence (filter (g/type-matcher this ts))
              (seq (.getContents this))))
  (eallcontents-internal [this ts]
    (sequence (filter (g/type-matcher this ts))
              (iterator-seq
               (EcoreUtil/getAllProperContents this true))))

  ResourceSet
  (eallcontents-internal [this ts]
    (sequence (comp (filter eobject?)
                    (filter (g/type-matcher this ts)))
              (iterator-seq
               (EcoreUtil/getAllProperContents this true))))

  clojure.lang.IPersistentCollection
  (econtents-internal [this tm]
    (sequence (mapcat #(econtents-internal % tm))
              this))
  (eallcontents-internal [this tm]
    (sequence (mapcat #(eallcontents-internal % tm))
              this)))

(defn eallcontents
  "Returns a lazy seq of `container`s direct and indirect contents
  matching the type spec `ts`.  `container` may be an EObject, a
  Collection, a Resource, or a ResourceSet."
  ([container]
   (eallcontents-internal container identity))
  ([container ts]
   (eallcontents-internal container ts)))

(defn econtents
  "Returns a lazy seq of `containers`s direct contents matching the type spec `ts`.
  `container` may be an EObject, a Collection, a Resource, or a
  ResourceSet."
  ([container]
   (econtents-internal container identity))
  ([container ts]
   (econtents-internal container ts)))

(extend-protocol g/IElements
  Resource
  (elements
    ([this]
     (eallcontents this))
    ([this ts]
     (eallcontents this ts)))

  ResourceSet
  (elements
    ([this]
     (eallcontents this))
    ([this ts]
     (eallcontents this ts))))

(defn eresource
  "Returns the Resource containing `eo` directly or indirectly (if any)."
  [^EObject eo]
  (.eResource eo))

(defn ^:private eref-matcher
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
    (u/prop-name? rs)  (let [n (name rs)]
                         (fn [^EReference ref]
                           (= n (.getName ref))))
    (ereference? rs) (fn [r] (= rs r))
    (coll? rs)       (if (seq rs)
                       (apply some-fn (map eref-matcher rs))
                       ;; Empty collection given: (), [], that's also ok
                       identity)
    :else (u/errorf "Don't know how to create a reference matcher for %s" rs)))

(defn ^:private get-eref ^EReference [^EClass ec n pred]
  (if-let [^EStructuralFeature sf (.getEStructuralFeature ec (name n))]
    (do
      (when-not (instance? EReference sf)
        (u/errorf "%s is no EReference." sf))
      (if pred
        (when (pred sf)
          sf)
        sf))
    (u/errorf "No such EReference %s." n)))

(defn econtainer
  "Returns the EObject containing `eo` (if any).
  If a reference spec `rs` is given, return only `eo`s container if it's
  referenced by a containment reference matching `rs`."
  ([^EObject eo]
   (.eContainer eo))
  ([^EObject eo rs]
   (if rs
     (if (keyword? rs)
       (when-let [ref (get-eref (.eClass eo) rs (fn [^EReference ref]
                                                  (.isContainer ref)))]
         (.eGet eo ref))
       (let [rm (eref-matcher rs)]
         (first (eduction (comp (filter (and (.isContainer ^EReference ref)
                                             (rm ref)))
                                (map (fn [^EReference r] (.eGet eo r))))
                          (seq (.getEAllReferences (.eClass eo)))))))
     ;; We cannot handle the rs = nil case with the above because it is
     ;; possible that there is no reference at all at the container side.
     (.eContainer eo))))

(extend-protocol g/IContainer
  EObject
  (container
    ([this]
     (econtainer this))
    ([this rs]
     (econtainer this rs))))

(defn ^:private eopposite-refs
  "Returns the seq of `eo`s EClass' references whose opposites match `src-rm`.

  Example:

           [Foo] f --- b [Bar]
              f \\
                 `---- c [Car]

  Given a Foo object and a eref-matcher matching f, returns a seq of the
  EReferences b and c, because those are the opposites of the matched f.  Of
  course, if `src-rm` matches only one specific EReference, i.e., it was
  constructed by (eref-matcher fERef) and not (eref-matcher :f), then only b
  or c (exclusive) is returned.."
  [^EObject eo src-rm]
  (seq (eduction (comp (map (fn [^EReference r]
                              (when-let [o (.getEOpposite r)]
                                (when (src-rm o) r))))
                       (remove nil?))
                 (seq (-> eo .eClass .getEAllReferences)))))

(defn ^:private search-ereferencers
  "Returns the seq of objects referencing `refed` by a reference matching `rm`
  that are contained in `container`.  `reffn` is either erefs or ecrossrefs."
  [refed reffn rm container]
  (filter (fn [o] (q/member? refed (reffn o rm)))
          (if (coll? container) container (eallcontents container))))

(defn erefs
  "Returns a set of EObjects referenced by EObject `eo`, possibly restricted by
  the reference spec `rs`.  For the syntax and semantics of `rs`, see
  `eref-matcher`.  In contrast to `ecrossrefs`, this function doesn't ignore
  containment refs."
  ([eo]
   (erefs eo nil))
  ([^EObject eo rs]
   (if (keyword? rs)
     (when-let [ref (get-eref (.eClass eo) rs nil)]
       (when-let [x (.eGet eo ref)]
         (if (.isMany ref) x #{x})))
     (let [rm (eref-matcher rs)]
       (into []
             (comp (filter rm)
                   (mapcat (fn [^EReference r]
                             (when-let [x (.eGet eo r)]
                               (if (.isMany r) x [x])))))
             (seq (-> eo .eClass .getEAllReferences)))))))

(defn ecrossrefs
  "Returns a collection of EObjects cross-referenced by EObject`eo`, possibly
  restricted by the reference spec `rs`.  For the syntax and semantics of `rs`,
  see `eref-matcher`.  In EMF, crossrefs are all non-containment refs."
  ([eo]
   (ecrossrefs eo nil))
  ([^EObject eo rs]
   (if (keyword? rs)
     (when-let [ref (get-eref (.eClass eo) rs (fn [^EReference ref]
                                                (and (not (.isContainment ref))
                                                     (not (.isContainer ref)))))]
       (when-let [x (.eGet eo ref)]
         (if (.isMany ref) x #{x})))
     (let [rm (eref-matcher rs)]
       (into []
             (comp (filter (fn [^EReference r]
                             (and (not (.isContainment r))
                                  (not (.isContainer r))
                                  (rm r))))
                   (mapcat (fn [^EReference r]
                             (when-let [x (.eGet eo r)]
                               (if (.isMany r) x [x])))))
             (seq (-> eo .eClass .getEAllReferences)))))))

(defn econtentrefs
  "Returns a collection of EObjects contained by EObject`eo`, possibly
  restricted by the reference spec `rs`.  For the syntax and semantics of `rs`,
  see `eref-matcher`."
  ([eo]
   (econtentrefs eo nil))
  ([^EObject eo rs]
   (if (keyword? rs)
     (when-let [ref (get-eref (.eClass eo) rs (fn [^EReference ref]
                                                (.isContainment ref)))]
       (when-let [x (.eGet eo ref)]
         (if (.isMany ref) x #{x})))
     (let [rm (eref-matcher rs)]
       (into []
             (comp (filter (fn [^EReference r]
                             (and (.isContainment r)
                                  (rm r))))
                   (mapcat (fn [^EReference r]
                             (when-let [x (.eGet eo r)]
                               (if (.isMany r) x [x])))))
             (seq (-> eo .eClass .getEAllReferences)))))))

(extend-protocol g/IContents
  EObject
  (contents
    ([this]
     (econtents this))
    ([this ts-or-role]
     (if (symbol? ts-or-role)
       (econtents this ts-or-role)
       (econtentrefs this ts-or-role)))))

(defn inv-erefs
  "Returns the seq of EOjects that reference EObject `eo` with an EReference
  matching `rs` (see `eref-matcher`).  If no `container` is given, then only
  check the opposite refs of `eo`.  Else, all objects in `container` are tested
  if they reference `eo`.  `container` may be either an EObject, a Resource,
  a ResourceSet, or a collection of EObjects.  For the former three, direct and
  indirect contents are checked, for collections only direct contents."
  ([eo]
   (inv-erefs eo nil nil))
  ([eo rs]
   (inv-erefs eo rs nil))
  ([eo rs container]
   (let [rm (eref-matcher rs)]
     (if container
       (search-ereferencers eo erefs rm container)
       (if-let [opposites (eopposite-refs eo rm)]
         (erefs eo (eref-matcher opposites))
         (u/error "No opposite EReferences found."))))))

(defn inv-ecrossrefs
  "Returns the seq of EOjects that cross-reference EObject `eo` with an
  EReference matching `rs` (see `eref-matcher`).  If no `container` is given,
  then only check the opposite refs of `eo`.  Else, all objects in `container`
  are tested if they cross-reference `eo`. `container` may be either an
  EObject, a Resource, a ResourceSet, or a collection of EObjects.  For the
  former three, direct and indirect contents are checked, for collections only
  direct contents."
  ([eo]
   (inv-ecrossrefs eo nil nil))
  ([eo rs]
   (inv-ecrossrefs eo rs nil))
  ([eo rs container]
   (let [rm (eref-matcher rs)]
     (if container
       (search-ereferencers eo ecrossrefs rm container)
       (if-let [opposites (eopposite-refs eo rm)]
         (ecrossrefs eo (eref-matcher opposites))
         (u/error "No opposite EReferences found."))))))

(defprotocol ^:private IEMFValues2ClojureValues
             (^:private emf2clj-internal [this]
               "Converts an EMF thingy to a clojure thingy.

  EMF Type     | Clojure Type
  -------------+-------------
  UniqueEList  | ordered-set
  EMap         | ordered-map
  EList        | vector

  All other objects are kept as-is."))

(extend-protocol IEMFValues2ClojureValues
  UniqueEList
  (emf2clj-internal [this] (into (os/ordered-set) (seq this)))
  EMap
  (emf2clj-internal [this] (into (om/ordered-map) (seq this)))
  EList
  (emf2clj-internal [this] (into (vector) this))
  Object
  (emf2clj-internal [this] this)
  nil
  (emf2clj-internal [_] nil))

(defn ^:private emf2clj
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
    (u/errorf "No such structural feature %s for %s." sf (print-str eo))))

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
      (do
        (cond
          (and (instance? Long value)
               (-> sfeat
                   .getEType
                   .getName
                   (= "EInt")))
          (.eSet eo sfeat (int value))

          (ratio? value)
          (.eSet eo sfeat (double value))

          :else (.eSet eo sfeat value))
        eo))
    (u/errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eunset!
  "Unsets `eo`s structural feature `sf` and returns `eo`.
  Throws an exception, if there's no EStructuralFeature `sf`."
  [^EObject eo sf]
  (if-let [sfeat (if (instance? EStructuralFeature sf)
                   sf
                   (.getEStructuralFeature (.eClass eo) (name sf)))]
    (doto eo
      (.eUnset sfeat))
    (u/errorf "No such structural feature %s for %s." sf (print-str eo))))

(defn eadd!
  "Adds `value` and `more` values to `eo`s list of attribute/reference values
  denoted by EStructuralFeature `sf`.  Returns `eo` again.  Throws an exception
  if there's no EStructuralFeature `sf`.

  In the arity-2 version, adds `obj` to `resource` and returns `resource`."
  ([eo sf value & more]
   (let [^EList l (eget-raw eo sf)]
     (.add l value)
     (when (seq more)
       (.addAll l more))
     eo))
  ([^Resource resource obj]
   (.add (.getContents resource) obj)
   resource))

(defn eaddall!
  "Adds all values in `coll` to `eo`s `sf` structural feature.
  In the arity 2 variant, adds all EObjects in `coll` to `resource` and
  returns `resource`."
  ([eo sf coll]
   (let [^EList l (eget-raw eo sf)]
     (.addAll l coll)
     eo))
  ([^Resource resource coll]
   (.addAll (.getContents resource) coll)
   resource))

(defn eremove!
  "Removes `value` and `more` values from `eo`s list of attribute/reference
  values denoted by `sf` and returns `eo`.  Throws an exception, if there's no
  EStructuralFeature `sf`.

  In the arity-2 version, removes `obj` from `resource` and returns `resource`.
  Note that it won't delete `obj` or remove references to it.

  Returns `eo` or `resource` again."
  ([eo sf value & more]
   (let [^EList l (eget-raw eo sf)]
     (.remove l value)
     (when (seq more)
       (.removeAll l more))
     eo))
  ([^Resource resource eo]
   (.remove (.getContents resource) eo)
   resource))

(defn eremoveall!
  "Removes all objects in `coll` from `eo`s list of attribute/reference values
  denoted by `sf` and returns `eo`.  Throws an exception, if there's no
  EStructuralFeature `sf`.

  In the arity-2 version, removes all objects in `coll` from `resource`
  and returns `resource`.  Note that it won't delete the objects or
  remove references to it.

  Returns `eo` or `resource` again."
  ([eo sf coll]
   (let [^EList l (eget-raw eo sf)]
     (.removeAll l coll)
     eo))
  ([^Resource resource coll]
   (.removeAll (.getContents resource) coll)
   resource))

;;### Generic attribute access

(extend-protocol g/IAttributeValueAccess
  EObject
  (aval [this attr]
    (let [^EStructuralFeature sf (.getEStructuralFeature (.eClass this) (name attr))]
      (if (eattribute? sf)
        (emf2clj-internal (.eGet this sf))
        (if (nil? sf)
          (u/errorf "No such attribute %s at object %s." attr this)
          (u/errorf "%s is no attribute of object %s." sf this)))))
  (set-aval! [this attr val]
    (let [^EStructuralFeature sf (.getEStructuralFeature (.eClass this) (name attr))]
      (cond
        (nil? sf)
        (u/errorf "No such attribute %s at object %s." attr this)

        (ereference? sf)
        (u/errorf "%s is no attribute of object %s but a reference." sf this)

        :else (eset! this attr val)))))


;;## Edges, i.e., src/trg tuples

(defn ^:private epairs-internal [this reffn src-rs trg-rs src-ts trg-ts]
  (let [done (atom #{})
        src-rm (eref-matcher src-rs)
        trg-rm (eref-matcher trg-rs)]
    (for [^EObject src (eallcontents this src-ts)
          ^EReference ref (seq (-> src .eClass .getEAllReferences))
          :when (not (q/member? ref @done))
          :when (trg-rm ref)
          :let [nthere-rm (eref-matcher ref)
                oref (.getEOpposite ref)]
          :when (if oref
                  (src-rm oref)
                  ;; no oref, but if it was given, there must be one so this
                  ;; ref is not meant!
                  (not src-rs))
          trg (reffn src nthere-rm)
          :when (or (nil? trg-ts) (g/has-type? trg trg-ts))]
      (do
        (when oref (swap! done conj oref))
        [src trg]))))

(defn epairs
  "Returns the lazy seq of all edges in terms of [src trg] pairs in `r`.
  This includes both containment as well as crossreferences.  Restrictions may
  be defined in terms of reference specs `src-rs` and `trg-rs`, and reference
  specs plus type specs `src-ts` and `trg-ts`.  `r` may be a Resource or
  ResourceSet."
  ([r]
   (epairs-internal r erefs nil nil nil nil))
  ([r src-rs trg-rs]
   (epairs-internal r erefs src-rs trg-rs nil nil))
  ([r src-rs trg-rs src-ts trg-ts]
   (epairs-internal r erefs src-rs trg-rs src-ts trg-ts)))

(defn ecrosspairs
  "Returns the lazy seq of all cross-reference edges in `r` in terms of [src trg] pairs.
  `r` may be a Resource or ResourceSet.  Restrictions may be defined in terms
  of reference specs `src-rs` and `trg-rs`, and reference specs plus type specs
  `src-ts` and `trg-ts`."
  ([r]
   (epairs-internal r ecrossrefs nil nil nil nil))
  ([r src-rs trg-rs]
   (epairs-internal r ecrossrefs src-rs trg-rs nil nil))
  ([r src-rs trg-rs src-ts trg-ts]
   (epairs-internal r ecrossrefs src-rs trg-rs src-ts trg-ts)))

(defn econtentpairs
  "Returns the lazy seq of all containment edges in `r` in terms of [src trg] pairs.
  `r` may be a Resource or ResourceSet.  src is the parent, trg is the child.
  Restrictions may be defined in terms of reference specs `src-rs` and
  `trg-rs`, and reference specs plus type specs `src-ts` and `trg-ts`."
  ([r]
   (epairs-internal r econtentrefs nil nil nil nil))
  ([r src-rs trg-rs]
   (epairs-internal r econtentrefs src-rs trg-rs nil nil))
  ([r src-rs trg-rs src-ts trg-ts]
   (epairs-internal r econtentrefs src-rs trg-rs src-ts trg-ts)))


;;## EObject Creation

(defn ecreate!
  "Creates an EObject of EClass `ec` and adds it to `resource` which may be nil.
  `ec` may be either an EClass or just an EClass name given as symbol.
  `prop-map` is an optional map of property names given as keywords to values
  to be set.  Since props are set using `eset!`, the value of a multi-valued
  reference must be a collection of EObjects."
  ([resource ec]
   (let [eo (EcoreUtil/create (if (eclass? ec) ec (eclassifier ec)))]
     (when resource
       (eadd! resource eo))
     eo))
  ([resource ec prop-map]
   (let [eo (ecreate! resource ec)]
     (doseq [[prop val] prop-map]
       (eset! eo prop val))
     eo)))

(extend-protocol g/ICreateElement
  Resource
  (create-element!
    ([model cls]
     (ecreate! model cls))
    ([model cls prop-map]
     (ecreate! model cls prop-map))))

;;## Generic setting of props

(extend-protocol g/IModifyAdjacencies
  EObject
  (set-adjs! [o role os]
    (eset! o role os))
  (set-adj! [o1 role o2]
    (eset! o1 role o2))
  (add-adjs! [o role os]
    (apply eadd! o role (first os) (rest os)))
  (add-adj! [o1 role o2]
    (eadd! o1 (name role) o2))
  (remove-adj! [o1 role o2]
    (eremove! o1 role o2))
  (remove-adjs! [o role os]
    (eremoveall! o role os)))

;;## EObject Deletion

(defn edelete!
  "Unsets all references of `eo` and removes it from its containing resource
  and containing EObject, and returns `eo`.  If `recursively` is true (the
  default), first edelete! all contents of `eo`.  If `unset-uni-crossrefs` is
  true (default is false), then also unset all unidirectional crossreferences
  pointing to `eo` from other objects contained in the same root object,
  resource, or resource set as `eo`."
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
     (.remove (.getContents res) eo))
   eo)
  ([^EObject eo recursively unset-uni-crossrefs]
   (if unset-uni-crossrefs
     (EcoreUtil/delete eo (boolean recursively))
     (edelete! eo recursively))
   eo))

(extend-protocol g/IDelete
  EObject
  (delete!
    ([this]
     (edelete! this true true))
    ([this recursive]
     (edelete! this recursive true))))

;;# Adjacencies

(defn ^:private ensure-coll [x]
  (if (or (nil? x)
          (instance? java.util.Collection x))
    x
    [x]))

(extend-protocol i/IAdjacenciesInternal
  EObject
  (adjs-internal [this ref allow-unknown-ref single-valued]
    (if-let [^EStructuralFeature sf (.getEStructuralFeature (.eClass this) (name ref))]
      (if (ereference? sf)
        (if single-valued
          (if (.isMany sf)
            (u/errorf "Must not call adj on many-valued EReference '%s'." sf)
            (ensure-coll (.eGet this sf)))
          (ensure-coll (.eGet this sf)))
        (u/errorf "'%s' at %s is no EReference." sf this))
      (when-not allow-unknown-ref
        (u/errorf "No such structural feature '%s' at %s." ref this)))))

;;# Neighbors

(extend-protocol g/INeighbors
  EObject
  (neighbors [elem]
    (erefs elem)))

;;# Generic Regular Path Expressions

(extend-protocol q/ISimpleRegularPathExpression
  EObject
  (-->
    ([n]         (u/oset (erefs n nil)))
    ([n rs]      (u/oset (erefs n rs))))
  (--->
    ([n]         (u/oset (ecrossrefs n nil)))
    ([n rs]      (u/oset (ecrossrefs n rs))))
  (<--
    ([n]         (u/oset (inv-erefs n nil nil)))
    ([n rs]      (u/oset (inv-erefs n rs  nil))))
  (<---
    ([n]         (u/oset (inv-ecrossrefs n nil nil)))
    ([n rs]      (u/oset (inv-ecrossrefs n rs  nil))))
  (<->
    ([n]         (u/into-oset (erefs n nil) (inv-erefs n nil nil)))
    ([n rs]      (u/into-oset (erefs n rs)  (inv-erefs n rs  nil))))
  (<-->
    ([n]         (u/into-oset (ecrossrefs n nil) (inv-ecrossrefs n nil nil)))
    ([n rs]      (u/into-oset (ecrossrefs n rs)  (inv-ecrossrefs n rs  nil))))
  (<>--
    ([n]         (u/oset (econtentrefs n nil)))
    ([n rs]      (u/oset (econtentrefs n rs))))
  (--<>
    ([n]         (u/oset (econtainer n)))
    ([n rs]      (u/oset (econtainer n rs)))))

;;# Copying a resource

(extend-protocol g/ICopyModel
  Resource
  (copy-model [r]
    (fixup-resource r)
    (let [r-new (new-resource)
          ^EList l-new (.getContents r-new)
          ^EList l (.getContents r)]
      (.addAll l-new (EcoreUtil/copyAll l))
      r-new)))

;;# Resource equality

(defn ^:private non-derived-attrs [^EClass ec]
  (filter (fn [^EStructuralFeature sf]
            (and (instance? EAttribute sf)
                 (not (.isDerived sf))))
          (.getEStructuralFeatures ec)))

(defn ^:private non-derived-refs [^EClass ec]
  (filter (fn [^EStructuralFeature sf]
            (and (instance? EReference sf)
                 (not (.isDerived sf))))
          (.getEStructuralFeatures ec)))

(defn ^:private type-and-attrs= [^EObject o1 ^EObject o2]
  (let [ec1 (.eClass o1)
        ec2 (.eClass o2)]
    (and (= ec1 ec2)
         (q/forall? #(= (.eGet o1 %) (.eGet o2 %))
                    (non-derived-attrs ec1)))))

(defn ^:private eget-1 [^EObject eo ^EReference r]
  (when-let [x (.eGet eo r)]
    (if (instance? java.util.Collection x) x [x])))

(declare matches?)
(defn ^:private refs=-without-link-order [^EObject o1 ^EObject o2]
  ;; We always check attrs first so the type is guaranteed to be equal already.
  (q/forall? (fn [ref]
               (let [ros1 (eget-1 o1 ref)
                     ros2 (eget-1 o2 ref)]
                 (and (= (count ros1) (count ros2))
                      (q/forall? (fn [ro1]
                                   (q/exists? #(matches? false ro1 %) ros2))
                                 ros1))))
             (non-derived-refs (.eClass o1))))

(defn ^:private refs=-with-link-order [^EObject o1 ^EObject o2]
  ;; We always check attrs first so the type is guaranteed to be equal already.
  (q/forall? (fn [ref]
               (let [ros1 (eget-1 o1 ref)
                     ros2 (eget-1 o2 ref)]
                 (and (= (count ros1) (count ros2))
                      (every? identity (map #(matches? true %1 %2) ros1 ros2)))))
             (non-derived-refs (.eClass o1))))

(def ^:private ^:dynamic *dynamic-matching*)
(def ^:private ^:dynamic *matching*)

(defn ^:private matches? [link-order o1 o2]
  (let [m-o1 (@*matching* o1)]
    (cond
      (identical? m-o1 o2) true
      ;;---
      (and (nil? m-o1)
           (nil? (@*matching* o2)))
      (let [dm-o1 (*dynamic-matching* o1)]
        (cond
          (identical? dm-o1 o2) true
          ;;---
          (and (nil? dm-o1)
               (nil? (*dynamic-matching* o2)))
          (when (type-and-attrs= o1 o2)
            (binding [*dynamic-matching*
                      (assoc *dynamic-matching*
                             o1 o2 o2 o1)]
              (when ((if link-order
                       refs=-with-link-order
                       refs=-without-link-order)
                     o1 o2)
                (vswap! *matching* assoc o1 o2 o2 o1))))
          ;;---
          :else false))
      ;;---
      :else false)))

(extend-protocol g/IEqualModels
  Resource
  (equal-models?
    ([r1 r2]
     (g/equal-models? r1 r2 false))
    ([r1 r2 link-order]
     (when r2
       (fixup-resource r1)
       (fixup-resource r2)
       (or (identical? r1 r2)
           (and (= (count (econtents r1))
                   (count (econtents r2)))
                (= (count (eallcontents r1))
                   (count (eallcontents r2)))
                (binding [*dynamic-matching* {}
                          *matching* (volatile! {})]
                  (if (q/forall? (fn [^EObject o1]
                                   (let [ec1 (.eClass o1)]
                                     (q/exists? #(matches? link-order o1 %)
                                                (econtents r2
                                                           (fn [^EObject o2]
                                                             (= ec1 (.eClass o2)))))))
                                 (econtents r1))
                    @*matching*
                    false))))))))

;;# Describing EObjects and EClasses

(extend-protocol g/IDescribe
  EClass
  (describe [this]
    {:name (g/qname this)
     :abstract (.isAbstract this)
     :interface (.isInterface this)
     :superclasses (seq (.getESuperTypes this))
     :attributes (into {}
                       (map (fn [^EAttribute attr]
                              [(keyword (.getName attr)) (.getEType attr)]))
                       (seq (.getEAttributes this)))
     :references (into {}
                       (map (fn [^EReference ref]
                              [(keyword (.getName ref)) (.getEReferenceType ref)]))
                       (seq (.getEReferences this)))})
  EObject
  (describe [this]
    {:eclass (g/qname this)
     :container (econtainer this)
     :attr-slots (into {}
                       (map (fn [^EAttribute attr]
                              (let [kw (keyword (.getName attr))]
                                [kw (eget this kw)])))
                       (seq (.getEAllAttributes (.eClass this))))
     :ref-slots (into {}
                      (map (fn [^EReference ref]
                             (let [kw (keyword (.getName ref))]
                               [kw (eget this kw)])))
                      (seq (.getEAllReferences (.eClass this))))}))

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
               (g/qname ec)
               (feature-str
                [[(.isAbstract ec)  :abstract]
                 [(.isInterface ec) :interface]])
               ">")))

(defmethod print-method EEnum
  [^EEnum en ^java.io.Writer out]
  (.write out
          (str "#<EEnum " (g/qname en)
               #_(feature-str
                  [[(.isSerializable en) :serializable]])
               ">")))

(defmethod print-method EDataType
  [^EDataType edt ^java.io.Writer out]
  (.write out
          (str "#<EDataType " (g/qname edt)
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
               (g/qname ep)
               (let [m (into {}
                             (map (fn [[v k]]
                                    (when v
                                      [k v])))
                             [[(.getNsPrefix ep) :nsPrefix]
                              [(.getNsURI ep)    :nsURI]])]
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

;;* Ecore Model specific functional API

(defn ^:private create-eclass-fns [^EClass ec prefix]
  `(do
     ~(when-not (g/mm-abstract? ec)
        `(defn ~(symbol (str prefix "create-" (g/escaped-uname-str ec) "!"))
           ~(format "Creates a new %s object and adds it to resource `r`.
  Properties are set according to `prop-map`.
  `r` may be nil.
  Shorthand for (ecreate! r '%s prop-map)."
                    (g/qname ec)
                    (g/qname ec))
           ([~'r]
            (ecreate! ~'r '~(g/qname ec)))
           ([~'r ~'prop-map]
            (ecreate! ~'r '~(g/qname ec) ~'prop-map))))

     (defn ~(symbol (let [n (g/escaped-uname-str ec)]
                      (str prefix "eall-" (inflections.core/plural n))))
       ~(format "Returns the sequence of %s objects in `r`.
  Shorthand for (eallcontents r '%s)."
                (g/qname ec)
                (g/qname ec))
       [~'r]
       (eallcontents ~'r '~(g/qname ec)))

     ;; TYPE PRED
     (defn ~(symbol (str prefix "isa-" (g/escaped-uname-str ec) "?"))
       ~(format "Returns true if `eo` is a %s-EObject."
                (g/qname ec))
       [~'eo]
       (g/has-type? ~'eo '~(g/qname ec)))))

(defn ^:private create-eattribute-fns [attr owners prefix]
  (let [bool? (group-by (fn [^EClass ec]
                          (let [^EAttribute ea (.getEStructuralFeature ec (name attr))]
                            (= "EBoolean" (-> ea
                                              .getEAttributeType
                                              .getName))))
                        owners)]
    `(do
       ~@(when (bool? true)
           `[(defn ~(symbol (str prefix (name attr) "?"))
               ~(format "Checks if `eo`s is %s.
  Possible types for `eo`: %s"
                        (name attr)
                        (str/join ", " (apply sorted-set (map g/qname (bool? true)))))
               [~'eo]
               (eget ~'eo ~attr))])
       ~@(when (bool? false)
           `[(defn ~(symbol (str prefix (name attr)))
               ~(format "Returns the value of `eo`s %s attribute.
  Possible types for `eo`: %s"
                        (name attr)
                        (str/join ", " (apply sorted-set (map g/qname (bool? false)))))
               [~'eo]
               (eget ~'eo ~attr))])
       (defn ~(symbol (str prefix "set-" (name attr) "!"))
         ~(format "Sets the value of `eo`s %s attribute to `val`.
  Possible types for `eo`: %s"
                  (name attr)
                  (str/join ", " (apply sorted-set (map g/qname owners))))
         [~'eo ~'val]
         (eset! ~'eo ~attr ~'val)))))

(defn ^:private create-ereference-fns [ref owners prefix]
  (let [multi? (group-by (fn [^EClass ec]
                           (g/mm-multi-valued-property? ec ref))
                         owners)
        owner-string (str/join ", " (apply sorted-set (map g/qname owners)))]
    `(do
       ;; GETTER
       (defn ~(symbol (str prefix "->" (name ref)))
         ~(format "Returns the %s in `eo`s %s reference.
  Possible types for `eo`: %s"
                  (cond
                    ;; This ref is always multi-valued
                    (and (multi? true) (not (multi? false))) "objects"
                    ;; This ref is always single-valued
                    (and (not (multi? true)) (multi? false)) "object"
                    :else "object[s]")
                  (name ref)
                  owner-string)
         [~'eo]
         (eget ~'eo ~ref))

       ;; SETTER
       (defn ~(symbol (str prefix "->set-" (name ref) "!"))
         ~(format "Sets `eo`s %s reference to `refed`.
  `refed` must be a %s.
  Possible types for `eo`: %s"
                  (name ref)
                  (cond
                    ;; This ref is always multi-valued
                    (and (multi? true) (not (multi? false))) "collection of objects"
                    ;; This ref is always single-valued
                    (and (not (multi? true)) (multi? false)) "single object"
                    :else "single object or coll of objects, depending on `eo`s type")
                  owner-string)
         [~'eo ~'refed]
         (eset! ~'eo ~ref ~'refed))

       ;; ADDER
       ~@(when (multi? true)
           `[(defn ~(symbol (str prefix "->add-" (name ref) "!"))
               ~(format "Adds `eobj` and `more` eobjects to `eo`s %s reference.
  Possible types for `eo`: %s"
                        (name ref)
                        owner-string)
               [~'eo ~'eobj ~'& ~'more]
               (apply eadd! ~'eo ~ref ~'eobj ~'more))
             (defn ~(symbol (str prefix "->addall-" (name ref) "!"))
               ~(format "Adds all `eobjs` to `eo`s %s reference.
  Possible types for `eo`: %s"
                        (name ref)
                        owner-string)
               [~'eo ~'eobjs]
               (eaddall! ~'eo ~ref ~'eobjs))]))))

(defmacro generate-ecore-model-functions
  "Generates a Ecore-model-specific API consisting of functions for creating
  EObjects and functions for accessing properties (attributes and references).

  `ecore-file` is the ecore file containing the metamodel for which to generate
  the API.

  `nssym` is a symbol denoting the new namespace in which to generate.  It may
  be nil in which case the current namespace is used.

  `alias` is an alias under which the newly generated namespace will be
  required.

  `prefix` is an optional prefix all generated functions should use.  (That's
  mostly useful when generating in an existing namespace to prevent name
  clashes.)

  The following functions are generated.

  For any EClass Foo in the metamodel, a (create-Foo! resource)
  function, a (eall-Foos resource) function, and a (isa-Foo? eo) type
  check predicate is generated.

  For any EAttribute name attr, the following functions are generated:

    (attr eo) ;; Returns the attr value of eo
    (set-attr! eo val) ;; Sets the attr value of eo to val

  For boolean attributes, the getter is named attr?.

  For any EReference name ref, the following functions are generated:

    (->ref eo) ;; Returns the object/objects in eo's ref role
    (->set-ref! eo refed) ;; Sets eo's ref reference to refed
    (->add-ref! eo r1 r2 r3...) ;; Adds r1, r2, and r3 to eo's ref reference
    (->addall-ref! eo rs) ;; Adds all objects in rs to eo's ref reference

 The add-* functions are only generated if ref occurs as a multi-valued
 reference.  If eo's ref-reference is multi-valued, then the setter wants a
 collection of eobjects, else a single eobject."
  ([ecore-file]
   `(generate-ecore-model-functions ~ecore-file nil nil nil))
  ([ecore-file nssym]
   `(generate-ecore-model-functions ~ecore-file ~nssym nil nil))
  ([ecore-file nssym alias]
   `(generate-ecore-model-functions ~ecore-file ~nssym ~alias nil))
  ([ecore-file nssym alias prefix]
   `(g/metamodel-api-generator ~ecore-file
                               ~nssym
                               ~alias
                               ~prefix
                               create-eclass-fns
                               nil
                               create-eattribute-fns
                               create-ereference-fns)))
