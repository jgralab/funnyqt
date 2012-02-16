(ns funnyqt.emf.core
  "Core functions for accessing and manipulating EMF models."
  (:use funnyqt.utils)
  (:use funnyqt.generic)
  (:use ordered.set)
  (:use ordered.map)
  (:import
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.common.util URI EList UniqueEList EMap]
   [org.eclipse.emf.ecore EcorePackage EPackage EObject EModelElement EClassifier EClass
    EDataType EEnumLiteral EEnum EFactory ETypedElement EAnnotation EAttribute EReference
    EStructuralFeature]))

(add-long-doc! "TODO")

;;* Code

;;** Metamodel

(def ^org.eclipse.emf.ecore.EPackage$Registry
  epackage-registry org.eclipse.emf.ecore.EPackage$Registry/INSTANCE)

(defn- register-epackages
  "Registeres the given packages at the EPackage$Registry by their nsURI.
  Skips already registered packages."
  [pkgs]
  (doseq [^EPackage p pkgs]
    (when-let [uri (.getNsURI p)]
      ;; Empty URI or already registered -> skip it
      (when (and (seq uri)
                 (nil? (.get epackage-registry uri)))
        (.put epackage-registry (.getNsURI p) p)))
    (let [subs (.getESubpackages p)]
      (when (seq subs)
        (register-epackages subs)))))

(defn load-metamodel
  "Loads the Ecore metamodel from the ecore file `f'.
  Returns as seq of (usually one) root EPackages.
  All EPackages are registered recursively."
  [f]
  (let [uri (URI/createURI f)
        res (XMIResourceImpl. uri)]
    (doto res
      (.load (.getDefaultLoadOptions res)))
    (let [pkgs (seq (.getContents res))]
      (register-epackages pkgs)
      pkgs)))

(def ^:dynamic *ns-uris* nil)
(defmacro with-ns-uris [uris & body]
  `(binding [*ns-uris* ~uris]
     ~@body))

(defn epackages
  "The lazy seq (pkg subpkg...).
  If no package is given, the lazy seq of all registered packages is returned."
  ([]
     (mapcat #(epackages (.getEPackage epackage-registry %))
             (or *ns-uris* (keys epackage-registry))))
  ([^EPackage pkg]
     (when pkg
       (cons pkg (map epackages (.getESubpackages pkg))))))

(defn eclassifiers
  "The lazy seq of EClassifiers."
  []
  (mapcat (fn [^EPackage ep]
            (.getEClassifiers ep))
          (epackages)))

(defn eclasses
  "The lazy seq of EClasses."
  []
  (filter #(instance? EClass %) (eclassifiers)))

(defn eclassifier
  "Returns the eclassifier with the given `name'.
  Throws an exception if no such classifier could be found."
  [name]
  (let [n (clojure.core/name name)]
    (or (some (fn [^EClassifier ec]
                (when (= (.getName ec) n)
                  ec))
              (eclassifiers))
        (error (format "No such EClassifier %s." n)))))

;;** Model

(defn load-model
  "Loads an EMF model from the XMI file `f'.
  Returns a seq of the models top-level elements."
  [f]
  (let [uri (URI/createURI f)
        res (XMIResourceImpl. uri)]
    (doto res
      (.load (.getDefaultLoadOptions res)))
    (seq (.getContents res))))

(defn- type-matcher-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        ^EClassifier type  (eclassifier qname)]
    (cond
     (and (not neg) (not exact)) (fn [^EClass x] (.isInstance type x))
     (and (not neg) exact)       (fn [^EClass x] (identical? type (.eClass x)))
     (and neg       (not exact)) (fn [^EClass x] (not (.isInstance type x)))
     :default                    (fn [^EClass x] (not (identical? type (.eClass x)))))))

(defn type-matcher
  "Returns a matcher for either nil, !Foo!, [Foo Bar! !Baz], [:and 'Foo 'Bar],
  or [:or 'Foo 'Bar].  In a collection spec, the first element may be one of
  the keywords :or (default), :nor, :and, :nand, or :xor with the usual logic
  semantics."
  [ts]
  (cond
   (nil? ts)   identity
   (fn? ts)    ts
   (qname? ts) (type-matcher-1 ts)
   (coll? ts)  (if (seq ts)
                  (let [f (first ts)
                        [op r] (case f
                                 :and  [every-pred (next ts)]
                                 :nand [nand-fn    (next ts)]
                                 :or   [some-fn    (next ts)]
                                 :nor  [nor-fn     (next ts)]
                                 :xor  [xor-fn     (next ts)]
                                 [some-fn    ts])
                        t-matchers (map type-matcher r)]
                    (apply op t-matchers))
                  ;; Empty collection given: (), [], that's also ok
                  identity)
   :else (RuntimeException.
          (format "Don't know how to create a type matcher for %s" ts))))

(defprotocol EContents
  (eallcontents-internal [this tm]
    "Returns a seq of this and all directly and indirectly contained EObjects
  whose type matches the type-matcher `tm'.")
  (econtents-internal [this tm]
    "Returns a seq of this and all directly contained EObjects whose type
  matches the type-matcher `tm'.")
  (econtainer [this]
    "Returns the EObject containing this."))

(extend-protocol EContents
  EObject
  (econtents-internal [this tm]
    (filter tm (cons this (seq (.eContents this)))))
  (eallcontents-internal [this tm]
    (filter tm (cons this (iterator-seq (.eAllContents this)))))
  (econtainer [this]
    (.eContainer this))
  clojure.lang.IPersistentCollection
  (econtents-internal [this tm]
    (mapcat #(econtents-internal % tm) this))
  (eallcontents-internal [this tm]
    (mapcat #(eallcontents-internal % tm) this)))

(defn eallcontents
  "Returns a seq of `x' and all contents of `x' matching the type spec `ts'."
  ([x]
     (eallcontents-internal x identity))
  ([x ts]
     (eallcontents-internal x (type-matcher ts))))

(defn econtents
  "Returns a seq of `x' and its direct contents matching the type spec `ts'."
  ([x]
     (econtents-internal x identity))
  ([x ts]
     (econtents-internal x (type-matcher ts))))

(defn ref-matcher
  "Returns a reference matcher for the reference spec `rs'.
  A reference matcher is a function of arity one that gets an EReference and
  returns logical true if that ref should be accepted, false otherwise.

  Semantics depend on `rs':

    nil           => accept all references
    someERef      => accept only this EReference
    :foo          => accept only references named foo
    [:foo :bar]   => accept both foo and bar refs
    (fn [r] ...)  => simply use that"
  [rs]
  (cond
   (nil? rs)    identity
   (fn? rs)     rs
   (qname? rs)  (let [n (name rs)]
                  (fn [^EReference ref]
                    (= n (.getName ref))))
   (instance? EReference rs) (fn [r] (= rs r))
   (coll? rs)  (if (seq rs)
                 (apply some-fn (map ref-matcher rs))
                  ;; Empty collection given: (), [], that's also ok
                  identity)
   :else (RuntimeException.
          (format "Don't know how to create a reference matcher for %s" rs))))

(defprotocol EReferences
  (ecrossrefs-internal [this rm]
    "Returns a seq of cross-referenced EObjects accepted by reference-matcher
    `rm'.  Cross-referenced objects are those that are referenced by a
    non-containment relationship.")
  (erefs-internal [this rm]
    "Returns a seq of referenced EObjects accepted by reference-matcher `rm'.
    In contrast to ecrossrefs-internal, containment refs are not excluded.")
  (inv-erefs-internal [this rm]))

(defn- eopposite-refs
  "Returns the seq of `eo's EClass' references whose opposites match `here-rm'.

  Example: [Foo] f --- b [Bar]
              f \\
                 `---- c [Car]

  Given a Foo object and a ref-matcher matching f, returns a seq of the
  EReferences b and c, because those are the opposites of the matched f.  Of
  course, if `here-rm' matches only one specific EReference, i.e., it was
  constructed by (ref-matcher fERef) and not (ref-matcher :f)."
  [^EObject eo here-rm]
  (seq (remove nil? (map (fn [^EReference r]
                           (when-let [o (.getEOpposite r)]
                             (when (here-rm o) r)))
                         (seq (-> eo .eClass .getEAllReferences))))))

(extend-protocol EReferences
  EObject
  (ecrossrefs-internal [this rm]
    (let [^org.eclipse.emf.ecore.util.EContentsEList$FeatureIterator it
          (-> this .eCrossReferences .iterator)]
      (loop [r []]
        (if (.hasNext it)
          (let [eo (.next it)]
            (recur (if (rm (.feature it))
                     (conj r eo)
                     r)))
          r))))
  (erefs-internal [this rm]
    (loop [r [], refs (seq (-> this .eClass .getEAllReferences))]
      (if (seq refs)
        (let [^EReference ref (first refs)]
          (recur (if (rm ref)
                   (if (.isMany ref)
                     (into r (.eGet this ref))
                     (conj r (.eGet this ref)))
                   r)
                 (rest refs)))
        r)))
  (inv-erefs-internal [this rm]
    (if-let [opposites (eopposite-refs this rm)]
      (erefs-internal this (ref-matcher opposites))
      (error "No opposite EReferences found.")))
  clojure.lang.IPersistentCollection
  (ecrossrefs-internal [this rm]
    (mapcat #(ecrossrefs-internal % rm) this))
  (erefs-internal [this rm]
    (mapcat #(erefs-internal % rm) this))
  (inv-erefs-internal [this rm]
    (mapcat #(inv-erefs-internal % rm) this)))

(defn ecrossrefs
  "Returns a seq of EObjects cross-referenced by `eo', possibly restricted by
  the reference spec `rs'.  `eo' may be an EObject or a collection of EObjects.
  For the syntax and semantics of `rs', see `ref-matcher'.  In EMF, crossrefs
  are all non-containment refs."
  ([eo]
     (ecrossrefs-internal eo identity))
  ([eo rs]
     (ecrossrefs-internal eo (ref-matcher rs))))

(defn erefs
  "Returns a seq of EObjects referenced by `eo', possibly restricted by the
  reference spec `rs'.  `eo' may be an EObject or a collection of EObjects.
  For the syntax and semantics of `rs', see `ref-matcher'.  In contrast to
  `ecrossrefs', this function doesn't ignore containment refs."
  ([eo]
     (erefs-internal eo identity))
  ([eo rs]
     (erefs-internal eo (ref-matcher rs))))

(defn inv-erefs
  "Returns the seq of EOjects that reference `eo' with an EReference described
  by `rs'.  `eo' may also be a collection of eobjects."
  ([eo]
     (inv-erefs-internal eo identity))
  ([eo rs]
     (inv-erefs-internal eo (ref-matcher rs))))

(defprotocol EmfToClj
  (emf2clj [this]
    "Converts an EMF thingy to a clojure thingy.

  EMF Type     | Clojure Type
  -------------+-------------
  UniqueEList  | ordered-set
  EMap         | ordered-map
  EList        | seq

  All other objects are kept as-is."))

(extend-protocol EmfToClj
  UniqueEList
  (emf2clj [this] (into (ordered-set) (seq this)))
  EMap
  (emf2clj [this] (into (ordered-map) (seq this)))
  EList
  (emf2clj [this] (seq this))
  EObject
  (emf2clj [this] this)
  nil
  (emf2clj [_] nil))

(defprotocol CljToEmf
  (clj2emf [this]
    "Converts a Clojure thingy to an EMF thingy.

  Clojure Type | EMF Type
  -------------+-------------
  ordered-set  | UniqueEList
  set          | UniqueEList
  map          | EMap
  seq          | EList

  All other objects are kept as-is."))

(extend-protocol CljToEmf
  ordered.set.OrderedSet
  (clj2emf [this]
    (let [ul (org.eclipse.emf.common.util.UniqueEList. (count this))]
      (doseq [item this]
        (.add ul (clj2emf item)))
      ul))
  clojure.lang.IPersistentSet
  (clj2emf [this]
    (let [ul (org.eclipse.emf.common.util.UniqueEList. (count this))]
      (doseq [item this]
        (.add ul (clj2emf item)))
      ul))
  clojure.lang.IPersistentMap
  (clj2emf [this]
    (let [em (org.eclipse.emf.common.util.BasicEMap. (count this))]
      (doseq [[k v] this]
        (.put em (clj2emf k) (clj2emf v)))
      em))
  clojure.lang.ISeq
  (clj2emf [this]
    (let [el (org.eclipse.emf.common.util.BasicEList. (count this))]
      (doseq [item this]
        (.add el (clj2emf item)))
      el))
  java.lang.Object
  (clj2emf [this] this))

(defn eget
  "Returns the value of `eo's structural feature `sf'.
  The value is converted to some clojure type (see EmfToClj protocol).
  Throws an exception, if there's no EStructuralFeature `sf'."
  [^EObject eo sf]
  (if-let [sfeat (.getEStructuralFeature (.eClass eo) (name sf))]
    (emf2clj (.eGet eo sfeat))
    (error (format "No such structural feature %s for %s." sf (print-str eo)))))

(defn eset!
  "Sets `eo's structural feature `sf' to `value' and returns `value'.
  The value is converted to some EMF type (see CljToEmf protocol).
  Throws an exception, if there's no EStructuralFeature `sf'."
  [^EObject eo sf value]
  (if-let [sfeat (.getEStructuralFeature (.eClass eo) (name sf))]
    (do (.eSet eo sfeat (clj2emf value)) value)
    (error (format "No such structural feature %s for %s." sf (print-str eo)))))

;;** EObject Creation

(defn ecreate
  "Creates an EObject of EClass `ecls'.
  `ecls' may either be an EClass or just an EClass name given as symbol,
  string, or keyword."
  [ecls]
  (let [^EClassifier ecl (if (instance? EClass ecls)
                           ecls
                           (eclassifier ecls))
        ^EFactory f (-> ecl .getEPackage .getEFactoryInstance)]
    (.create f ecl)))

;;** Printing

;;*** Normal toString() output

;; TODO: We don't handle EFactories, ETypedElements, and EAnnotations yet.

(defn- feature-str
  "Returns a description of enabled features `fs'.
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
               (.getName ec)
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

(defmethod print-method EDataType
  [^EDataType edt ^java.io.Writer out]
  (.write out
          (str "#<EDataType " (.getName edt)
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
               (.getName ep)
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
            (let [ec (.eClass eo)]
              (str "#<"
                   (-> ec .getName)
                   (let [m (into {}
                                 (map (fn [^EAttribute attr]
                                        [(keyword (.getName attr)) (.eGet eo attr)])
                                      (seq (.getEAttributes ec))))]
                     (when (seq m)
                       (str " " m)))
                   ">")))))


