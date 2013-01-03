(ns funnyqt.emf-protocols
  "EMF specific protocols and types"
  (:use funnyqt.protocols)
  (:use funnyqt.utils)
  (:use ordered.set)
  (:use ordered.map)
  (:require clojure.java.shell)
  (:import
   [org.eclipse.emf.ecore.xmi XMLResource]
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.ecore.util EcoreUtil]
   [org.eclipse.emf.common.util URI EList]
   [org.eclipse.emf.ecore.resource Resource]
   [org.eclipse.emf.ecore.resource.impl ResourceImpl]
   [org.eclipse.emf.ecore
    EcorePackage EPackage EPackage$Registry EObject EModelElement EClassifier EClass
    EDataType EEnumLiteral EEnum EFactory ETypedElement EAnnotation EAttribute EReference
    EStructuralFeature]))

;;# Code

(defmacro with-system-class-loader [& body]
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

;;## Metamodel

(defn ^:private register-epackages
  "Registeres the given packages at the EPackage$Registry by their nsURI.
  Skips packages that are already registered."
  [pkgs]
  (with-system-class-loader
    (doseq [^EPackage p pkgs]
      (when-let [uri (.getNsURI p)]
        (when (seq uri)
          (when-not (.containsKey EPackage$Registry/INSTANCE uri)
            (.put EPackage$Registry/INSTANCE uri p)))))))

(defn ^:private epks-and-subpgks
  ([pkgs]
     (epks-and-subpgks pkgs []))
  ([pkgs rv]
     (if (seq pkgs)
       (recur (mapcat (fn [^EPackage p] (seq (.getESubpackages p)))
                      pkgs)
              (into rv pkgs))
       rv)))

(defprotocol EcoreModelBasics
  "A protocol for basid EcoreModel operations."
  (load-and-register-internal [this])
  (metamodel-epackages-internal [this])
  (save-metamodel-internal [this file]))

(deftype EcoreModel [^Resource resource]
  EcoreModelBasics
  (load-and-register-internal [this]
    (.load resource nil)
    (register-epackages (metamodel-epackages-internal this)))
  ;; TODO: Implement me.
  (save-metamodel-internal [this file]
    (error "Not yet implemented!"))
  (metamodel-epackages-internal [this]
    (epks-and-subpgks (seq (.getContents resource)))))

;;## EContents

(defprotocol EContents
  (eallcontents-internal [this tm]
    "Returns a seq of all directly and indirectly contained EObjects whose type
  matches the type spec `ts` (see `funnyqt.protocols/type-matcher`).")
  (econtents-internal [this tm]
    "Returns a seq of all directly contained EObjects whose type matches the
  type spec `ts` (see `funnyqt.protocols/type-matcher`).")
  (econtainer-internal [this]
    "Returns the EObject containing this.")
  (eallobjects-internal [this tm]
    "Returns a seq of all objects matching the type specification `ts` (see
  `funnyqt.protocols/type-matcher`) that are contained in this EMFModel."))

;;## Model

(defprotocol EMFModelBasics
  "A protocol for basid EMFModel operations."
  (init-model-internal [this])
  (add-eobject!-internal [this eo])
  (add-eobjects!-internal [this eos])
  (clone-model-internal [this])
  (save-model-internal [this] [this file]))

(deftype EMFModel [^Resource resource]
  EMFModelBasics
  (init-model-internal [this]
    (.load resource nil))
  (add-eobject!-internal [this eo]
    (doto (.getContents resource)
      (.add eo))
    eo)
  (add-eobjects!-internal [this eos]
    (doto (.getContents resource)
      (.addAll eos))
    eos)
  (clone-model-internal [this]
    (let [nres (ResourceImpl.)
          nconts (.getContents nres)]
      (doseq [o (EcoreUtil/copyAll (.getContents resource))]
        (.add nconts o))
      (EMFModel. nres)))
  (save-model-internal [this]
    (if (.getURI resource)
      (.save resource nil)
      (error (str "You tried to save-metamodel-internal a non-file-Resource!\n"
                  "Use (save-model-internal m \"foo.xmi\") instead."))))
  (save-model-internal [this file]
    (let [uri (URI/createFileURI file)
          nres (XMIResourceImpl. uri)
          nconts (.getContents nres)]
      (doseq [o (EcoreUtil/copyAll (.getContents resource))]
        (.add nconts o))
      (println "Saving model to" (.toFileString uri))
      (.save nres nil))))

;;## EReferences

(defprotocol EReferences
  (epairs-internal [this reffn src-rm trg-rm src-ts trg-ts]
    "Returns the seq of edges in terms of [src-obj trg-obj] pairs.
  May be restricted by reference matchers and type specs on source and target.
  `reffn` is either `erefs-internal`, `ecrossrefs-internal`, or
  `econtents-internal`.")
  (ecrossrefs-internal [this rm]
    "Returns a seq of cross-referenced EObjects accepted by reference-matcher
  `rm`.  Cross-referenced objects are those that are referenced by a
  non-containment relationship.")
  (erefs-internal [this rm]
    "Returns a seq of referenced EObjects accepted by reference-matcher `rm`.
  In contrast to ecrossrefs-internal, containment refs are not excluded.")
  (inv-ecrossrefs-internal [this rm container]
    "Returns a seq of EObjects that cross-reference `this` with a ref matching
  `rm`.  Cross-referenced objects are those that are referenced by a
  non-containment relationship.  If `container` is nil, check only opposites of
  this object's ref, else do a search over the contents of `container`, which
  may be an EMFModel or a collection of EObjects.")
  (inv-erefs-internal [this rm container]
    "Returns a seq of EObjects that reference `this` with a ref matching `rm`.
  If `container` is nil, check only opposites of this object's ref, else do a
  search over the contents of `container`, which may be an EMFModel or a
  collection of EObjects."))

;;## Value conversion

(defprotocol EmfToClj
  (emf2clj-internal [this]
    "Converts an EMF thingy to a clojure thingy.

  EMF Type     | Clojure Type
  -------------+-------------
  UniqueEList  | ordered-set
  EMap         | ordered-map
  EList        | vector

  All other objects are kept as-is."))

