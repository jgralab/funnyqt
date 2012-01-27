(ns funnyqt.emf.core
  "Core functions for accessing and manipulating EMF models."
  (:use funnyqt.utils)
  (:use funnyqt.generic)
  (:import
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.common.util URI]
   [org.eclipse.emf.ecore EPackage EObject EModelElement]))

(add-long-doc! "TODO")

;;* Code

(def epackage-registry org.eclipse.emf.ecore.EPackage$Registry/INSTANCE)

(defn- register-epackages
  [pkgs]
  (doseq [^EPackage p pkgs]
    (.put epackage-registry (.getNsURI p) p)
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

(defn load-model
  "Loads an EMF model from the XMI file `f'.
  Returns a seq of the models top-level elements."
  [f]
  (let [uri (URI/createURI f)
        res (XMIResourceImpl. uri)]
    (doto res
      (.load (.getDefaultLoadOptions res)))
    (seq (.getContents res))))

(defprotocol EObjects
  (eobjects-internal [this tm]
    "Returns a seq of all directly and indirectly contained EObjects
  whose type matches the type-matcher tm."))

(extend-protocol EObjects
  EObject
  (eobjects-internal [this tm]
    (filter tm (cons this (iterator-seq (.eAllContents this)))))
  clojure.lang.ISeq
  (eobjects-internal [this tm]
    (mapcat #(eobjects-internal % tm) this)))

(defn- get-eclassifier
  [qn]
  (let [[pn n _] (split-qname qn)]
    (if-let [p (.getEPackage epackage-registry pn)]
      (.getEClassifier p n)
      (error "No such package!"))))

(defn- type-matcher-1
  "Returns a matcher for elements Foo, !Foo, Foo!, !Foo!."
  [c]
  (let [v     (type-with-modifiers (name c))
        neg   (v 0)
        qname (v 1)
        exact (v 2)
        type  (get-eclassifier qname)]
    (cond
     (and (not neg) (not exact)) (fn [x] (.isInstance type x))
     (and (not neg) exact)       (fn [x] (identical? type (.eClass x)))
     (and neg       (not exact)) (fn [x] (not (.isInstance type x)))
     :default                    (fn [x] (not (identical? type (.eClass x)))))))

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

(defn eobjects
  ([x]
     (eobjects-internal x identity))
  ([x ts]
     (eobjects-internal x (type-matcher ts))))
