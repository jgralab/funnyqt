(ns funnyqt.utils.emf
  (:require [funnyqt.emf :as emf])
  (:import (org.eclipse.emf.ecore EPackage EClass EAttribute EReference)))

(defmacro ecore-model-ns-generator
  "A helper macro to generate metamodel specific APIs in some namespace.

  `ecore-file` is the ecore file containing the metamodel.

  The `nssym` denotes the name of the namespace in which to generate the API.
  If `nssym` is nil, generate it in the current namespace.

  `eclass-fn` is a function receiving an EClass.  It should return a valid
  definition-form, e.g., a (defn stuff-with-that-eclass [...] ...).

  `eattr-fn` is a function receiving an EAttribute name as keyword and a set of
  EClasses that have such an attribute.  It should return a valid
  definition-form.

  `eref-fn` is a function receiving an EReference name as keyword and a set of
  EClasses that have such an reference.  It should return a valid
  definition-form.

  The functions are called with all classes/attributes/roles of the metamodel."
  [ecore-file nssym eclass-fn eattr-fn eref-fn]
  (let [ecore-model (emf/load-metamodel
                     (if (.exists (clojure.java.io/file ecore-file))
                       ecore-file
                       (clojure.java.io/resource ecore-file)))
        atts (atom {}) ;; map from attribute kws to set of eclasses that have it
        refs (atom {}) ;; map from reference kws to set of eclasses that have it
        old-ns *ns*]
    `(do
       ~@(when nssym
           `[(ns ~nssym)])
       (emf/with-ns-uris ~(mapv #(.getNsURI ^EPackage %)
                                (emf/metamodel-epackages ecore-model))
         ~@(emf/with-ns-uris (mapv #(.getNsURI ^EPackage %)
                                   (emf/metamodel-epackages ecore-model))
             (concat
              (doall
               (mapcat
                (fn [^EClass ecl]
                  (doseq [a (map #(keyword (.getName ^EAttribute %))
                                 (seq (.getEAttributes ecl)))]
                    (swap! atts
                           #(update-in %1 [%2] conj ecl)
                           a))
                  (doseq [r (map #(keyword (.getName ^EReference %))
                                 (seq (.getEReferences ecl)))]
                    (swap! refs
                           #(update-in %1 [%2] conj ecl)
                           r))
                  ((resolve eclass-fn) ecl))
                (emf/eclassifiers)))
              (for [[a owners] @atts]
                ((resolve eattr-fn) a owners))
              (for [[r owners] @refs]
                ((resolve eref-fn) r owners)))))
       (in-ns '~(ns-name old-ns)))))
