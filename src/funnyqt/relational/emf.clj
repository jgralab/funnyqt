(ns funnyqt.relational.emf
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.core.logic.protocols :only [walk]]
        funnyqt.relational.util)
  (:require funnyqt.protocols
            funnyqt.emf
            funnyqt.query
            funnyqt.query.emf
            [funnyqt.utils :as u]
            [funnyqt.relational :as rel]
            clojure.java.io)
  (:import
   (org.eclipse.emf.ecore
    EStructuralFeature EAttribute EReference EObject EClass EPackage)))

;;# Utilities

(defn eobjecto
  "A relation where EObject `e` has the type `t`, an EClass name in EMFModel `m`.
  In fact, `t` may be any type specification (see
  `funnyqt.protocols/type-matcher`)."
  ([m eo]
     (fn [a]
       (let [geo (walk a eo)]
         (if (ground? geo)
           (if (funnyqt.emf/eobject? geo) (succeed a) (fail a))
           (to-stream
            (->> (map #(unify a eo %)
                      (funnyqt.emf/eallobjects m))
                 (remove not)))))))
  ([m e t]
     (fn [a]
       (let [ge (walk a e)
             gt (walk a t)]
         (cond
          (and (ground? ge) (ground? gt))
          (if (and (funnyqt.emf/eobject? ge)
                   (or (coll? gt) (symbol? gt))
                   (funnyqt.protocols/has-type? ge gt))
            (succeed a)
            (fail a))

          (ground? ge)
          (or (and (funnyqt.emf/eobject? ge)
                   (unify a t (funnyqt.protocols/qname ge)))
              (fail a))

          (ground? gt)
          (to-stream
           (->> (map #(unify a e %)
                     (funnyqt.emf/eallobjects m t))
                (remove not)))

          :else (to-stream
                 (->> (for [elem (funnyqt.emf/eallobjects m t)]
                        (unify a [e t] [elem (funnyqt.protocols/qname elem)]))
                      (remove not))))))))

(defn valueo
  "A relation where EObject `eo` has value `val` for its `at` attribute in
  EMFModel `m`."
  [m eo at val]
  (fn [a]
    (let [geo  (walk a eo)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (and (ground? geo)
            (ground? gat))
       (or (and (funnyqt.emf/eobject? geo)
                (keyword? gat)
                (when-let [sf (.getEStructuralFeature (.eClass ^EObject geo)
                                                       ^String (name gat))]
                  (instance? EAttribute sf))
                (unify a val (funnyqt.emf/eget geo gat)))
           (fail a))

       (ground? geo)
       (if (funnyqt.emf/eobject? geo)
         (to-stream
          (->> (for [^EAttribute attr (seq (.getEAllAttributes (.eClass ^EObject geo)))
                     :let [an (keyword (.getName attr))]]
                 (unify a [at val] [an (funnyqt.emf/eget geo an)]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (funnyqt.emf/eallobjects m)
                         ^EAttribute attr (seq (.getEAllAttributes (.eClass elem)))
                         :let [an (keyword (.getName attr))]]
                     (unify a [eo at val] [elem an (funnyqt.emf/eget elem an)]))
                   (remove not)))))))

(defn adjo
  "A relation where `eo` references `reo` with its `ref` reference in EMFModel
  `m`."
  [m eo ref reo]
  (fn [a]
    (let [geo  (walk a eo)
          gref (walk a ref)
          greo (walk a reo)]
      (cond
       (and (ground? geo)
            (ground? gref))
       (if (and (funnyqt.emf/eobject? geo)
                (keyword? gref)
                (when-let [sf (.getEStructuralFeature (.eClass ^EObject geo)
                                                       ^String (name gref))]
                  (instance? EReference sf)))
         (to-stream
          (->> (for [refed (funnyqt.query/adjs* geo gref)]
                 (unify a [reo] [refed]))
               (remove not)))
         (fail a))

       (ground? geo)
       (if (funnyqt.emf/eobject? geo)
         (to-stream
          (->> (for [^EReference reference (seq (.getEAllReferences (.eClass ^EObject geo)))
                     :let [rn (keyword (.getName reference))]
                     refed (funnyqt.query/adjs* geo rn)]
                 (unify a [ref reo] [rn refed]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (funnyqt.emf/eallobjects m)
                         ^EReference reference (seq (.getEAllReferences (.eClass elem)))
                         :let [rn (keyword (.getName reference))]
                         refed (funnyqt.query/adjs* elem rn)]
                     (unify a [eo ref reo] [elem rn refed]))
                   (remove not)))))))

;;# Metamodel specific relations

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the eclass `c`."
  [^EClass c]
  (let [dup (funnyqt.emf/eclassifier (symbol (.getName c)))
        fqn (funnyqt.protocols/qname c)
        n (if (= dup c)
            (.getName c)
            fqn)]
    (mapv (fn [s]
            (with-meta (symbol s)
              {:unique-name
               (symbol (str "+" (clojure.string/replace
                                 s #"([!])?.*[.]" #(or (nth % 1) ""))))}))
          [fqn (str fqn "!") (str "!" fqn) (str "!" fqn "!")])))

(defn ^:private create-eclass-relations
  "Creates relations for the given eclass."
  [ecls]
  (for [na (class->rel-symbols ecls)]
    `(defn ~(:unique-name (meta na))
       ~(format "A relation where `eo` is an %s EObject." na)
       [~'m ~'eo]
       (eobjecto ~'m ~'eo '~na))))

(defn ^:private create-ereference-relation
  "Creates relations for the given EReference."
  [[eref ecls]]
  (let [ts (mapv #(funnyqt.protocols/qname %) ecls)]
    `(defn ~(symbol (str "+->" (name eref)))
       ~(format "A relation where `eo` includes `reo` in its %s reference." eref)
       [~'m ~'eo ~'reo]
       (all
        (eobjecto ~'m ~'eo '~ts)
        (adjo ~'m ~'eo ~eref ~'reo)))))

(defn ^:private create-eattribute-relation
  "Creates relations for the given EAttribute."
  [[attr ecls]] ;; attr is an attr name symbol, ecls the set of classes having
                ;; such an attr
  (let [ts (mapv #(funnyqt.protocols/qname %) ecls)]
    `(defn ~(symbol (str "+" (name attr)))
       ~(format "A relation where `eo` has value `val` for its %s attribute." attr)
       [~'m ~'eo ~'val]
       (all
        (eobjecto ~'m ~'eo '~ts)
        (valueo ~'m ~'eo ~attr ~'val)))))


;;# Main

(defmacro generate-ecore-model-relations
  "Generates metamodel-specific relations in the namespace denoted by `nssym`.
  If `nssym` is nil (or not given), generate them in the current namespace.
  `ecore-file` is the ecore file containing the metamodel."
  ([ecore-file] `(generate-ecore-model-relations ~ecore-file nil))
  ([ecore-file nssym]
     (let [ecore-model (funnyqt.emf/load-metamodel
                        (if (.exists (clojure.java.io/file ecore-file))
                          ecore-file
                          (clojure.java.io/resource ecore-file)))
           atts (atom {}) ;; map from attribute names to set of eclasses that have it
           refs (atom {}) ;; map from reference names to set of eclasses that have it
           old-ns *ns*]
       `(do
          ~@(when nssym
              `[(ns ~nssym
                  (:refer-clojure :exclude [~'==]))])
          ;; Metamodel specific relations
          ~@(funnyqt.emf/with-ns-uris (mapv #(.getNsURI ^EPackage %)
                                            (funnyqt.emf/metamodel-epackages ecore-model))
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
                   (create-eclass-relations ecl))
                 (funnyqt.emf/eclassifiers)))
               (for [^EAttribute a @atts]
                 (create-eattribute-relation a))
               (for [^EReference r @refs]
                 (create-ereference-relation r))))
          (in-ns '~(ns-name old-ns))))))

