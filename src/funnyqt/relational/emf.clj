(ns funnyqt.relational.emf
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        [clojure.core.logic.protocols :only [walk]]
        funnyqt.relational.util)
  (:require funnyqt.protocols
            [funnyqt.emf :as emf]
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
           (if (emf/eobject? geo) (succeed a) (fail a))
           (to-stream
            (->> (map #(unify a eo %)
                      (emf/eallobjects m))
                 (remove not)))))))
  ([m e t]
     (fn [a]
       (let [ge (walk a e)
             gt (walk a t)]
         (cond
          (or (and (ground? ge) (not (emf/eobject? ge)))
              (and (ground? gt) (not (or (symbol? gt) (coll? gt)))))
          (fail a)

          (and (ground? ge) (ground? gt))
          (if (funnyqt.protocols/has-type? ge gt)
            (succeed a)
            (fail a))

          (ground? ge)
          (unify a t (funnyqt.protocols/qname ge))

          (ground? gt)
          (to-stream
           (->> (map #(unify a e %) (emf/eallobjects m t))
                (remove not)))

          :else (to-stream
                 (->> (for [elem (emf/eallobjects m t)]
                        (unify a [e t] [elem (funnyqt.protocols/qname elem)]))
                      (remove not))))))))

(defn ^:private attribute-list [eo]
  (seq (.getEAllAttributes (.eClass ^EObject eo))))

(defn valueo
  "A relation where EObject `eo` has value `val` for its `at` attribute in
  EMFModel `m`."
  [m eo at val]
  (fn [a]
    (let [geo  (walk a eo)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (or (and (ground? geo) (not (emf/eobject? geo)))
           (and (ground? gat) (not (keyword? gat)))
           (and (ground? geo) (ground? gat)
                (not (when-let [sf (.getEStructuralFeature
                                    (.eClass ^EObject geo) ^String (name gat))]
                       (instance? EAttribute sf)))))
       (fail a)

       (and (ground? geo) (ground? gat))
       (unify a val (emf/eget geo gat))

       (ground? geo)
       (to-stream
        (->> (for [^EAttribute attr (attribute-list geo)
                   :let [an (keyword (.getName attr))]]
               (unify a [at val] [an (emf/eget geo an)]))
             (remove not)))

       :else (to-stream
              (->> (for [^EObject elem (emf/eallobjects m)
                         ^EAttribute attr (attribute-list elem)
                         :let [an (keyword (.getName attr))]]
                     (unify a [eo at val] [elem an (emf/eget elem an)]))
                   (remove not)))))))

(defn ^:private reference-list [eo]
  (seq (.getEAllReferences (.eClass ^EObject eo))))

(defn adjo
  "A relation where `eo` references `reo` with its `ref` reference in EMFModel
  `m`."
  [m eo ref reo]
  (fn [a]
    (let [geo  (walk a eo)
          gref (walk a ref)
          greo (walk a reo)]
      (cond
       (or (and (ground? geo) (not (emf/eobject? geo)))
           (and (ground? gref) (not (keyword? gref)))
           (and (ground? greo) (not (emf/eobject? greo)))
           (and (ground? geo) (ground? gref)
                (not (when-let [sf (.getEStructuralFeature
                                    (.eClass ^EObject geo) ^String (name gref))]
                       (instance? EReference sf)))))
       (fail a)

       (and (ground? geo) (ground? gref))
       (to-stream
        (->> (for [refed (funnyqt.query/adjs* geo gref)]
               (unify a [reo] [refed]))
             (remove not)))

       (ground? geo)
       (to-stream
        (->> (for [^EReference reference (reference-list geo)
                   :let [rn (keyword (.getName reference))]
                   refed (funnyqt.query/adjs* geo rn)]
               (unify a [ref reo] [rn refed]))
             (remove not)))

       :else (to-stream
              (->> (for [^EObject elem (emf/eallobjects m)
                         ^EReference reference (reference-list elem)
                         :let [rn (keyword (.getName reference))]
                         refed (funnyqt.query/adjs* elem rn)]
                     (unify a [eo ref reo] [elem rn refed]))
                   (remove not)))))))

;;# Metamodel specific relations

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the eclass `c`."
  [^EClass c]
  (let [dup (emf/eclassifier (symbol (.getName c)))
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
     (let [ecore-model (emf/load-metamodel
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
                   (create-eclass-relations ecl))
                 (emf/eclassifiers)))
               (for [^EAttribute a @atts]
                 (create-eattribute-relation a))
               (for [^EReference r @refs]
                 (create-ereference-relation r))))
          (in-ns '~(ns-name old-ns))))))

