(ns funnyqt.relational.emf
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.util)
  (:require funnyqt.protocols
            funnyqt.emf
            funnyqt.query
            funnyqt.query.emf
            [funnyqt.relational :as rel]
            clojure.java.io)
  (:import
   (org.eclipse.emf.ecore
    EStructuralFeature EAttribute EReference EObject EClass EPackage)))

;;# Utilities

(defn typeo
  "A relation where the EObject `e` has the type `t`, an EClass name.
  In fact, `t` may be any type specification (see `eclass-matcher`)."
  [e t]
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
                  (funnyqt.emf/eallobjects rel/*model* gt))
             (remove not)))

       :else (to-stream
              (->> (for [elem (funnyqt.emf/eallobjects rel/*model*)]
                     (unify a [e t] [elem (funnyqt.protocols/qname elem)]))
                   (remove not)))))))

(defn eobjecto
  "A relation where `eo` is an EObject."
  [eo]
  (fn [a]
    (let [geo (walk a eo)]
      (if (ground? geo)
        (if (funnyqt.emf/eobject? geo) (succeed a) (fail a))
        (to-stream
         (->> (map #(unify a eo %)
                   (funnyqt.emf/eallobjects rel/*model*))
              (remove not)))))))

(defn valueo
  "A relation where `eo` has value `val` for its `at` attribute."
  [eo at val]
  (fn [a]
    (let [geo  (walk a eo)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (and (ground? geo)
            (ground? gat))
       (or (and (funnyqt.emf/eobject? geo) (keyword? gat)
                (unify a val (funnyqt.emf/eget geo gat)))
           (fail a))

       (ground? geo)
       (if (funnyqt.emf/eobject? geo)
         (to-stream
          (->> (for [^EAttribute attr (seq (.getEAllAttributes
                                            ^EClass (.eClass ^EObject geo)))
                     :let [an (keyword (.getName attr))]]
                 (unify a [at val] [an (funnyqt.emf/eget geo an)]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (funnyqt.emf/eallobjects rel/*model*)
                         ^EAttribute attr (seq (.getEAllAttributes
                                                 ^EClass (.eClass elem)))
                         :let [an (keyword (.getName attr))]]
                     (unify a [eo at val] [elem an (funnyqt.emf/eget elem an)]))
                   (remove not)))))))

(defn adjo
  "A relation where `eo` references `reo` with its `ref` reference."
  [eo ref reo]
  (fn [a]
    (let [geo  (walk a eo)
          gref (walk a ref)
          greo (walk a reo)]
      (cond
       (and (ground? geo) (ground? gref))
       (if (and (funnyqt.emf/eobject? geo) (keyword? gref))
         (to-stream
          (->> (for [refed (funnyqt.query/adjs* geo gref)]
                 (unify a [reo] [refed]))
               (remove not)))
         (fail a))

       (ground? geo)
       (if (funnyqt.emf/eobject? geo)
         (to-stream
          (->> (for [^EReference reference (seq (.getEAllReferences
                                                 ^EClass (.eClass ^EObject geo)))
                     :let [rn (keyword (.getName reference))]
                     refed (funnyqt.query/adjs* geo rn)]
                 (unify a [ref reo] [rn refed]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (funnyqt.emf/eallobjects rel/*model*)
                         ^EReference reference (seq (.getEAllReferences
                                                      ^EClass (.eClass elem)))
                         :let [rn (keyword (.getName reference))]
                         refed (funnyqt.query/adjs* elem rn)]
                     (unify a [eo ref reo] [elem rn refed]))
                   (remove not)))))))

(defn- class->rel-symbols
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

(defn- create-eclass-relations
  "Creates relations for the given eclass."
  [ecls]
  (for [na (class->rel-symbols ecls)]
    `(defn ~(:unique-name (meta na))
       {:doc ~(format "A relation where `eo` is an %s EObject." na)}
       [~'eo]
       (fn [~'a]
         (let [~'gv (walk ~'a ~'eo)]
           (if (fresh? ~'gv)
             (to-stream
              (->> (map (fn [~'object] (unify ~'a ~'eo ~'object))
                        (funnyqt.emf/eallobjects ~'*model* '~na))
                   (remove not)))
             (if (and (funnyqt.emf/eobject? ~'gv)
                      (funnyqt.protocols/has-type? ~'gv '~na))
               (succeed ~'a)
               (fail ~'a))))))))

(defn- create-ereference-relation
  "Creates relations for the given EReference."
  [[eref ecls]]
  (let [ts (mapv #(funnyqt.protocols/qname %) ecls)]
    `(defn ~(symbol (str "+->" (name eref)))
       {:doc ~(format
               "A relation where `eo` includes `reo` in its %s reference." eref)}
       [~'eo ~'reo]
       (fn [~'a]
         (let [~'geo (walk ~'a ~'eo)]
           (cond
            (ground? ~'geo)
            (if (funnyqt.emf/eobject? ~'geo)
              (to-stream
               (->> (map (fn [~'adj-eo] (unify ~'a ~'reo ~'adj-eo))
                         (funnyqt.query/adjs* ~'geo ~eref))
                    (remove not)))
              (fail ~'a))

            :else (to-stream
                   (->> (for [~'obj (funnyqt.emf/eallobjects ~'*model* '~ts)
                              ~'robj (funnyqt.query/adjs* ~'obj ~eref)]
                          (unify ~'a [~'eo ~'reo] [~'obj ~'robj]))
                        (remove not)))))))))

(defn- create-eattribute-relation
  "Creates relations for the given EAttribute."
  [[attr ecls]] ;; attr is an attr name symbol, ecls the set of classes having
                ;; such an attr
  (let [ts     (mapv #(funnyqt.protocols/qname %) ecls)]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `eo' has value `val' for its %s attribute." attr)}
       [~'eo ~'val]
       (fn [~'a]
         (let [~'geo (walk ~'a ~'eo)]
           (cond
            (ground? ~'geo)
            (or (and (funnyqt.emf/eobject? ~'geo)
                     (unify ~'a ~'val (funnyqt.emf/eget ~'geo ~attr)))
                (fail ~'a))

            :else (to-stream
                   (->> (for [~'obj (funnyqt.emf/eallobjects ~'*model* '~ts)
                              :let [~'oval (funnyqt.emf/eget ~'obj ~attr)]]
                          (unify ~'a [~'eo ~'val] [~'obj ~'oval]))
                        (remove not)))))))))


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
                  (:refer-clojure :exclude [~'==]))

                (def ~(vary-meta '*model* assoc :dynamic true))])

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

