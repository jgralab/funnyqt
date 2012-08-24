(ns funnyqt.relational.emf
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.util
        [funnyqt.relational :only [*model*]])
  (:require [funnyqt.protocols :as genprots]
            [funnyqt.emf :as core]
            [funnyqt.query :as query]
            funnyqt.query.emf)
  (:import
   (org.eclipse.emf.ecore
    EStructuralFeature EAttribute EReference EObject EClass EPackage)))

;;# Utilities

(defn eobjecto
  "A relation where `eo` is an EObject."
  [eo]
  (fn [a]
    (let [geo (walk a eo)]
      (if (ground? geo)
        (if (core/eobject? geo) (succeed a) (fail a))
        (to-stream
         (->> (map #(unify a eo %)
                   (core/eallobjects *model*))
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
       (or (and (core/eobject? geo) (keyword? gat)
                (unify a val (core/eget geo gat)))
           (fail a))

       (ground? geo)
       (if (core/eobject? geo)
         (to-stream
          (->> (for [^EAttribute attr (seq (.getEAllAttributes
                                            ^EClass (.eClass ^EObject geo)))
                     :let [an (keyword (.getName attr))]]
                 (unify a [at val] [an (core/eget geo an)]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (core/eallobjects *model*)
                         ^EAttribute attr (seq (.getEAllAttributes
                                                 ^EClass (.eClass elem)))
                         :let [an (keyword (.getName attr))]]
                     (unify a [eo at val] [elem an (core/eget elem an)]))
                   (remove not)))))))

(defn referenceo
  "A relation where `eo` references `reo` with its `ref` reference."
  [eo ref reo]
  (fn [a]
    (let [geo  (walk a eo)
          gref (walk a ref)
          greo (walk a reo)]
      (cond
       (and (ground? geo) (ground? gref))
       (if (and (core/eobject? geo) (keyword? gref))
         (to-stream
          (->> (for [refed (query/adjs* geo gref)]
                 (unify a [reo] [refed]))
               (remove not)))
         (fail a))

       (ground? geo)
       (if (core/eobject? geo)
         (to-stream
          (->> (for [^EReference reference (seq (.getEAllReferences
                                                 ^EClass (.eClass ^EObject geo)))
                     :let [rn (keyword (.getName reference))]
                     refed (query/adjs* geo rn)]
                 (unify a [ref reo] [rn refed]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [^EObject elem (core/eallobjects *model*)
                         ^EReference reference (seq (.getEAllReferences
                                                      ^EClass (.eClass elem)))
                         :let [rn (keyword (.getName reference))]
                         refed (query/adjs* elem rn)]
                     (unify a [eo ref reo] [elem rn refed]))
                   (remove not)))))))

(defn- class->rel-symbols
  "Returns a relation symbol for the eclass `c`."
  [^EClass c]
  (let [dup (core/eclassifier (symbol (.getName c)))
        fqn (genprots/qname c)
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
  (let [v 'eo]
    (for [na (class->rel-symbols ecls)]
      `(defn ~(:unique-name (meta na))
         {:doc ~(format "A relation where `%s` is an %s EObject." v na)}
         [~v]
         (fn [a#]
           (let [v# (walk a# ~v)]
             (if (fresh? v#)
               (to-stream
                (->> (map #(unify a# ~v %)
                          (core/eallobjects *model* '~na))
                     (remove not)))
               (if (and (core/eobject? v#)
                        (genprots/has-type? v# '~na))
                 (succeed a#)
                 (fail a#)))))))))

(defn- create-ereference-relation
  "Creates relations for the given EReference."
  [[eref ecls]]
  (let [ts     (mapv #(genprots/qname %) ecls) ;; a type spec
        elem   'eo
        val    'refed-eo]
    `(defn ~(symbol (str "+->" (name eref)))
       {:doc ~(format
               "A relation where `%s` includes `%s` in its %s reference."
               elem val eref)}
       [~elem ~val]
       (fn [a#]
         (let [gelem# (walk a# ~elem)]
           (cond
            (ground? gelem#)
            (if (core/eobject? gelem#)
              (to-stream
               (->> (map #(unify a# ~val %)
                         (query/adjs* gelem# ~eref))
                    (remove not)))
              (fail a#))

            :else (to-stream
                   (->> (for [e# (core/eallobjects *model* '~ts)
                              v# (query/adjs* e# ~eref)]
                          (unify a# [~elem ~val] [e# v#]))
                        (remove not)))))))))

(defn- create-eattribute-relation
  "Creates relations for the given EAttribute."
  [[attr ecls]] ;; attr is an attr name symbol, ecls the set of classes having
                ;; such an attr
  (let [ts     (mapv #(genprots/qname %) ecls) ;; a type spec
        elem   'eo
        val    'val]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `%s' has value `%s' for its %s attribute."
               elem val attr)}
       [~elem ~val]
       (fn [a#]
         (let [elem# (walk a# ~elem)]
           (cond
            (ground? elem#)
            (or (and (core/eobject? elem#)
                     (unify a# ~val (core/eget elem# ~attr)))
                (fail a#))

            :else (to-stream
                   (->> (for [e# (core/eallobjects *model* '~ts)
                              :let [v# (core/eget e# ~attr)]]
                          (unify a# [~elem ~val] [e# v#]))
                        (remove not)))))))))


;;# Main

(defn create-ecore-model-relations-ns
  "Populates the namespace `nssym` (a symbol) with relations reflecting the
  EClasses in the EPackages of EcoreModel `ecore-model`."
  [ecore-model nssym]
  (let [atts (atom {}) ;; map from attribute names to set of eclasses that have it
        refs (atom {}) ;; map from reference names to set of eclasses that have it
        old-ns *ns*
        code `(do
                (ns ~nssym
                  (:refer-clojure :exclude [~'==])
                  (:use clojure.core.logic)
                  (:use funnyqt.relational
                        funnyqt.relational.emf))
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Schema specific relations ;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ~@(core/with-ns-uris (mapv #(.getNsURI ^EPackage %)
                                           (core/metamodel-epackages ecore-model))
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
                       (core/eclassifiers)))
                     (for [^EAttribute a @atts]
                       (create-eattribute-relation a))
                     (for [^EReference r @refs]
                       (create-ereference-relation r)))))]
    ;;(clojure.pprint/pprint code)
    (eval code)
    (in-ns (ns-name old-ns))
    nssym))
