(ns funnyqt.relational.emf
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use [funnyqt.relational])
  (:require [funnyqt.emf :as core])
  (:require [funnyqt.protocols :as genprots])
  (:import
   (org.eclipse.emf.ecore EStructuralFeature EAttribute EReference EObject EClass
                          EPackage)))

;;# Utilities

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
  (let [v (gensym "eo")]
    (for [na (class->rel-symbols ecls)]
      `(defn ~(:unique-name (meta na))
         {:doc ~(format "A relation where `%s` is an %s EObject." v na)}
         [~v]
         (fn [a#]
           (let [v# (walk a# ~v)]
             (if (fresh? v#)
               (to-stream
                (->> (map #(unify a# ~v %)
                          (core/eallobjects ~'+model+ '~na))
                     (remove not)))
               (if (and (core/eobject? v#)
                        (genprots/has-type? v# '~na))
                 a#
                 (fail a#)))))))))

(defn- eget-as-seq
  [^EObject eo ^EStructuralFeature sf]
  (if (.isMany sf)
    (core/eget eo sf)
    (list (core/eget eo sf))))

(defn- create-ereference-relation
  "Creates relations for the given EReference."
  [[eref ecls]]
  (let [ts     (mapv #(genprots/qname %) ecls) ;; a type spec
        elem   (gensym "elem")
        val    (gensym "val")]
    `(defn ~(symbol (str "+-" (name eref)))
       {:doc ~(format
               "A relation where `%s' includes `%s' in its %s reference."
               elem val eref)}
       [~elem ~val]
       (fn [a#]
         (let [elem# (walk a# ~elem)]
           (cond
            (and (ground? elem#)
                 (core/eobject? elem#))
            (to-stream
             (->> (map (unify a# ~val (eget-as-seq elem# ~eref)))
                  (remove not)))

            :else (to-stream
                   (->> (for [e# (core/eallobjects ~'+model+ '~ts)
                              v# (eget-as-seq e# ~eref)]
                          (unify a# [~elem ~val] [e# v#]))
                        (remove not)))))))))

(defn- create-eattribute-relation
  "Creates relations for the given EAttribute."
  [[attr ecls]] ;; attr is an attr name symbol, ecls the set of classes having
                ;; such an attr
  (let [ts     (mapv #(genprots/qname %) ecls) ;; a type spec
        elem   (gensym "elem")
        val    (gensym "val")]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `%s' has value `%s' for its %s attribute."
               elem val attr)}
       [~elem ~val]
       (fn [a#]
         (let [elem# (walk a# ~elem)]
           (cond
            (and (ground? elem#)
                 (core/eobject? elem#))
            (or (unify a# ~val (core/eget elem# ~attr))
                (fail a#))

            :else (to-stream
                   (->> (for [e# (core/eallobjects ~'+model+ '~ts)
                              :let [v# (core/eget e# ~attr)]]
                          (unify a# [~elem ~val] [e# v#]))
                        (remove not)))))))))

;;# Main

(defn create-epackage-relations-ns
  "Populates the namespace `nssym` (a symbol) with relations reflecting the
  EClasses in EPackage `epkg` (and subpackages)."
  [^EPackage epkg nssym]
  (let [atts (atom {}) ;; map from attribute names to set of eclasses that have it
        refs (atom {}) ;; map from reference names to set of eclasses that have it
        old-ns *ns*
        code `(do
                (ns ~nssym
                  (:refer-clojure :exclude [~'==])
                  (:use [clojure.core.logic])
                  (:use [funnyqt.relational]))
                ;; The model of this namespace, to be set later on.
                (def ~'+model+ nil)
                ;;;;;;;;;;;;;;;;;;;;;;;
                ;; Generic relations ;;
                ;;;;;;;;;;;;;;;;;;;;;;;
                (defn ~'eobjecto
                  "A relation where `eo` is an EObject."
                  [eo#]
                  (fn [a#]
                    (let [geo# (walk a# eo#)]
                      (if (fresh? geo#)
                        (to-stream
                         (->> (map #(unify a# eo# %)
                                   (core/eallobjects ~'+model+))
                              (remove not)))
                        (if (core/eobject? geo#)
                          a#
                          (fail a#))))))

                (defn ~'valueo
                  "A relation where `ae` has value `val` for its `at` attribute."
                  [ae# at# val#]
                  (fn [a#]
                    (let [gae#  (walk a# ae#)
                          gat#  (walk a# at#)
                          gval# (walk a# val#)]
                      (cond
                       (and (ground? gae#)
                            (ground? gat#)
                            (core/eobject? gae#)
                            (or (keyword? gat#) (string? gat#) (symbol? gat#)))
                       (or (unify a# [ae# at# val#] [gae# gat# (core/eget gae# gat#)])
                           (fail a#))

                       (and (ground? gae#)
                            (core/eobject? gae#))
                       (to-stream
                        (->> (for [^EAttribute attr# (seq (.getEAllAttributes
                                                          ^EClass (.eClass gae#)))
                                   :let [an# (keyword (.getName attr#))]]
                               (unify a# [ae# at# val#] [gae# an# (core/eget gae# an#)]))
                             (remove not)))

                       :else (to-stream
                              (->> (for [elem# (core/eallobjects ~'+model+)
                                         ^EAttribute attr# (seq (.getEAllAttributes
                                                                 ^EClass (.eClass elem#)))
                                         :let [an# (keyword (.getName attr#))]]
                                     (unify a# [ae# at# val#] [elem# an# (core/eget elem# an#)]))
                                   (remove not)))))))
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Schema specific relations ;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ~@(doall
                   (mapcat
                    (fn [^EClass ecl]
                      (doseq [a (map #(keyword (.getName ^EAttribute %))
                                     (seq (.getEAttributes ecl)))]
                        (swap! atts
                               #(assoc %1 %2 (clojure.set/union (get %1 %2) #{ecl}))
                               a))
                      (doseq [r (map #(keyword (.getName ^EReference %))
                                     (seq (.getEReferences ecl)))]
                        (swap! refs
                               #(assoc %1 %2 (clojure.set/union (get %1 %2) #{ecl}))
                               r))
                      `(~@(create-eclass-relations ecl)))
                    (core/with-ns-uris [(.getNsURI epkg)]
                      (core/eclassifiers))))
                ;;~(clojure.pprint/pprint @atts)
                ~@(doall
                   (for [^EAttribute a @atts]
                     `(~@(create-eattribute-relation a))))
                ~@(doall
                   (for [^EReference r @refs]
                     `(~@(create-ereference-relation r)))))]
    ;; (clojure.pprint/pprint code)
    (eval code)
    (in-ns (ns-name old-ns))
    nssym))

