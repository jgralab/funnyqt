(ns funnyqt.relational.emf
  (:require [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt.protocols :as p]
            [funnyqt.emf :as emf]
            [funnyqt.query :as q]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.relational.util :as ru]
            [funnyqt.utils :as u])
  (:import
   (org.eclipse.emf.ecore EStructuralFeature EAttribute EReference EObject
                          EClass EPackage)
   (funnyqt.relational.tmp_elem WrapperElement TmpElement)))

(defn tmp-eobjecto
  ([m eo]
     (fn [a]
       (let [geo (cclp/walk a eo)]
         (cond
          (not (or (ru/fresh? geo) (tmp/tmp-or-wrapper-element? geo)))
          (u/errorf "tmp-eobjecto/2: eo must be fresh or a ground Wrapper/TmpElement but was %s."
                    geo)

          (ru/ground? geo)
          (if (tmp/set-kind geo :element)
            (ccl/succeed a)
            (ccl/fail a))

          :else (ccl/to-stream
                 (->> (map #(ccl/unify a eo %)
                           (concat
                            (map (partial tmp/make-wrapper m eo)
                                 (emf/eallobjects m))
                            [(tmp/make-tmp-element m :element)]))
                      (remove not)))))))
  ([m eo t]
     (fn [a]
       (let [geo (cclp/walk a eo)
             gt  (cclp/walk a t)]
         (cond
          (not (ru/ground? gt))
          (u/errorf "tmp-eobjecto/3: type must be ground.")

          (not (or (ru/fresh? geo) (tmp/tmp-or-wrapper-element? geo)))
          (u/errorf "tmp-eobjecto/3: eo must be fresh or a ground Wrapper/TmpElement but was %s."
                    geo)

          (ru/ground? geo) ;; TODO: we probably need something like tg/kind-aec-tup-from-spec, too
          (if (and (tmp/set-kind geo :element)
                   (tmp/set-type geo gt))
            (ccl/succeed a)
            (ccl/fail a))

          :else (ccl/to-stream
                 (->> (map #(ccl/unify a eo %)
                           (concat
                            (map (partial tmp/make-wrapper m eo)
                                 (emf/eallobjects m gt))
                            [(tmp/make-tmp-element m :element gt)]))
                      (remove not))))))))

(defn eobjecto
  "A relation where EObject `e` has the type `t`, an EClass name in Resouce or
  ResourceSet `m`.  In fact, `t` may be any type specification (see
  `funnyqt.protocols/type-matcher`)."
  ([m eo]
     (if tmp/*make-tmp-elements*
       (tmp-eobjecto m eo)
       (fn [a]
         (let [geo (cclp/walk a eo)]
           (if (ru/ground? geo)
             (if (emf/eobject? geo) (ccl/succeed a) (ccl/fail a))
             (ccl/to-stream
              (->> (map #(ccl/unify a eo %)
                        (emf/eallobjects m))
                   (remove not))))))))
  ([m eo t]
     (if tmp/*make-tmp-elements*
       (tmp-eobjecto m eo t)
       (fn [a]
         (let [geo (cclp/walk a eo)
               gt (cclp/walk a t)]
           (cond
            (or (and (ru/ground? geo) (not (emf/eobject? geo)))
                (and (ru/ground? gt) (not (or (symbol? gt) (coll? gt)))))
            (ccl/fail a)

            (and (ru/ground? geo) (ru/ground? gt))
            (if (p/has-type? geo gt)
              (ccl/succeed a)
              (ccl/fail a))

            (ru/ground? geo)
            (ccl/unify a t (p/qname geo))

            (ru/ground? gt)
            (ccl/to-stream
             (->> (map #(ccl/unify a eo %) (emf/eallobjects m t))
                  (remove not)))

            :else (ccl/to-stream
                   (->> (for [elem (emf/eallobjects m t)]
                          (ccl/unify a [eo t] [elem (p/qname elem)]))
                        (remove not)))))))))

(defn ^:private attribute-list [eo]
  (seq (.getEAllAttributes (.eClass ^EObject eo))))

(defn tmp-valueo [m eo at val]
  (fn [a]
    (let [geo  (cclp/walk a eo)
          gat  (cclp/walk a at)
          gval (cclp/walk a val)]
      (cond
       (not (tmp/tmp-or-wrapper-element? geo))
       (u/errorf "tmp-valueo: eo has to be a ground Tmp/WrapperElement but was %s."
                 geo)

       (not (keyword? gat))
       (u/errorf "tmp-valueo: az must be a ground keyword but was %s." gat)

       :else (if (tmp/add-attr geo gat val)
               (ccl/succeed a)
               (ccl/fail a))))))

(defn valueo
  "A relation where EObject `eo` has value `val` for its `at` attribute in
  EMF Resource or ResourceSet `m`."
  [m eo at val]
  (if tmp/*make-tmp-elements*
    (tmp-valueo m eo at val)
    (fn [a]
      (let [geo  (cclp/walk a eo)
            gat  (cclp/walk a at)
            gval (cclp/walk a val)]
        (cond
         (or (and (ru/ground? geo) (not (emf/eobject? geo)))
             (and (ru/ground? gat) (not (keyword? gat)))
             (and (ru/ground? geo) (ru/ground? gat)
                  (not (when-let [sf (.getEStructuralFeature
                                      (.eClass ^EObject geo) ^String (name gat))]
                         (instance? EAttribute sf)))))
         (ccl/fail a)

         (and (ru/ground? geo) (ru/ground? gat))
         (ccl/unify a val (emf/eget geo gat))

         (ru/ground? geo)
         (ccl/to-stream
          (->> (for [^EAttribute attr (attribute-list geo)
                     :let [an (keyword (.getName attr))]]
                 (ccl/unify a [at val] [an (emf/eget geo an)]))
               (remove not)))

         :else (ccl/to-stream
                (->> (for [^EObject elem (emf/eallobjects m)
                           ^EAttribute attr (attribute-list elem)
                           :let [an (keyword (.getName attr))]]
                       (ccl/unify a [eo at val] [elem an (emf/eget elem an)]))
                     (remove not))))))))

(defn ^:private reference-list [eo]
  (seq (.getEAllReferences (.eClass ^EObject eo))))

(defn tmp-adjo [m eo ref reo]
  (fn [a]
    (let [geo  (cclp/walk a eo)
          gref (cclp/walk a ref)
          greo (cclp/walk a reo)]
      (cond
       (not (tmp/tmp-or-wrapper-element? geo))
       (u/errorf "tmp-adjo: geo has to be a ground Tmp/WrapperElement but was %s."
                 geo)

       (not (keyword? gref))
       (u/errorf "tmp-adjo: ref must be a ground keyword but was %s." gref)

       (or (and (tmp/tmp-or-wrapper-element? geo) (tmp/tmp-or-wrapper-element? greo))
           (and (tmp/tmp-element? geo)            (ru/fresh? greo)))
       (if (tmp/add-ref geo gref reo)
         (ccl/succeed a)
         (ccl/fail a))

       (and (tmp/wrapper-element? geo) (ru/fresh? greo))
       (ccl/to-stream
        (->> (map #(ccl/unify a reo (if (fn? %) (%) %))
                  (concat
                   (map #(tmp/make-wrapper m reo %)
                        (q/adjs (.wrapped-element ^WrapperElement geo) gref))
                   [#(let [refed (tmp/make-tmp-element m :element)]
                       (tmp/add-ref geo gref reo)
                       refed)]))
             (remove not)))

       :else (u/errorf "unsupported args to tmp-adjo:\n  v = %s\n  role = %s\n rv = %s"
                       geo gref greo)))))

(defn adjo
  "A relation where `eo` references `reo` with its `ref` reference in the EMF
  Resource or ResourceSet `m`."
  [m eo ref reo]
  (if tmp/*make-tmp-elements*
    (tmp-adjo m eo ref reo)
    (fn [a]
      (let [geo  (cclp/walk a eo)
            gref (cclp/walk a ref)
            greo (cclp/walk a reo)]
        (cond
         (or (and (ru/ground? geo) (not (emf/eobject? geo)))
             (and (ru/ground? gref) (not (keyword? gref)))
             (and (ru/ground? greo) (not (emf/eobject? greo)))
             (and (ru/ground? geo) (ru/ground? gref)
                  (not (when-let [sf (.getEStructuralFeature
                                      (.eClass ^EObject geo) ^String (name gref))]
                         (instance? EReference sf)))))
         (ccl/fail a)

         (and (ru/ground? geo) (ru/ground? gref))
         (ccl/to-stream
          (->> (for [refed (funnyqt.query/adjs* geo gref)]
                 (ccl/unify a [reo] [refed]))
               (remove not)))

         (ru/ground? geo)
         (ccl/to-stream
          (->> (for [^EReference reference (reference-list geo)
                     :let [rn (keyword (.getName reference))]
                     refed (funnyqt.query/adjs* geo rn)]
                 (ccl/unify a [ref reo] [rn refed]))
               (remove not)))

         :else (ccl/to-stream
                (->> (for [^EObject elem (emf/eallobjects m)
                           ^EReference reference (reference-list elem)
                           :let [rn (keyword (.getName reference))]
                           refed (funnyqt.query/adjs* elem rn)]
                       (ccl/unify a [eo ref reo] [elem rn refed]))
                     (remove not))))))))

;;# Metamodel specific relations

(defn ^:private class->rel-symbols
  "Returns a relation symbol for the eclass `c`."
  [^EClass c prefix]
  (let [dup (emf/eclassifier (symbol (.getName c)))
        fqn (p/qname c)
        n (if (= dup c)
            (.getName c)
            fqn)]
    (mapv (fn [s]
            (with-meta (symbol s)
              {:relation-name
               (symbol (str prefix (clojure.string/replace
                                    s #"([!])?.*[.]" #(or (nth % 1) ""))))}))
          [fqn (str fqn "!") (str "!" fqn) (str "!" fqn "!")])))

(defn ^:private create-eclass-relations
  "Creates relations for the given eclass."
  [ecls prefix]
  `(do
     ~@(for [na (class->rel-symbols ecls prefix)]
         `(defn ~(:relation-name (meta na))
            ~(format "A relation where `eo` is an %s EObject." na)
            [~'m ~'eo]
            (eobjecto ~'m ~'eo '~na)))))

(defn ^:private create-ereference-relation
  "Creates relations for the given EReference."
  [eref ecls prefix]
  (let [ts (mapv #(p/qname %) ecls)]
    `(defn ~(symbol (str prefix "->" (clojure.string/replace (name eref) "_" "-")))
       ~(format "A relation where `eo` includes `reo` in its %s reference." eref)
       [~'m ~'eo ~'reo]
       (ccl/all
        (eobjecto ~'m ~'eo '~ts)
        (adjo ~'m ~'eo ~eref ~'reo)))))

(defn ^:private create-eattribute-relation
  "Creates relations for the given EAttribute."
  [attr ecls prefix]
  ;; attr is an attr name symbol, ecls the set of classes having
  ;; such an attr
  (let [ts (mapv #(p/qname %) ecls)]
    `(defn ~(symbol (str prefix (clojure.string/replace (name attr) "_" "-")))
       ~(format "A relation where `eo` has value `val` for its %s attribute." attr)
       [~'m ~'eo ~'val]
       (ccl/all
        (eobjecto ~'m ~'eo '~ts)
        (valueo ~'m ~'eo ~attr ~'val)))))


;;# Main

(defmacro generate-ecore-model-relations
  "Generates metamodel-specific relations in the namespace denoted by `nssym`.
  `ecore-file` is the ecore file containing the metamodel.

  If `nssym` is nil (or not given), generate them in the current namespace, and
  require it as `alias` (if non-nil/given).

  `prefix` is an optional prefix all relations should have.  (Useful if you
  generate in the current namespace.)

  For any EClass Foo, there will be a relation (+Foo model el) that succeeds
  for all EObjects el in the model that have the type Foo.  Similarly, there
  are relations +!Foo, +Foo!, and +!Foo! restricting to exact Foo instances
  and/or negating.

  For any attribute bar, there will be a relation (+bar model el val) where el
  is an element of model that posesses a bar attribute whose value is val.

  For any reference baz, there will be a relation (+baz model el ref) where el
  is an element of model that posesses a baz reference that links to ref.  If
  baz is a multi-valued reference, then ref is contained in that list.

  Property names containing an underscore will result in relations with a
  hyphen instead, e.g., attribute \"is_persistent\" is translated into a
  relation +is-persistent."
  ([ecore-file]
     `(generate-ecore-model-relations ~ecore-file nil nil nil))
  ([ecore-file nssym]
     `(generate-ecore-model-relations ~ecore-file ~nssym nil nil))
  ([ecore-file nssym alias]
     `(generate-ecore-model-relations ~ecore-file ~nssym ~alias nil))
  ([ecore-file nssym alias prefix]
     `(emf/ecore-model-ns-generator ~ecore-file ~nssym ~alias ~prefix
                                    create-eclass-relations
                                    create-eattribute-relation
                                    create-ereference-relation)))
