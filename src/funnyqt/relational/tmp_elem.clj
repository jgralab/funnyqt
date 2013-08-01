(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.emf :as emf]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.protocols :as p]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt.relational.util :as ru]))

(def ^:dynamic *make-tmp-elements* false)

;;# Protocols

(defprotocol IAsMap
  (as-map [this]))

(defprotocol IAttr
  (add-attr [this attr val])
  (finalize-attrs [this subst]))

(defprotocol IRef
  (add-ref [this ref target])
  (finalize-refs [this subst]))

(defprotocol IKind
  (set-kind [this k]))

(defprotocol IType
  (set-type [this t]))

(defprotocol IManifestation
  (manifest [this])
  (manifestation [this]))

(defprotocol IAlphaOmega
  (set-alpha [this a])
  (set-omega [this o])
  (finalize-alpha-and-omega [this subst]))

(defprotocol IUnset
  (unset? [this attr]))

(extend-protocol IUnset
  de.uni_koblenz.jgralab.AttributedElement
  (unset? [this attr]
    (nil? (tg/value this attr)))
  org.eclipse.emf.ecore.EObject
  (unset? [this attr]
    (not (.eIsSet this (.getEStructuralFeature (.eClass this) (name attr))))))

(defprotocol IContainmentRef
  (containment-ref? [mm-class ref-kw]))

(extend-protocol IContainmentRef
  de.uni_koblenz.jgralab.schema.VertexClass
  (containment-ref? [this ref-kw]
    (if-let [^de.uni_koblenz.jgralab.schema.impl.DirectedSchemaEdgeClass
             dec (.getDirectedEdgeClassForFarEndRole this (name ref-kw))]
      (let [ec  (.getEdgeClass dec)
            dir (.getDirection dec)
            ic  (if (= dir de.uni_koblenz.jgralab.EdgeDirection/OUT)
                  (.getTo ec)
                  (.getFrom ec))]
        (= (.getAggregationKind ic)
           de.uni_koblenz.jgralab.schema.AggregationKind/COMPOSITE))
      (u/errorf "No such role %s at metamodel class %s." ref-kw this)))
  org.eclipse.emf.ecore.EClass
  (containment-ref? [this ref-kw]
    (if-let [^org.eclipse.emf.ecore.EReference
             er (.getEStructuralFeature this (name ref-kw))]
      (.isContainment er)
      (u/errorf "No such reference %s at metamodel class %s." ref-kw this))))

(defprotocol IWrappedRelationship
  (get-src [this])
  (get-trg [this]))

(extend-protocol IWrappedRelationship
  de.uni_koblenz.jgralab.Edge
  (get-src [this] (tg/alpha this))
  (get-trg [this] (tg/omega this)))

;;# Types

(declare wrapper-element?)
(declare tmp-element?)
(declare groundify-attrs)
(declare groundify-refs)
(declare single-containers?)

;;## WrapperElement

(def element-types #{de.uni_koblenz.jgralab.Vertex org.eclipse.emf.ecore.EObject})
(def relationship-types #{de.uni_koblenz.jgralab.Edge})

(defn ^:private instance-of-any? [class-set elem]
  (some #(instance? % elem) class-set))

(deftype WrapperElement [model
                         wrapped-element
                         ^:unsynchronized-mutable attrs
                         ^:unsynchronized-mutable refs
                         ^:unsynchronized-mutable manifested]
  Object
  (toString [this]
    (str "WrapperElement@" (Integer/toHexString (hash this)) (as-map this)))
  IAsMap
  (as-map [this]
    {:model model :wrapped-element wrapped-element
     :attrs attrs :refs refs :manifested manifested})
  IKind
  (set-kind [this k]
    (when manifested (u/errorf "Already manifested: %s" this))
    (if (set? k)
      true ;; Kind ambiguous as result of attr relation, so simply succeed.
      (do
        (when-not (#{:element :relationship} k)
          (u/errorf "Unknown kind %s." k))
        (let [cur (cond
                   (instance-of-any? element-types wrapped-element) :element
                   (instance-of-any? relationship-types wrapped-element) :relationship
                   :else (u/errorf "The type of %s is not registered in %s or %s."
                                   wrapped-element #'element-types #'relationship-types))]
          (= k cur)))))
  IType
  (set-type [this t]
    (when manifested (u/errorf "Already manifested: %s" this))
    (let [cur-class (p/mm-class wrapped-element)
          super-or-eq-to-cur? #(or (= % cur-class)
                                   (p/mm-super-class? % cur-class))]
      (if (vector? t)
        (q/exists? super-or-eq-to-cur? (map (partial p/mm-class model) t))
        (super-or-eq-to-cur? (p/mm-class model t)))))
  IUnset
  (unset? [this attr]
    (unset? wrapped-element attr))
  IAttr
  (add-attr [this attr val]
    (when manifested (u/errorf "Already manifested: %s" this))
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (p/aval wrapped-element attr)]
      (cond
       (unset? this attr) (do (set! attrs (assoc attrs attr val)) true)
       (= cur val) true
       :else false)))
  (finalize-attrs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (add-ref [this ref target]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; target is either fresh or a wrapper or tmp element
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (let [cur (q/adjs wrapped-element ref)] ;; Throws if ref is no valid role name
      (cond
       (and (wrapper-element? target)
            (q/member? (.wrapped-element ^WrapperElement target) cur))
       true

       :else (do
               (set! refs (update-in refs [ref]
                                     #(conj (vec %1) %2)
                                     target))
               true))))
  (finalize-refs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! refs (groundify-refs refs subst))
    (single-containers? (p/mm-class wrapped-element) refs))
  IAlphaOmega
  (set-alpha [this a]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; a is either fresh or a wrapper or tmp element
    (when-not (instance-of-any? relationship-types wrapped-element)
      (u/errorf "Can't set alpha of non-relationship %s." wrapped-element))
    (cond
     (wrapper-element? a) (= (get-src wrapped-element) (.wrapped-element ^WrapperElement a))
     (tmp-element? a)     false
     :else                true))
  (set-omega [this o]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; o is either fresh or a wrapper or tmp element
    (when-not (instance-of-any? relationship-types wrapped-element)
      (u/errorf "Can't set omega of non-relationship %s." wrapped-element))
    (cond
     (wrapper-element? o) (= (get-trg wrapped-element) (.wrapped-element ^WrapperElement o))
     (tmp-element? o)     false
     :else                true))
  (finalize-alpha-and-omega [this subst]
    true)
  IManifestation
  (manifest [this]
    (if manifested
      wrapped-element
      (do
        (set! manifested true)
        (doseq [[at val] attrs]
          (p/set-aval! wrapped-element at val))
        (doseq [[role rs] refs]
          (doseq [r rs]
            (p/add-adj! wrapped-element role (manifest r))))
        wrapped-element)))
  (manifestation [this] wrapped-element))

(def ^{:dynamic true
       :doc "A cache (atom {}) for WrapperElements.  There is at most only one
  WrapperElement per model object and relation."}
  *wrapper-cache* nil)

(defn make-wrapper [model element]
  (let [cur (get @*wrapper-cache* element ::not-found)]
    (if (identical? cur ::not-found)
      (let [w (->WrapperElement model element {} {} false)]
        (swap! *wrapper-cache* assoc element w)
        w)
      cur)))

;;## TmpElement

(deftype TmpElement [model
                     ^:unsynchronized-mutable kind
                     ^:unsynchronized-mutable type
                     ^:unsynchronized-mutable attrs
                     ^:unsynchronized-mutable refs
                     ^:unsynchronized-mutable alpha
                     ^:unsynchronized-mutable omega
                     ^:unsynchronized-mutable manifested-element]
  Object
  (toString [this]
    (str "TmpElement@" (hash this) (as-map this)))
  IAsMap
  (as-map [this]
    {:model model :kind kind :type type :attrs attrs :refs refs
     :alpha alpha :omega omega :manifested-element manifested-element})
  IKind
  (set-kind [this k]
    (if (set? k)
      true ;; Kind ambiguous as result of attr relation, so simply succeed.
      (do
        (when-not (#{:element :relationship} k)
          (u/errorf "Unknown kind %s." k))
        (cond
         (nil? kind) (do (set! kind k) true)
         (= kind k) true
         :else (u/errorf "Cannot reset kind from %s to %s." kind k)))))
  IType
  (set-type [this t]
    (if (vector? t)
      true ;; Many types given as a result of an attribute relation.  Simply
      ;; succeed.  TODO: We could handle that more restrictively, i.e.,
      ;; the current type must be a subclass or equal to one of the given
      ;; types, and if there's no current type, it must be a subclass or
      ;; equal to one of the given classes in the future.
      (let [mm-class (p/mm-class model (second (u/type-with-modifiers (name t))))]
        (cond
         ;; No type set already.
         (nil? type) (do (set! type mm-class) true)
         ;; The current type is a superclass of mm-class, so set to the more
         ;; specific.
         (p/mm-super-class? type mm-class) (do (set! type mm-class) true)
         ;; The given type is a superclass of or identical to the currently set
         ;; type, so ccl/succeed without changing anything.
         (or (= mm-class type)
             (p/mm-super-class? mm-class type)) true
             :else (u/errorf "Cannot reset type from %s to %s." (p/qname type) t)))))
  IUnset
  (unset? [this attr]
    (nil? (get attrs attr)))
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (get attrs attr)]
      (cond
       (unset? this attr) (do (set! attrs (assoc attrs attr val)) true)
       (= cur val) true
       :else (u/errorf "Cannot reset attribute %s from %s to %s." attr cur val))))
  (finalize-attrs [this subst]
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (add-ref [this ref target]
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (let [cur (get refs ref)]
      (cond
       (q/member? target cur) true
       :else (do
               (set! refs (update-in refs [ref]
                                     #(conj (vec %1) %2)
                                     target))
               true))))
  (finalize-refs [this subst]
    (set! refs (groundify-refs refs subst))
    (single-containers? type refs))
  IAlphaOmega
  (set-alpha [this a]
    ;; a is either fresh or a wrapper or tmp element
    (when-not (= kind :relationship)
      (u/errorf "Can't set alpha of non-edge %s." this))
    (cond
     (= alpha a) true
     (nil? alpha) (set! alpha a)
     :else (u/errorf "Can't reset alpha of %s." this)))
  (set-omega [this o]
    ;; o is either fresh or a wrapper or tmp element
    (when-not (= kind :relationship)
      (u/errorf "Can't set omega of non-edge %s." this))
    (cond
     (= omega o) true
     (nil? omega) (set! omega o)
     :else (u/errorf "Can't reset omega of %s." this)))
  (finalize-alpha-and-omega [this subst]
    (set! alpha (cclp/walk subst alpha))
    (set! omega (cclp/walk subst omega))
    (when (ru/fresh? alpha)
      (u/errorf "Can't groundify alpha of %s." this))
    (when (ru/fresh? omega)
      (u/errorf "Can't groundify omega of %s." this))
    true)
  IManifestation
  (manifest [this]
    (or manifested-element
        (do
          (when-not type
            (u/errorf "Can't manifest: type is nil in %s." this))
          (set! manifested-element (cond
                                    (= kind :element) (p/create-element! model type)
                                    (= kind :relationship) (p/create-relationship!
                                                            model type
                                                            (manifest alpha) (manifest omega))
                                    :else (u/errorf "Unknown kind %s." kind)))
          (doseq [[at val] attrs]
            (p/set-aval! manifested-element at val))
          (doseq [[role rs] refs]
            (doseq [r rs]
              (p/add-adj! manifested-element role r)))
          manifested-element)))
  (manifestation [this] manifested-element))

(defn make-tmp-element
  ([model kind]
     (doto (->TmpElement model nil nil {} {} nil nil nil)
       (set-kind kind)))
  ([model kind type]
     (doto (make-tmp-element model kind)
       (set-type type))))

;;# Helpers

(defn tmp-or-wrapper-element? [el]
  (or (instance? WrapperElement el)
      (instance? TmpElement el)))

(defn tmp-element? [el]
  (instance? TmpElement el))

(defn wrapper-element? [el]
  (instance? WrapperElement el))

(defn groundify-attrs [attrs subst]
  (apply hash-map (mapcat (fn [[a v]]
                            (let [v (if (ccl/lvar? v)
                                      (cclp/walk subst v)
                                      v)]
                              (when (ccl/lvar? v)
                                (u/errorf "Attribute %s can't be grounded." a))
                              [a v]))
                          attrs)))

(defn groundify-refs [refs subst]
  (apply hash-map
         (mapcat (fn [[r vs]]
                   (let [vs (mapv (fn [v]
                                    (let [v (if (ccl/lvar? v)
                                              (cclp/walk subst v)
                                              v)]
                                      (when (ccl/lvar? v)
                                        (u/errorf "Reference %s can't be grounded." r))
                                      v))
                                  vs)]
                     [r vs]))
                 refs)))

(defn single-containers? [type refs]
  (loop [rs refs, ok true]
    (if (seq rs)
      (let [[r ts] (first rs)]
        (if (containment-ref? type r)
          (recur (rest rs) (and ok (funnyqt.query/forall?
                                    #(cond
                                      (wrapper-element? %)
                                      (not (p/container (.wrapped-element ^WrapperElement %)))

                                      (tmp-element? %)
                                      true)
                                    ts)))
          (recur (rest rs) ok)))
      ok)))

;;# Finalization

(defn finalizeo [& els]
  (fn [a]
    (let [all-ok
          (loop [els (filter tmp-or-wrapper-element? (map (partial cclp/walk a) els)), ok true]
            (if (seq els)
              (recur (rest els) (and ok
                                     (finalize-attrs (first els) a)
                                     (finalize-refs  (first els) a)
                                     (finalize-alpha-and-omega (first els) a)))
              ok))]
      (if all-ok
        (ccl/succeed a)
        (ccl/fail a)))))
