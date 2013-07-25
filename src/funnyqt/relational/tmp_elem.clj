(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.protocols :as p]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp])
  (:use funnyqt.relational.util))

(def ^:dynamic *make-tmp-elements* false)

;; TODO: Add Unset protocol with unset? function instead of using nil? checks
;; for attributes.

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
  (set-omega [this o]))

(defprotocol ContainmentRef
  (containment-ref? [mm-class ref-kw]))

(extend-protocol ContainmentRef
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

;;# Types

(declare wrapper-element?)
(declare tmp-element?)
(declare groundify-attrs)
(declare groundify-refs)
(declare single-containers?)

;;## WrapperElement

(deftype WrapperElement [model
                         wrapped-element
                         ^:unsynchronized-mutable attrs
                         ^:unsynchronized-mutable refs
                         ^:unsynchronized-mutable manifested]
  Object
  (hashCode [this]
    (.hashCode wrapped-element))
  (equals [this that]
    (and (wrapper-element? that)
         (= wrapped-element (.wrapped-element that))))
  IAsMap
  (as-map [this]
    {:model model :wrapped-element wrapped-element
     :attrs attrs :refs refs :manifested manifested})
  IKind
  (set-kind [this k]
    (if (set? k)
      true ;; Kind ambiguous as result of attr relation, so simply succeed.
      (let [cur (condp instance? wrapped-element
                  de.uni_koblenz.jgralab.Vertex :vertex
                  de.uni_koblenz.jgralab.Edge   :edge
                  org.eclipse.emf.ecore.EObject :eobject)]
        (= k cur))))
  IType
  (set-type [this t]
    (let [cur-class (p/mm-class wrapped-element)
          super-or-eq-to-cur? #(or (= % cur-class)
                                   (p/mm-super-class? % cur-class))]
      (if (vector? t)
        (q/exists? super-or-eq-to-cur? (map (partial p/mm-class model) t))
        (super-or-eq-to-cur? (p/mm-class model t)))))
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (p/aval wrapped-element attr)]
      (cond
       (= cur val) true
       (nil? cur) (do (set! attrs (assoc attrs attr val)) true)
       :else false)))
  (finalize-attrs [this subst]
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (add-ref [this ref target]
    ;; target is either fresh or a wrapper or tmp element
    (when (instance? de.uni_koblenz.jgralab.Edge wrapped-element)
      (u/errorf "Can't add ref to an edge."))
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
    (set! refs (groundify-refs refs subst))
    (single-containers? (p/mm-class wrapped-element) refs))
  IAlphaOmega
  (set-alpha [this a]
    ;; a is either fresh or a wrapper or tmp element
    (when-not (instance? de.uni_koblenz.jgralab.Edge wrapped-element)
      (u/errorf "Can't set alpha of non-edge %s." wrapped-element))
    (cond
     (wrapper-element? a) (= (tg/alpha wrapped-element) (.wrapped-element a))
     (tmp-element? a)     false
     :else                true))
  (set-omega [this o]
    ;; o is either fresh or a wrapper or tmp element
    (when-not (instance? de.uni_koblenz.jgralab.Edge wrapped-element)
      (u/errorf "Can't set omega of non-edge %s." wrapped-element))
    (cond
     (wrapper-element? o) (= (tg/omega wrapped-element) (.wrapped-element ^WrapperElement o))
     (tmp-element? o)     false
     :else                true))
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

(defn make-wrapper [model element]
  (->WrapperElement model element {} {} false))

;;## TmpElement

(deftype TmpElement [model
                     ^:unsynchronized-mutable kind
                     ^:unsynchronized-mutable type
                     ^:unsynchronized-mutable attrs
                     ^:unsynchronized-mutable refs
                     ^:unsynchronized-mutable alpha
                     ^:unsynchronized-mutable omega
                     ^:unsynchronized-mutable manifested-element]
  IAsMap
  (as-map [this]
    {:model model :kind kind :type type :attrs attrs :refs refs
     :manifested-element manifested-element})
  IKind
  (set-kind [this k]
    (if (set? k)
      true ;; Kind ambiguous as result of attr relation, so simply succeed.
      (cond
       (nil? kind) (do (set! kind k) true)
       (= kind k) true
       :else (u/errorf "Cannot reset kind from %s to %s." kind k))))
  IType
  (set-type [this t]
    (if (vector? t)
      true ;; Many types given as a result of an attribute relation.  Simply
           ;; succeed.  TODO: We could handle that more restrictively, i.e.,
           ;; the current type must be a subclass or equal to one of the given
           ;; types, and if there's no current type, it must be a subclass or
           ;; equal to one of the given classes in the future.
      (let [mm-class (p/mm-class model t)]
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
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (get attrs attr)]
      (cond
       (nil? cur) (do (set! attrs (assoc attrs attr val)) true)
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
    (when-not (= kind :edge)
      (u/errorf "Can't set alpha of non-edge %s." this))
    (cond
     (= alpha a) true
     (nil? alpha) (set! alpha a)
     :else (u/errorf "Can't reset alpha of %s." this)))
  (set-omega [this o]
    ;; o is either fresh or a wrapper or tmp element
    (when-not (= kind :edge)
      (u/errorf "Can't set omega of non-edge %s." this))
    (cond
     (= omega o) true
     (nil? omega) (set! omega o)
     :else (u/errorf "Can't reset omega of %s." this)))
  IManifestation
  (manifest [this]
    (or manifested-element
        (do
          (set! manifested-element (p/create-element! model type))
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
                                      (instance? WrapperElement %)
                                      (not (p/container (.wrapped-element %)))

                                      (instance? TmpElement %)
                                      true)
                                    ts)))
          (recur (rest rs) ok)))
      ok)))

;;# Finalization

(defn finalizeo [& els]
  (fn [a]
    (let [all-ok
          (loop [els (filter tmp-or-wrapper-element? els), ok true]
            (if (and ok (seq els))
              (recur (rest els) (and ok
                                     (finalize-attrs (first els) a)
                                     (finalize-refs  (first els) a)))
              ok))]
      (if all-ok
        (ccl/succeed a)
        (ccl/fail a)))))

(comment
  (run* [q]
    (vertexo g v1)
    (typeo   g v1 'County)
    (valueo  g v1 :name "Hessen")
    (adjo    g v1 :localities v2)
    (vertexo g v2)
    (typeo   g v2 'City)
    (valueo  g v2 :name "Frankfurt")))
