(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.protocols :as p]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp])
  (:use funnyqt.relational.util))

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

;;# Containment check protocol

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

;;# Helpers

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
                                        (u/errorf "Reference %s can't be grounded." r))))
                                  vs)]
                     [r vs]))
                 refs)))

(defn enforce-single-containers [refs subst]
  (doseq [[r ts] refs]
    (when (containment-ref? type r)
      (doseq [t ts]
        (when (p/container t)
          (ccl/fail subst))))))

;;# Types

;;## WrapperElement

(deftype WrapperElement [model
                         wrapped-element
                         ^:unsynchronized-mutable attrs
                         ^:unsynchronized-mutable refs]
  IAsMap
  (as-map [this]
    {:model model :wrapped-element wrapped-element
     :attrs attrs :refs refs})
  IKind
  (set-kind [this k]
    (when-not (#{:vertex :edge :eobject} k)
      (u/errorf "kind must be any of :vertex, :edge, :eobject but was %s." k))
    (let [cur (condp instance? wrapped-element
                de.uni_koblenz.jgralab.Vertex :vertex
                de.uni_koblenz.jgralab.Edge   :edge
                org.eclipse.emf.ecore.EObject :eobject)]
      (= k cur)))
  IType
  (set-type [this t]
    (when-not (symbol? t)
      (u/errorf "type must be a symbol but was %s." t))
    (let [mm-class (p/mm-class model t)
          cur-class (p/mm-class wrapped-element)]
      (or (= mm-class cur-class)
          (p/mm-super-class? mm-class cur-class))))
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (p/aval wrapped-element attr)]
      (cond
       (= cur val) true
       (nil? cur) (do (set! attrs (assoc attrs attr val)) true)
       :else (u/errorf "Cannot reset attribute %s from %s to %s." attr cur val))))
  (finalize-attrs [this subst]
    (set! attrs (groundify-attrs attrs subst)))
  IRef
  (add-ref [this ref target]
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (let [cur (q/adjs wrapped-element ref)] ;; Throws if ref is no valid role name
      (cond
       (q/member? target cur) true
       :else (do
               (set! refs (update-in refs [ref]
                                     #(conj (vec %1) %2)
                                     target))
               true))))
  (finalize-refs [this subst]
    (groundify-refs refs subst)
    (enforce-single-containers refs subst)))

(defn make-wrapper [model element]
  (->WrapperElement model element {} {}))

;;## TmpElement

(deftype TmpElement [model
                     ^:unsynchronized-mutable kind
                     ^:unsynchronized-mutable type
                     ^:unsynchronized-mutable attrs
                     ^:unsynchronized-mutable refs]
  IAsMap
  (as-map [this]
    {:model model :kind kind :type type :attrs attrs :refs refs})
  IKind
  (set-kind [this k]
    (when-not (#{:vertex :edge :eobject} k)
      (u/errorf "kind must be any of :vertex, :edge, :eobject but was %s." k))
    (cond
     (nil? kind) (do (set! kind k) true)
     (= kind k) true
     :else (u/errorf "Cannot reset kind from %s to %s." kind k)))
  IType
  (set-type [this t]
    (when-not (symbol? t)
      (u/errorf "type must be a symbol but was %s." t))
    (let [mm-class (p/mm-class model t)]
      (cond
       (nil? type) (do (set! type mm-class) true)
       ;; The current type is a superclass of mm-class, so set to the more
       ;; specific.
       (p/mm-super-class? type mm-class) (do (set! type mm-class) true)
       ;; The given type is a superclass of or identical to the currently set
       ;; type, so succeed without changing anything.
       (or (= mm-class type)
           (p/mm-super-class? mm-class type)) true
       :else (u/errorf "Cannot reset type from %s to %s." (p/qname type) t))))
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
    (set! attrs (groundify-attrs attrs subst)))
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
    (groundify-refs refs subst)
    (enforce-single-containers refs subst)))

(defn make-tmp-element
  ([model kind]
     (doto (->TmpElement model nil nil {} {})
       (set-kind kind)))
  ([model kind type]
     (doto (make-tmp-element model kind)
       (set-type type))))

;;# Finalization

(defn finalizeo [& tmps]
  (fn [a]
    (doseq [tmp tmps]
      (finalize-attrs tmp a)
      (finalize-refs  tmp a))))

(comment
  (run* [q]
    (vertexo g v1)
    (typeo   g v1 'County)
    (valueo  g v1 :name "Hessen")
    (adjo    g v1 :localities v2)
    (vertexo g v2)
    (typeo   g v2 'City)
    (valueo  g v2 :name "Frankfurt")))
