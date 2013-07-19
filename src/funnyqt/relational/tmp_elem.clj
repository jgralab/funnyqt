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
  (add-attr [this attr val]))

(defprotocol IRef
  (add-ref [this ref target]))

(defprotocol IKind
  (set-kind [this k]))

(defprotocol IType
  (set-type [this t]))

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
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (let [cur (p/aval wrapped-element attr)]
      (cond
       (= cur val) true
       (nil? cur) (do (set! attrs (assoc attrs attr val)) true)
       :else false)))
  IRef
  (add-ref [this ref target]
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (let [cur (q/adjs wrapped-element ref)]
      (cond
       (q/member? target cur) true
       :else (do
               (set! refs (update-in refs [ref]
                                     #(conj (vec %1) %2)
                                     target))
               true)))))

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
     :else (u/errorf "Cannot reset kind from %s to %s." kind k))))

(comment
  (run* [q]
    (vertexo g v1)
    (typeo   g v1 'County)
    (valueo  g v1 :name "Hessen")
    (adjo    g v1 :localities v2)
    (vertexo g v2)
    (typeo   g v2 'City)
    (valueo  g v2 :name "Frankfurt")))
