(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.protocols :as p])
  (:use funnyqt.relational.util)
  (:import (de.uni_koblenz.jgralab.schema GraphElementClass)))

(def ^:dynamic *make-tmp-elements* nil)

(defprotocol TmpElementAccessors
  (get-graph [this])
  (get-kind  [this])
  (get-type  [this])
  (get-alpha [this])
  (get-omega [this])
  (get-attrs [this]))

(defprotocol TmpAEOps
  (set-kind [this kind])
  (set-type [this type])
  (add-attr [this attr val])
  (manifest [this])
  (as-map [this]))

(defprotocol TmpEdgeOps
  (set-alpha [this al])
  (set-omega [this om]))

(deftype TmpElement [graph
                     ^:volatile-mutable kind
                     ^:volatile-mutable type
                     ^:volatile-mutable alpha
                     ^:volatile-mutable omega
                     ^:volatile-mutable attrs
                     ^:volatile-mutable manifested
                     ^:volatile-mutable manifestation]
  TmpElementAccessors
  (get-graph [this] graph)
  (get-kind  [this] kind)
  (get-type  [this] type)
  (get-alpha [this] alpha)
  (get-omega [this] omega)
  (get-attrs [this] attrs)
  TmpAEOps
  (manifest [this]
    ;; TODO: Note that manifestation can already exist.
    (when-not manifested
      (when-not kind
        (u/errorf "TmpElement kind not set!"))
      (when-not type
        (u/errorf "TmpElement type not set!"))
      (condp = kind
        :vertex (set! manifestation
                      (tg/create-vertex! graph (p/qname type)))
        :edge   (set! manifestation
                      (tg/create-edge! graph (p/qname type)
                                       (manifest alpha)
                                       (manifest omega)))))
    (doseq [[a val] attrs]
      (tg/set-value! manifestation a val))
    (set! manifested true)
    manifestation)
  (set-kind [this k]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (when (and kind (not= kind k))
      (u/errorf "Cannot reset kind %s to %s." kind k))
    (set! kind k)
    true)
  (set-type [this t]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (when-not (symbol? t)
      (u/errorf "Type must be a plain type symbol: %s" t))
    (let [^String n (name t)]
      (when (or (.startsWith n "!")
                (.endsWith n "!"))
        (u/errorf "Type must be a plain type symbol (no !): %s" t)))
    (let [aec (tg/attributed-element-class graph t)]
      (if (nil? type)
        (do
          (set-kind this (if (tg/vertex-class? aec) :vertex :edge))
          (set! type aec))
        (when-not (= aec type)
          (when (q/xor (tg/vertex-class? aec) (tg/vertex-class? type))
            (u/errorf "Cannot chang from VC to EC or vice versa: %s vs. %s"
                      type aec))
          (when (.isSubClassOf ^GraphElementClass aec type)
            (set! type aec)))))
    true)
  (add-attr [this attr val]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (let [v (get attrs attr ::not-found)]
      (cond
       (= v ::not-found) (set! attrs (assoc attrs attr val))
       (= v val) val
       :else (u/errorf "Cannot reset %s value from %s to %s." attr v val)))
    true)
  (as-map [this]
    {:kind kind :type type :alpha alpha :omega omega :attrs attrs
     :manifested manifested :manifestation manifestation})
  TmpEdgeOps
  (set-alpha [this al]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (if (and alpha (not= alpha al))
      (u/errorf "The alpha vertex is already set to %s. Cannot reset to %s." alpha al)
      (set! alpha al))
    true)
  (set-omega [this om]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (if (and omega (not= omega om))
      (u/errorf "The omega vertex is already set to %s. Cannot reset to %s." omega om)
      (set! omega om))
    true))

(defn make-tmp-element
  ([g]
     (make-tmp-element g nil))
  ([g type]
     (let [e (->TmpElement g nil nil nil nil {} false nil)]
       (when type (set-type e type))
       e)))

(defn make-tmp-vertex
  ([g]
     (make-tmp-vertex g nil))
  ([g type]
     (let [e (->TmpElement g :vertex nil nil nil {} false nil)]
       (when type (set-type e type))
       e)))

(defn make-tmp-edge
  ([g]
     (make-tmp-edge g nil))
  ([g type]
     (let [e (->TmpElement g :edge nil nil nil {} false nil)]
       (when type (set-type e type))
       e)))

(defn tmp-element? [elem]
  (instance? funnyqt.relational.tmp_elem.TmpElement elem))

(defn tmp-vertex? [elem]
  (and (tmp-element? elem)
       (= (get-kind elem) :vertex)))

(defn tmp-edge? [elem]
  (and (tmp-element? elem)
       (= (get-kind elem) :edge)))

