(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.query :as q]
            [funnyqt.utils :as u]
            [funnyqt.protocols :as p]
            [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp])
  (:use funnyqt.relational.util)
  (:import (de.uni_koblenz.jgralab.schema GraphElementClass AggregationKind EdgeClass)))

(def ^:dynamic *make-tmp-elements* false)

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
  (finalize-attrs [this subst])
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
  java.lang.Comparable
  (compareTo [this that]
    (if (= this that)
      0
      (let [hd (- (hash this) (hash that))]
        (if (zero? hd)
          (u/errorf "Compare to zero but are different: %s, %s"
                    this that)
          hd))))
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
      (set! manifestation (condp = kind
                            :vertex (tg/create-vertex! graph type)
                            :edge (do
                                    (when-not (tg/vertex? alpha)
                                      (set! alpha (manifest alpha)))
                                    (when-not (tg/vertex? omega)
                                      (set! omega (manifest omega)))
                                    (tg/create-edge! graph type alpha omega))))
      (set! manifested true)
      (doseq [[a val] attrs]
        (tg/set-value! manifestation a val)))
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
      #_(println "Set type of" this "to" aec)
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
  (finalize-attrs [this subst]
    (set! attrs (apply hash-map (mapcat (fn [[a v]]
                                          [a (if (ccl/lvar? v)
                                               (cclp/walk subst v)
                                               v)])
                                        attrs))))
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

(defn finalize-tmp-elems [& args]
  (fn [a]
    (doseq [te (filter tmp-element?
                       (map (partial cclp/walk a)
                            args))]
      (finalize-attrs te a))
    a))

(defn valid-match? [match]
  (let [tmp-edges (filter tmp-edge? (vals match))
        contained-vertices (loop [tes tmp-edges, cm {}]
                             ;; cm is {vertex compoEdgeContainingVertex}
                             (if (seq tes)
                               (let [edge (first tes)
                                     ^EdgeClass ec (get-type edge)]
                                 (cond
                                  (= (-> ec .getFrom .getAggregationKind)
                                     AggregationKind/COMPOSITE)
                                  (let [al (get-alpha edge)]
                                    (if (cm al)
                                      false
                                      (recur (rest tes) (assoc cm al edge))))
                                  ;;;------
                                  (= (-> ec .getTo .getAggregationKind)
                                     AggregationKind/COMPOSITE)
                                  (let [om (get-omega edge)]
                                    (if (cm om)
                                      false
                                      (recur (rest tes) (assoc cm om edge))))))
                               (keys cm)))]
    (if (seq contained-vertices)
      (every? (fn [v]
                (or (tmp-vertex? v)
                    (nil? (tg/container v))))
              contained-vertices)
      true)))
