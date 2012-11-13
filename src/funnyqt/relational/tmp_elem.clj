(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.utils :as u])
  (:use funnyqt.relational.util))

(def ^:dynamic *make-tmp-elements* nil)

(defprotocol TmpElementAccessors
  (get-graph [this])
  (get-kind  [this])
  (get-type  [this])
  (get-alpha [this])
  (get-omega [this])
  (get-attrs [this]))

(defprotocol TmpAEOps
  (set-tmp-kind [this kind])
  (set-tmp-type [this type])
  (add-tmp-attr [this attr val])
  (manifest [this])
  (as-map [this]))

(defprotocol TmpEdgeOps
  (set-tmp-alpha [this al])
  (set-tmp-omega [this om]))

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
                      (tg/create-vertex! graph type))
        :edge   (set! manifestation
                      (tg/create-edge! graph type
                                       (manifest alpha)
                                       (manifest omega)))))
    (doseq [[a val] attrs]
      (tg/set-value! manifestation a val))
    (set! manifested true)
    manifestation)
  (set-tmp-kind [this k]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (when (and kind (not= kind k))
      (u/errorf "Cannot reset kind %s to %s." kind k))
    (set! kind k))
  (set-tmp-type [this t]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (when (and type (not= type t))
      (u/errorf "Cannot reset type %s to %s." type t))
    (when (not (symbol? type))
      (u/errorf "Type must be a plain type: %s" type))
    (let [^String n (name type)]
      (when (or (.startsWith n "!")
                (.endsWith n "!"))
        (u/errorf "Type must be a plain type (no !): %s" type)))
    (set! type t))
  (add-tmp-attr [this attr val]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (let [v (get attrs attr ::not-found)]
      (cond
       (= v ::not-found) (set! attrs (assoc attrs attr val))
       (= v val) val
       :else (u/errorf "Cannot reset %s value from %s to %s." attr v val))))
  (as-map [this]
    {:kind kind :type type :alpha alpha :omega omega :attrs attrs
     :manifested manifested :manifestation manifestation})
  TmpEdgeOps
  (set-tmp-alpha [this al]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (if (and alpha (not= alpha al))
      (u/errorf "The alpha vertex is already set to %s. Cannot reset to %s." alpha al)
      (set! alpha al)))
  (set-tmp-omega [this om]
    (when manifested
      (u/errorf "Cannot modify a manifested element!"))
    (if (and omega (not= omega om))
      (u/errorf "The omega vertex is already set to %s. Cannot reset to %s." omega om)
      (set! omega om))))

(defn make-tmp-element
  ([g]
     (make-tmp-element g nil))
  ([g type]
     (->TmpElement g nil type nil nil {} false nil)))

(defn make-tmp-vertex [g]
  (->TmpElement g :vertex nil nil nil {} false nil))

(defn make-tmp-edge [g]
  (->TmpElement g :edge nil nil nil {} false nil))

(defn tmp-element? [elem]
  (instance? funnyqt.relational.tmp_elem.TmpElement elem))

(defn tmp-vertex? [elem]
  (and (tmp-element? elem)
       (= (get-kind elem) :vertex)))

(defn tmp-edge? [elem]
  (and (tmp-element? elem)
       (= (get-kind elem) :edge)))

