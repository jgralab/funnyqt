(ns funnyqt.relational.tmp-elem
  (:require [funnyqt.tg :as tg]
            [funnyqt.utils :as u]))

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
  TmpAEOps
  (manifest [this]
    ;; TODO: Note that manifestation can already exist.
    (when-not manifested
      (when-not kind
        (u/errorf "TmpElement kind not set!"))
      (when-not type
        (u/errorf "TmpElement type not set!"))
      (condp = kind
        :vertex (let [v (tg/create-vertex! graph type)]
                  (doseq [[a val] attrs]
                    (tg/set-value! v a val))
                  (set! manifested true)
                  (set! manifestation v))
        :edge (do
                (when-not (tg/vertex? alpha)
                  (set! alpha (manifest alpha)))
                (when-not (tg/vertex? omega)
                  (set! omega (manifest omega)))
                (let [e (tg/create-edge! graph type alpha omega)]
                  (doseq [[a val] attrs]
                    (tg/set-value! e a val))
                  (set! manifested true)
                  (set! manifestation e)))))
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

(defn make-tmp-element [g]
  (->TmpElement g nil nil nil nil {} false nil))

(defn make-tmp-vertex [g type]
  (let [^String n (name type)]
    (when (or (.startsWith n "!")
              (.endsWith n "!"))
      (u/errorf "Type must be a plain type (no !): %s" type)))
  (->TmpElement g :vertex type nil nil {} false nil))

(defn make-tmp-edge [g type]
  (let [^String n (name type)]
    (when (or (.startsWith n "!")
              (.endsWith n "!"))
      (u/errorf "Type must be a plain type (no !): %s" type)))
  (->TmpElement g :edge type nil nil {} false nil))

(defn tmp-element? [elem]
  (instance? funnyqt.relational.tg.TmpElement elem))
