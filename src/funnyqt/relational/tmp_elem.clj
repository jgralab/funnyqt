(ns funnyqt.relational.tmp-elem
  "(Internal) Stuff for letting relations create temporary elements."
  (:require [clojure.core.logic :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt
             [generic :as g]
             [query :as q]
             [utils :as u]]))

(def ^:dynamic *make-tmp-elements* false)

;;# Protocols

(defprotocol IAsMap
  (as-map [this]))

(defprotocol IAttr
  (add-attr [this attr val may-override subst])
  (check-attr-validity [this subst])
  (finalize-attrs [this subst]))

(defprotocol IRef
  (get-refs [this])
  (add-ref [this ref target may-override])
  (check-ref-validity [this subst])
  (finalize-refs [this subst]))

(defprotocol IKind
  (set-kind [this k]))

(defprotocol IType
  (set-type [this t])
  (check-type [this]))

(defprotocol IManifestation
  (manifest [this])
  (manifestation [this]))

(defprotocol ISourceTarget
  (set-source [this a])
  (set-target [this o])
  (finalize-alpha-and-omega [this subst]))

;;# Types

(declare wrapper-element?)
(declare tmp-element?)
(declare groundify-attrs)
(declare groundify-refs)
(declare single-containers?)
(declare single-valued-refs-are-single?)

;;## WrapperElement

(deftype WrapperElement [model
                         wrapped-element
                         ^:unsynchronized-mutable attrs
                         ^:unsynchronized-mutable refs
                         ^:unsynchronized-mutable manifested
                         ^:unsynchronized-mutable overridable-properties]
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
                    (g/element? wrapped-element)      :element
                    (g/relationship? wrapped-element) :relationship
                    :else (u/errorf "%s is neither element nor relationship."
                                    wrapped-element))]
          (= k cur)))))
  IType
  (check-type [this] true)
  (set-type [this t]
    (when manifested (u/errorf "Already manifested: %s" this))
    (let [cur-class (g/mm-class wrapped-element)
          super-or-eq-to-cur? #(or (= % cur-class)
                                   (g/mm-superclass? % cur-class))]
      (if (vector? t)
        (q/exists? super-or-eq-to-cur? (map (partial g/mm-class model) t))
        (super-or-eq-to-cur? (g/mm-class model t)))))
  IAttr
  (add-attr [this attr val may-override subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (cond
      may-override
      (do (set! overridable-properties (conj overridable-properties attr))
          (set! attrs (assoc attrs attr val))
          true)
      ;;---
      (g/unset? wrapped-element attr)
      (do (set! attrs (assoc attrs attr val))
          true)
      ;;---
      (let [gval (cclp/walk subst val)
            cval (g/aval wrapped-element attr)]
        (= gval cval))
      true
      ;;---
      :else false))
  (check-attr-validity [this subst]
    (q/forall? (fn [[a v]]
                 (or (overridable-properties a)
                     (g/unset? wrapped-element a)
                     (= (g/aval wrapped-element a) (cclp/walk subst v))))
               attrs))
  (finalize-attrs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (get-refs [this] refs)
  (add-ref [this ref target may-override]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; target is either fresh or a wrapper or tmp element
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (when may-override
      (set! overridable-properties (conj overridable-properties ref)))
    (set! refs (update-in refs [ref]
                          #(set (conj %1 %2))
                          target)))
  (check-ref-validity [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (let [cls (g/mm-class wrapped-element)]
      (and (single-containers? this cls subst)
           (single-valued-refs-are-single? this cls overridable-properties subst))))
  (finalize-refs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! refs (groundify-refs refs subst)))
  ISourceTarget
  (set-source [this a]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; a is either fresh or a wrapper or tmp element
    (when-not (g/relationship? wrapped-element)
      (u/errorf "Can't set source of non-relationship %s." wrapped-element))
    (cond
      (tmp-element? a)     false
      :else                true))
  (set-target [this o]
    (when manifested (u/errorf "Already manifested: %s" this))
    ;; o is either fresh or a wrapper or tmp element
    (when-not (g/relationship? wrapped-element)
      (u/errorf "Can't set target of non-relationship %s." wrapped-element))
    (cond
      (tmp-element? o)     false
      :else                true))
  (finalize-alpha-and-omega [this subst]
    true)
  IManifestation
  (manifest [this]
    (try
      (if manifested
        wrapped-element
        (do
          (set! manifested true)
          (doseq [[at val] attrs]
            (g/set-aval! wrapped-element at val))
          (doseq [[role rs] refs
                  :let [multi-valued (g/mm-multi-valued-property?
                                      (g/mm-class wrapped-element) role)
                        cur-refed (set (g/adjs wrapped-element role))]]
            (doseq [r rs]
              (let [mr (manifest r)]
                (when-not (q/member? mr cur-refed)
                  (if multi-valued
                    (g/add-adj! wrapped-element role (manifest r))
                    (g/set-adj! wrapped-element role (manifest r)))))))
          wrapped-element))
      (catch Exception ex
        (throw (RuntimeException. (format "%s during manifestation of %s."
                                          (.getSimpleName (class ex)) this)
                                  ex)))))
  (manifestation [this] wrapped-element))

(def ^{:dynamic true
       :doc "A cache (atom {}) for WrapperElements.  There is at most only one
  WrapperElement per model object."}
  *wrapper-cache* nil)

(defn make-wrapper [model lvar element]
  (let [cur (get @*wrapper-cache* element)]
    (or cur
        (let [w (->WrapperElement model element {} {} false #{})]
          (swap! *wrapper-cache* assoc element w)
          w))))

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
  (check-type [this]
    (or type (u/errorf "No type defined for %s." this)))
  (set-type [this t]
    (if (vector? t)
      true
      ;; Many types given as a result of an attribute relation.  Simply
      ;; succeed.  TODO: We could handle that more restrictively, i.e.,
      ;; the current type must be a subclass or equal to one of the given
      ;; types, and if there's no current type, it must be a subclass or
      ;; equal to one of the given classes in the future.
      (let [mm-class (g/mm-class model (second (u/type-with-modifiers (name t))))]
        (cond
          ;; No type set already.
          (nil? type) (do (set! type mm-class) true)
          ;; The given type is a superclass of or identical to the currently set
          ;; type, so ccl/succeed without changing anything.
          (or (= mm-class type)
              (g/mm-superclass? mm-class type)) true
          ;; The current type is a superclass of mm-class, so set to the more
          ;; specific.
          (g/mm-superclass? type mm-class) (do (set! type mm-class) true)
          :else (u/errorf "Cannot reset type from %s to %s." (g/qname type) t)))))
  IAttr
  (add-attr [this attr val _ _] ;; The may-override param is ignored for new elements.
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (cond
      (nil? (get attrs attr)) (do (set! attrs (assoc attrs attr val)) true)
      (= (get attrs attr) val) true
      :else (u/errorf "Cannot reset attribute %s from %s to %s."
                      attr (get attrs attr) val)))
  (check-attr-validity [this subst]
    true)
  (finalize-attrs [this subst]
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (get-refs [this] refs)
  (add-ref [this ref target _]
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (set! refs  (update-in refs [ref]
                           #(set (conj %1 %2))
                           target))
    true)
  (check-ref-validity [this subst]
    (when manifested-element (u/errorf "Already manifested: %s" this))
    (and (single-containers? this type subst)
         (single-valued-refs-are-single? this type nil subst)))
  (finalize-refs [this subst]
    (set! refs (groundify-refs refs subst)))
  ISourceTarget
  (set-source [this a]
    ;; a is either fresh or a wrapper or tmp element
    (when-not (= kind :relationship)
      (u/errorf "Can't set alpha of non-edge %s." this))
    (cond
      (= alpha a) true
      (nil? alpha) (set! alpha a)
      :else (u/errorf "Can't reset alpha of %s." this)))
  (set-target [this o]
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
    (when (ccl/lvar? alpha)
      (u/errorf "Can't groundify alpha of %s." this))
    (when (ccl/lvar? omega)
      (u/errorf "Can't groundify omega of %s." this))
    true)
  IManifestation
  (manifest [this]
    (try
      (or manifested-element
          (do
            (when-not type
              (u/errorf "Can't manifest: type is nil in %s." this))
            (set! manifested-element (cond
                                       (= kind :element) (g/create-element! model type)
                                       (= kind :relationship) (g/create-relationship!
                                                               model type
                                                               (manifest alpha)
                                                               (manifest omega))
                                       :else (u/errorf "Unknown kind %s." kind)))
            (doseq [[at val] attrs]
              (g/set-aval! manifested-element at val))
            (doseq [[role rs] refs
                    :let [multi-valued (g/mm-multi-valued-property?
                                        (g/mm-class manifested-element) role)]]
              (doseq [r rs]
                (let [mr (manifest r)]
                  (if multi-valued
                    (g/add-adj! manifested-element role mr)
                    (g/set-adj! manifested-element role mr)))))
            manifested-element))
      (catch Exception ex
        (throw (RuntimeException. (format "%s during manifestation of %s."
                                          (.getSimpleName (class ex)) this)
                                  ex)))))
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

(defn groundify-val [subst v]
  (let [v (if (ccl/lvar? v)
            (cclp/walk subst v)
            v)]
    (when (ccl/lvar? v)
      (u/errorf "%s can't be grounded with substitution %s." v subst))
    v))

(defn groundify-attrs [attrs subst]
  (into {} (map (fn [[a v]]
                  [a (groundify-val subst v)]))
        attrs))

(defn groundify-refs [refs subst]
  (into {}
        (map (fn [[r vs]]
               (let [vs (mapv (partial groundify-val subst) vs)]
                 [r vs])))
        refs))

(defn ref-checker [el type subst ref-selector-fn predicate]
  ;; el: the tmp or wrapper element having refs
  ;; type: el's type
  ;; subst: the current substitution
  (loop [rs (get-refs el), ok true]
    (if (and ok (seq rs))
      (let [[r ts] (first rs)]
        (if (ref-selector-fn type r)
          (recur (rest rs) (q/forall? #(predicate r %) ts))
          (recur (rest rs) ok)))
      ok)))

(defn single-containers? [el type subst]
  (ref-checker el type subst g/mm-containment-reference?
               (fn [ref target]
                 (let [target (cclp/walk subst target)]
                   (cond
                     (wrapper-element? target)
                     (let [container (g/container (.wrapped-element ^WrapperElement target))]
                       (or (nil? container)
                           (and (wrapper-element? el)
                                (identical? (.wrapped-element ^WrapperElement el)
                                            container))))
                     (tmp-element? target)
                     true
                     :else (u/error "Shouldn't happen!"))))))

(defn single-valued-refs-are-single? [el type overridable-props subst]
  (ref-checker el type subst (complement g/mm-multi-valued-property?)
               (fn [ref target]
                 (let [target (cclp/walk subst target)]
                   (cond
                     (wrapper-element? el)
                     (let [cur (g/adj (.wrapped-element ^WrapperElement el) ref)]
                       (or (nil? cur)
                           (and (wrapper-element? target)
                                (identical? (.wrapped-element ^WrapperElement target)
                                            cur))
                           (and overridable-props (overridable-props ref))))
                     (tmp-element? el)
                     (== 1 (count (set (map #(cclp/walk subst %)
                                            (get (get-refs el) ref)))))
                     :else (u/error "Shouldn't happen!"))))))

;;# Finalization

(defn finalizeo [& els]
  (fn [a]
    (let [tw-els (vec (filter tmp-or-wrapper-element? (map (partial cclp/walk a) els)))]
      (if (q/forall? #(and (check-type %)
                           (check-attr-validity % a)
                           (check-ref-validity % a))
                     tw-els)
        (do
          (doseq [el tw-els]
            (finalize-attrs el a)
            (finalize-refs  el a)
            (finalize-alpha-and-omega el a))
          (ccl/succeed a))
        (do
          #_(println (format "invalid: %s" (vec (map (partial cclp/walk a) els))))
          (ccl/fail a))))))
