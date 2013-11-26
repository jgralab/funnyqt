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
  (get-refs [this])
  (add-ref [this ref target])
  (check-validity [this subst])
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
(declare single-valued-refs-are-single?)

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
  p/IUnset
  (p/unset? [this attr]
    (p/unset? wrapped-element attr))
  IAttr
  (add-attr [this attr val]
    (when manifested (u/errorf "Already manifested: %s" this))
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (cond
     (p/unset? this attr) (do (set! attrs (assoc attrs attr val)) true)
     (= (p/aval wrapped-element attr) val) true
     :else false))
  (finalize-attrs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (get-refs [this] refs)
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
                                     #(conj (set %1) %2)
                                     target))
               true))))
  (check-validity [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (let [cls (p/mm-class wrapped-element)]
      (and (single-containers? this cls subst)
           (single-valued-refs-are-single? this cls subst))))
  (finalize-refs [this subst]
    (when manifested (u/errorf "Already manifested: %s" this))
    (set! refs (groundify-refs refs subst)))
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
    (try
      (if manifested
        wrapped-element
        (do
          (set! manifested true)
          (doseq [[at val] attrs]
            (p/set-aval! wrapped-element at val))
          (doseq [[role rs] refs
                  :let [multi-valued (p/mm-multi-valued-property?
                                      (p/mm-class wrapped-element) role)]]
            (doseq [r rs]
              (let [mr (manifest r)]
                (when-not (q/member? mr (q/adjs wrapped-element role))
                  (if multi-valued
                    (p/add-adj! wrapped-element role (manifest r))
                    (p/set-adj! wrapped-element role (manifest r)))))))
          wrapped-element))
      (catch Exception ex
        (throw (RuntimeException. (format "%s during manifestation of %s."
                                          (.getSimpleName (class ex)) this)
                                  ex)))))
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
         ;; The given type is a superclass of or identical to the currently set
         ;; type, so ccl/succeed without changing anything.
         (or (= mm-class type)
             (p/mm-super-class? mm-class type)) true
         ;; The current type is a superclass of mm-class, so set to the more
         ;; specific.
         (p/mm-super-class? type mm-class) (do (set! type mm-class) true)
         :else (u/errorf "Cannot reset type from %s to %s." (p/qname type) t)))))
  p/IUnset
  (p/unset? [this attr]
    (nil? (get attrs attr)))
  IAttr
  (add-attr [this attr val]
    (when-not (keyword? attr)
      (u/errorf "attr must be given as keyword but was %s." attr))
    (cond
     (p/unset? this attr) (do (set! attrs (assoc attrs attr val)) true)
     (= (get attrs attr) val) true
     :else (u/errorf "Cannot reset attribute %s from %s to %s."
                     attr (get attrs attr) val)))
  (finalize-attrs [this subst]
    (set! attrs (groundify-attrs attrs subst))
    true)
  IRef
  (get-refs [this] refs)
  (add-ref [this ref target]
    (when-not (keyword? ref)
      (u/errorf "ref must be given as keyword but was %s." ref))
    (let [cur (get refs ref)]
      (cond
       (q/member? target cur) true
       :else (do
               (set! refs (update-in refs [ref]
                                     #(conj (set %1) %2)
                                     target))
               true))))
  (check-validity [this subst]
    (and (single-containers? this type subst)
         (single-valued-refs-are-single? this type subst)))
  (finalize-refs [this subst]
    (set! refs (groundify-refs refs subst)))
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
    (try
      (or manifested-element
          (do
            (when-not type
              (u/errorf "Can't manifest: type is nil in %s." this))
            (set! manifested-element (cond
                                      (= kind :element) (p/create-element! model type)
                                      (= kind :relationship) (p/create-relationship!
                                                              model type
                                                              (manifest alpha)
                                                              (manifest omega))
                                      :else (u/errorf "Unknown kind %s." kind)))
            (doseq [[at val] attrs]
              (p/set-aval! manifested-element at val))
            (doseq [[role rs] refs
                    :let [multi-valued (p/mm-multi-valued-property?
                                        (p/mm-class manifested-element) role)]]
              (doseq [r rs]
                (let [mr (manifest r)]
                  (if multi-valued
                    (p/add-adj! manifested-element role mr)
                    (p/set-adj! manifested-element role mr)))))
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

(defn ref-checker [el type subst ref-selector-fn predicate]
  ;; el: the tmp or wrapper element having refs
  ;; type: el's type
  ;; subst: the current substitution
  (loop [rs (get-refs el), ok true]
    (if (and ok (seq rs))
      (let [[r ts] (first rs)]
        (if (ref-selector-fn type r)
          (recur (rest rs) (funnyqt.query/forall? #(predicate r %) ts))
          (recur (rest rs) ok)))
      ok)))

(defn single-containers? [el type subst]
  (ref-checker el type subst p/mm-containment-ref?
               (fn [ref target]
                 (let [target (cclp/walk subst target)]
                   (cond
                    (wrapper-element? target)
                    (let [container (p/container (.wrapped-element ^WrapperElement target))]
                      (or (nil? container)
                          (and (wrapper-element? el)
                               (identical? (.wrapped-element ^WrapperElement el)
                                           container))))
                    (tmp-element? target)
                    true)))))

(defn single-valued-refs-are-single? [el type subst]
  (ref-checker el type subst (complement p/mm-multi-valued-property?)
               (fn [ref target]
                 (let [refed (cclp/walk subst target)]
                   (cond
                    (wrapper-element? el)
                    (let [cur (q/adj (.wrapped-element ^WrapperElement el) ref)]
                      (or (nil? cur)
                          (and (wrapper-element? refed)
                               (identical? (.wrapped-element ^WrapperElement refed)
                                           cur))))
                    (tmp-element? el)
                    (= 1 (count (set (map #(cclp/walk subst %)
                                          (get (get-refs el) ref))))))))))

;;# Finalization

(defn finalizeo [& els]
  (fn [a]
    (let [tw-els (vec (filter tmp-or-wrapper-element? (map (partial cclp/walk a) els)))]
      (if (q/forall? #(check-validity % a) tw-els)
        (do
          (doseq [el tw-els]
            (finalize-attrs el a)
            (finalize-refs  el a)
            (finalize-alpha-and-omega el a))
          (ccl/succeed a))
        (do
          #_(u/errorf "invalid: %s" (vec (map (partial cclp/walk a) els)))
          (ccl/fail a))))))
