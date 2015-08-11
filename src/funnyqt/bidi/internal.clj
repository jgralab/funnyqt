(ns funnyqt.bidi.internal
  "Internals needed by the bidi implementation."
  (:require [clojure.core.logic :as ccl]
            [clojure.tools.macro :as tm]
            [clojure.walk :as cw]
            [flatland.ordered.map :as om]
            [funnyqt
             [generic :as g]
             [utils :as u]]
            [funnyqt.relational
             [tmp-elem :as tmp]
             [util :as ru]]))

(defn select-match
  "Only for internal use.
  Simply returns the first match.  Throws an exception if there's none."
  [matches relation src-match]
  (when-not (seq matches)
    (u/errorf "Couldn't create a %s target match for source match: %s"
              (.getSimpleName (class relation)) src-match))
  (first matches))

(defn enforce-match
  "Only for internal use.
  Manifests the temporary and wrapper elements in `match`."
  [match]
  (doseq [el (vals match)
          :when (tmp/tmp-or-wrapper-element? el)]
    ;;(println "Enforcing" (tmp/as-map el))
    (tmp/manifest el)))

(defn replace-tmps-and-wrappers-with-manifestations
  "Only for internal use."
  [trg-match]
  (apply hash-map
         (mapcat (fn [[k v]]
                   [k (if (tmp/tmp-or-wrapper-element? v)
                        (tmp/manifestation v)
                        v)])
                 trg-match)))

(defn src-initializeo
  "Only for internal use."
  [args-map & lvars]
  (fn [a]
    (ccl/unify a (vec lvars)
               (mapv (fn [lv]
                       (let [val (get args-map (keyword (:oname lv)) ::unknown)]
                         (if (= val ::unknown) lv val)))
                     lvars))))

(defn ^:private maybe-wrap
  "Wraps `val` in bound to the logic variable `lv` in a WrapperElement if it is
  a model object.  Else returns `val` unchanged.
  Only for internal use."
  [target-model lv val]
  (if (or (g/element? val) (g/relationship? val))
    (tmp/make-wrapper target-model lv val)
    val))

(defn trg-initializeo
  "Only for internal use."
  [target-model enforcing src-match args-map & lvars]
  (fn [a]
    (ccl/unify a (vec lvars)
               (mapv (fn [lv]
                       (let [lv-kw    (keyword (:oname lv))
                             src-val  (get src-match lv-kw ::unknown)
                             args-val (get args-map  lv-kw ::unknown)]
                         (cond
                           (not= src-val  ::unknown)
                           (if enforcing
                             (maybe-wrap target-model lv src-val)
                             src-val)

                           (not= args-val ::unknown)
                           (if enforcing
                             (maybe-wrap target-model lv args-val)
                             args-val)

                           :else lv)))
                     lvars))))

(defn check-t-relation-args
  "Only for internal use.
  Check if the provided args in arg-map are valid (that is, in good-args)."
  [relsym arg-map good-args]
  (when-let [unbound-key (some #(when-not (good-args %) %) (keys arg-map))]
    (u/errorf "Unbound keyword arg %s when calling relation %s."
              unbound-key relsym)))

