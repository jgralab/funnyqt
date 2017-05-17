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
              relation src-match))
  (first matches))

(defn ^:private replace-tmps-and-wrappers-with-manifestations
  "Only for internal use."
  [trg-match]
  (into {}
        (map (fn [[k v]]
               [k (if (tmp/tmp-or-wrapper-element? v)
                    (tmp/manifestation v)
                    v)]))
        trg-match))

(defn enforce-match
  "Only for internal use.
  Manifests the temporary and wrapper elements in `match`."
  [match id-map-atom]
  ;; First manifest the temps and wrappers...
  (doseq [el (vals match)
          :when (tmp/tmp-or-wrapper-element? el)]
    (tmp/manifest el)
    (swap! id-map-atom (fn [m]
                         (if-let [[el v] (find m el)]
                           (let [m (dissoc m el)]
                             (assoc m (tmp/manifestation el) v))
                           m))))
  ;; ... then remove unneeded tmps and wrappers from the id-map
  (let [new-match (replace-tmps-and-wrappers-with-manifestations match)
        els (into #{} (vals new-match))]
    (swap! id-map-atom (fn [m]
                         (let [tr (filter (fn [el]
                                            (and (tmp/tmp-or-wrapper-element? el)
                                                 (not (els el))))
                                          (keys m))]
                           (apply dissoc m tr))))
    new-match))

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

(defn id [map-atom elem val]
  (fn [a]
    (let [gelem (ccl/walk* a elem)
          gelem (if (tmp/wrapper-element? gelem)
                  (tmp/manifestation gelem)
                  gelem)
          gval (ccl/walk* a val)
          [melem mval] (find @map-atom gelem)]
      (when-not (ru/ground? gelem)
        (u/error "elem must be ground in (id elem val) goals."))
      (if melem
        (and (ccl/unify a melem gelem)
             (ccl/unify a mval gval))
        (do
          (swap! map-atom assoc gelem gval)
          (ccl/succeed a))))))

(defn delete-unmatched-target-elements
  [left right dir trace]
  (when-not (#{:right :left} dir)
    (u/error "Must not be used in checkonly transformations."))
  (let [matched-els (into #{} (comp
                               (mapcat identity)
                               (mapcat vals)
                               (filter g/element?))
                          (->> trace :related vals))]
    (doseq [el (g/elements (if (= dir :right) right left))]
      (when (not (matched-els el))
        (g/delete! el false)))))
