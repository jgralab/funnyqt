(ns funnyqt.extensional.emf
  "Specify EMF models extensionally."
  (:require clojure.set
            [funnyqt.utils         :as u]
            [funnyqt.protocols     :as p]
            [funnyqt.emf           :as emf]
            [funnyqt.emf-protocols :as ep]
            [funnyqt.extensional   :as e])
  (:import
   (org.eclipse.emf.ecore EObject EClass EStructuralFeature EReference)))


;;# Dynamic vars

(def ^{:dynamic true
       :doc "Resolves the image of the given archetype in the img function
  corresponding to the EClass of the current attribute."}
  resolve-eobject)

(def ^{:dynamic true
       :doc "todo"}
  resolve-target)

(def ^{:dynamic true
       :doc "todo"}
  resolve-all-targets)

;;# Utility Functions

(defn ^:private img-internal
  [^EClass ec arch]
  (let [m (@e/*img* ec)]
    (or (and m (m arch))
        (loop [subs (emf/eallsubclasses ec)]
          (when (seq subs)
            (or (get (@e/*img* (first subs)) arch)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve image of %s in img fn of %s: %s"
                  (print-str arch) (print-str ec) @e/*img*))))

(defn ^:private arch-internal
  [^EClass ec img]
  (let [m (@e/*arch* ec)]
    (or (and m (m img))
        (loop [subs (emf/eallsubclasses ec)]
          (when (seq subs)
            (or (get (@e/*arch* (first subs)) img)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
                  (print-str img) (print-str ec) @e/*arch*))))

;;# Creating Elements

;;## Creating EObjects

(defn create-eobjects!
  [m cls archfn]
  (let [^EClass vc (emf/eclassifier cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (emf/ecreate! cls)
              a (first as)]
          (emf/eadd! m v)
          ;;(println "Created" v "for" a)
          (recur (rest as)
                 (assoc! im a v)
                 (assoc! am v a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when e/*img*
            (swap! e/*img*  e/into-trace-map vc img))
          (when e/*arch*
            (swap! e/*arch* e/into-trace-map vc arch))
          (keys arch))))))

;;## Setting Features Values

(defn ^:private internal-modify-feature-fn
  [m featureqn valfn action]
  (let [[ecname attrname _] (u/split-qname featureqn)
        ^EClass ec (emf/eclassifier ecname)]
    (if-let [^EStructuralFeature sf (.getEStructuralFeature ec ^String attrname)]
      (let [resolve-target-fn (if (instance? EReference sf)
                                (partial img-internal (.getEReferenceType ^EReference sf))
                                #(u/errorf "Can't call `resolve-target` for EAttribute %s!"
                                           featureqn))
            resolve-all-targets-fn (if (instance? EReference sf)
                                     (partial map resolve-target-fn)
                                     #(u/errorf
                                       "Can't call `resolve-all-targets` for EAttribute %s!"
                                       featureqn))]
        (doseq [[elem val]
                (binding [resolve-eobject (partial img-internal ec)
                          resolve-target resolve-target-fn
                          resolve-all-targets resolve-all-targets-fn]
                  (doall (valfn)))]
          (action elem attrname val)))
      (u/errorf "%s has no EStructuralFeature %s." (print-str ec) attrname))))

(defn set-features!
  [m featureqn valfn]
  (internal-modify-feature-fn m featureqn valfn emf/eset!))

(defn add-features!
  [m featureqn valfn]
  (internal-modify-feature-fn m featureqn valfn emf/eaddall!))
