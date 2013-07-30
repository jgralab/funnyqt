(ns funnyqt.extensional.emf
  "Specify EMF models extensionally."
  (:use funnyqt.emf)
  (:use funnyqt.extensional)
  (:use [funnyqt.utils :only [errorf split-qname]])
  (:use funnyqt.emf-protocols)
  (:require clojure.set)
  (:require funnyqt.protocols)
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
  (let [m (@*img* ec)]
    (or (and m (m arch))
        (loop [subs (eallsubclasses ec)]
          (when (seq subs)
            (or (get (@*img* (first subs)) arch)
                (recur (rest subs)))))
        (errorf "Couldn't resolve image of %s in img fn of %s: %s"
                (print-str arch) (print-str ec) @*img*))))

(defn ^:private arch-internal
  [^EClass ec img]
  (let [m (@*arch* ec)]
    (or (and m (m img))
        (loop [subs (eallsubclasses ec)]
          (when (seq subs)
            (or (get (@*arch* (first subs)) img)
                (recur (rest subs)))))
        (errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
                (print-str img) (print-str ec) @*arch*))))

;;# Creating Elements

;;## Creating EObjects

(defn create-eobjects!
  [m cls archfn]
  (let [^EClass vc (eclassifier cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (ecreate! cls)
              a (first as)]
          (eadd! m v)
          ;;(println "Created" v "for" a)
          (recur (rest as)
                 (assoc! im a v)
                 (assoc! am v a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when *img*
            (swap! *img*  into-trace-map vc img))
          (when *arch*
            (swap! *arch* into-trace-map vc arch))
          (keys arch))))))

;;## Setting Features Values

(defn ^:private internal-modify-feature-fn
  [m featureqn valfn action]
  (let [[ecname attrname _] (split-qname featureqn)
        ^EClass ec (eclassifier ecname)]
    (if-let [^EStructuralFeature sf (.getEStructuralFeature ec ^String attrname)]
      (let [resolve-target-fn (if (instance? EReference sf)
                                (partial img-internal (.getEReferenceType ^EReference sf))
                                #(errorf "Can't call `resolve-target` for EAttribute %s!"
                                         featureqn))
            resolve-all-targets-fn (if (instance? EReference sf)
                                     (partial map resolve-target-fn)
                                     #(errorf "Can't call `resolve-all-targets` for EAttribute %s!"
                                              featureqn))]
        (doseq [[elem val]
                (binding [resolve-eobject (partial img-internal ec)
                          resolve-target resolve-target-fn
                          resolve-all-targets resolve-all-targets-fn]
                  (doall (valfn)))]
          (action elem attrname val)))
      (errorf "%s has no EStructuralFeature %s." (print-str ec) attrname))))

(defn set-features!
  [m featureqn valfn]
  (internal-modify-feature-fn m featureqn valfn eset!))

(defn add-features!
  [m featureqn valfn]
  (internal-modify-feature-fn m featureqn valfn
                              (fn [e an val]
                                (when (seq val)
                                  (apply eadd! e an (first val) (rest val))))))
