(ns funnyqt.extensional.emf
  "Specify EMF models extensionally."
  (:require clojure.set
            [funnyqt.utils       :as u]
            [funnyqt.emf         :as emf]
            [funnyqt.extensional :as e])
  (:import
   (org.eclipse.emf.ecore EObject EClass EStructuralFeature EReference)))


;;# Dynamic vars

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the EClass of the current structural feature.  This function
  is only bound inside `set-values!` and `add-values!`."}
  resolve-eobject)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Returns the image of the given `archetype` in the image function
  of the current EReference's target class.  This function is only bound inside
  `set-values!` and `add-values!`."}
  resolve-target)

(def ^{:dynamic true
       :arglists '([archetypes])
       :doc "Returns the images of the given collection of `archetypes` in the
  image function of the current EReference's target class.  This function is
  only bound inside `set-values!` and `add-values!`."}
  resolve-all-targets)

;;# Creating Elements

;;## Creating EObjects

(defn create-eobjects!
  [m cls archfn]
  (let [^EClass vc (emf/eclassifier cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (emf/ecreate! m cls)
              a (first as)]
          ;;(println "Created" v "for" a)
          (recur (rest as)
                 (assoc! im a v)
                 (assoc! am v a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when e/*img*
            (swap! e/*img* e/into-trace-map vc img))
          (when e/*arch*
            (swap! e/*arch* e/into-trace-map vc arch))
          (keys arch))))))

;;## Setting Features Values

(defn ^:private internal-modify-feature-fn
  [m ecls feature valfn action]
  (let [^EClass ec (emf/eclassifier ecls)]
    (if-let [^EStructuralFeature sf (.getEStructuralFeature ec ^String (name feature))]
      (let [resolve-target-fn (if (emf/ereference? sf)
                                (partial e/image (.getEReferenceType ^EReference sf))
                                #(u/errorf "Can't call `resolve-target` for EAttribute %s!"
                                           ecls feature))
            resolve-all-targets-fn (if (emf/ereference? sf)
                                     (partial map resolve-target-fn)
                                     #(u/errorf
                                       "Can't call `resolve-all-targets` for EAttribute %s!"
                                       ecls feature))]
        (doseq [[elem val]
                (binding [resolve-eobject     (partial e/image ec)
                          resolve-target      resolve-target-fn
                          resolve-all-targets resolve-all-targets-fn]
                  (doall (valfn)))]
          (action elem feature val)))
      (u/errorf "%s has no EStructuralFeature %s." (print-str ec) feature))))

(defn set-values!
  "TODO: Document me!"
  [m ecls feature valfn]
  (internal-modify-feature-fn m ecls feature valfn emf/eset!))

(defn add-values!
  "TODO: Document me!"
  [m ecls feature valfn]
  (internal-modify-feature-fn m ecls feature valfn emf/eaddall!))
