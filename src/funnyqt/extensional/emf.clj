(ns funnyqt.extensional.emf
  "Specify EMF models extensionally."
  (:use funnyqt.emf)
  (:use funnyqt.extensional)
  (:use [funnyqt.utils :only [errorf split-qname]])
  (:use funnyqt.emf-protocols)
  (:require clojure.set)
  (:require funnyqt.protocols)
  (:import
   (org.eclipse.emf.ecore EObject EClass)))


;;# Dynamic vars

(def ^{:dynamic true
       :doc "Resolves the image of the given archetype in the img function
  corresponding to the EClass of the current attribute."}
  resolve-eobject)

;;# Utility Functions

(defn ^:private img-internal
  [^EClass ec arch]
  (let [m (@*img* ec)]
    (or (and m (m arch))
        (loop [subs (esubclasses (eroot-pkg-ns-uri ec) ec)]
          (when (seq subs)
            (or (get (@*img* (first subs)) arch)
                (recur (rest subs)))))
        (errorf "Couldn't resolve image of %s in img fn of %s: %s"
                arch ec @*img*))))

(defn ^:private arch-internal
  [^EClass ec img]
  (let [m (@*arch* ec)]
    (or (and m (m img))
        (loop [subs (esubclasses (eroot-pkg-ns-uri ec) ec)]
          (when (seq subs)
            (or (get (@*arch* (first subs)) img)
                (recur (rest subs)))))
        (errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
                img ec @*arch*))))

;;# Creating Elements

;;## Creating EObjects

#_(defn create-vertices!
  "In graph `g` create one vertex of VertexClass `cls` for every archetype
  returned by `archfn`.  `archfn` must return a collection of arbitrary
  objects.  It's value is taken as a set.  Traceability mappings are
  established implicitly."
  [g cls archfn]
  (let [^VertexClass vc (attributed-element-class g cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (create-vertex! g cls)
              a (first as)]
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

(defn create-eobjects!
  [m cls archfn]
  (let [^EClass vc (eclassifier (eroot-pkg-ns-uri m) cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (ecreate! m cls)
              a (first as)]
          (println "Created" v "for" a)
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

;;## Setting Attribute Values

#_(defn set-values!
  [m attrqn valfn]
  (let [[aecname attrname _] (split-qname attrqn)
        aec (attributed-element-class m aecname)]
    (doseq [[elem val] (binding [resolve-eobject (fn [arch] (img-internal aec arch))]
                         (doall (valfn)))]
      (set-value! elem attrname val))))

;;## Setting references
