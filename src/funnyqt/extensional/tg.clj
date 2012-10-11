(ns funnyqt.extensional.tg
  "Specify TGraphs extensionally."
  (:use funnyqt.tg)
  (:use funnyqt.query.tg)
  (:use funnyqt.query)
  (:use funnyqt.extensional)
  (:use funnyqt.protocols)
  (:use [funnyqt.utils :only [error errorf split-qname pr-identity]])
  (:require clojure.set)
  (:require [clojure.tools.macro :as m])
  (:import
   (de.uni_koblenz.jgralab.schema GraphElementClass EdgeClass VertexClass)))


;;# Dynamic vars

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the start vertex class."}
  resolve-alpha nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-edge-class!` / `create-edges!`.
  Resolves the image of the given archetype in the img function corresponding
  to the end vertex class."}
  resolve-omega nil)

(def ^{:dynamic true
       :doc "Only bound in calls to `create-attribute!` / `set-values!`.
  Resolves the image of the given archetype in the img function corresponding
  to the attributed element class of the current attribute."}
  resolve-element nil)

;;# Utility Functions

(defn- img-internal-1
  "Returns the image of `arch` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec arch]
  (or (when-let [m (@*img* aec)]
        (m arch))
      (first (remove nil?
                     (map #(img-internal-1 %1 arch)
                          (.getDirectSubClasses ^GraphElementClass aec))))))

(defn- img-internal
  [aec arch]
  (or (img-internal-1 aec arch)
      (errorf "Couldn't resolve image of %s in img fn of %s: %s"
              arch aec @*img*)))

(defn- arch-internal-1
  "Returns the archetype of `img` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [aec img]
  (or (when-let [m (@*arch* aec)]
        (m img))
      (first (remove nil?
                     (map #(arch-internal-1 %1 img)
                          (.getDirectSubClasses ^GraphElementClass aec))))))

(defn- arch-internal
  [aec img]
  (or (arch-internal-1 aec img)
      (errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
              img aec @*arch*)))

;;# Creating Elements

;;## Creating Vertices

(defn create-vertices! [g cls archfn]
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

;;## Creating Edges

(defn create-edges! [g cls archfn]
  (let [^EdgeClass ec (attributed-element-class g cls)
        saec (-> ec (.getFrom) (.getVertexClass))
        eaec (-> ec (.getTo)   (.getVertexClass))]
    (loop [as (binding [resolve-alpha #(img-internal saec %)
                        resolve-omega #(img-internal eaec %)]
                (set (archfn)))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [[a al om] (first as)
              e (create-edge! g cls al om)]
          (recur (rest as) (assoc! im a e) (assoc! am e a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when *img*
            (swap! *img*  into-trace-map ec img))
          (when *arch*
            (swap! *arch* into-trace-map ec arch))
          (keys arch))))))

;;## Setting Attribute Values

(defn set-values! [g a valfn]
  (let [[aecname attrname _] (split-qname a)
        aec (attributed-element-class g aecname)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (img-internal aec arch))]
                         (doall (valfn)))]
      (set-value! elem attrname val))))
