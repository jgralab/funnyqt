(ns funnyqt.extensional.tg
  "Specify TGraphs extensionally."
  (:use funnyqt.tg)
  (:use funnyqt.extensional)
  (:use funnyqt.protocols)
  (:use [funnyqt.utils :only [errorf split-qname]])
  (:require clojure.set)
  (:import
   (de.uni_koblenz.jgralab.schema GraphElementClass EdgeClass VertexClass)))


;;# Dynamic vars

(def ^{:dynamic true
       :doc "Resolves the image of the given archetype in the img function
  corresponding to the start vertex class."}
  resolve-alpha)

(def ^{:dynamic true
       :doc "Resolves the image of the given archetype in the img function
  corresponding to the end vertex class."}
  resolve-omega)

(def ^{:dynamic true
       :doc "Resolves the image of the given archetype in the img function
  corresponding to the attributed element class of the current attribute."}
  resolve-element)

;;# Utility Functions

(defn ^:private img-internal-1
  "Returns the image of `arch` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [^GraphElementClass aec arch]
  (or (when-let [m (@*img* aec)]
        (m arch))
      (first (remove nil?
                     (map #(img-internal-1 %1 arch)
                          (.getDirectSubClasses aec))))))

(defn ^:private img-internal
  [aec arch]
  (or (img-internal-1 aec arch)
      (errorf "Couldn't resolve image of %s in img fn of %s: %s"
              arch aec @*img*)))

(defn ^:private arch-internal-1
  "Returns the archetype of `img` for AttributedElementClass `aec`.
  Can only be called inside a deftransformation."
  [^GraphElementClass aec img]
  (or (when-let [m (@*arch* aec)]
        (m img))
      (first (remove nil?
                     (map #(arch-internal-1 %1 img)
                          (.getDirectSubClasses aec))))))

(defn ^:private arch-internal
  [aec img]
  (or (arch-internal-1 aec img)
      (errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
              img aec @*arch*)))

;;# Creating Elements

;;## Creating Vertices

(defn create-vertices!
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

;;## Creating Edges

(defn create-edges!
  "In graph `g` create one edge of edge class `cls` for every archetype
  returned by `archfn`.  `cls` is a symbol denoting the qualified name of the
  edge class.

  `archfn` must return a collection of triples [arch src trg].  arch is an
  arbitrary object taken as archetype for the new edge, and src and trg are the
  new edge's source and target vertex.  Traceability mappings are established
  implicitly.

  In `archfn`, `resolve-alpha` and `resolve-omega` are bound to functions that
  return the image of the given archetype in the image-mapping of the new
  edge's source/target vertex class."
  [g cls archfn]
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

(defn set-values!
  "In graph `g` set the attribute `attrqn` for all element-value pairs returned
  by `valfn`, i.e., `valfn` has to return a map {attr-elem attrqn-value} or a
  collection of pairs.  `attrqn` is a qualified attribute name,
  e.g. Person.firstName.

  In `valfn`, `resolve-element` is bound to a function that given an archetype
  for the class defining the attribute returns the image, that is, the instance
  of the defining class (or subclass) that has been created for the given
  archetype."
  [g attrqn valfn]
  (let [[aecname attrname _] (split-qname attrqn)
        aec (attributed-element-class g aecname)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (img-internal aec arch))]
                         (doall (valfn)))]
      (set-value! elem attrname val))))
