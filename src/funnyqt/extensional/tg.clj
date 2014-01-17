(ns funnyqt.extensional.tg
  "Specify TGraphs extensionally."
  (:require clojure.set
            [funnyqt.tg          :as tg]
            [funnyqt.extensional :as e]
            [funnyqt.utils       :as u])
  (:import
   (de.uni_koblenz.jgralab.schema GraphElementClass EdgeClass VertexClass)))


;;# Dynamic vars

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the start vertex class.  This function is only bound in
  `create-edges!`."}
  resolve-alpha)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the target vertex class.  This function is only bound in
  `create-edges!`."}
  resolve-omega)

(def ^{:dynamic true
       :arglists '([archetype])
       :doc "Resolves the image of the given `archetype` in the img function
  corresponding to the attributed element class of the current attribute.  This
  function is only bound in `set-values!`."}
  resolve-element)

;;# Utility Functions

(defn ^:private img-internal
  "Returns the image of `arch` for GraphElementClass `gec`.
  Can only be called inside a `deftransformation`."
  [^GraphElementClass gec arch]
  (let [m (@e/*img* gec)]
    (or (and m (m arch))
        (loop [subs (.getAllSubClasses gec)]
          (when (seq subs)
            (or (get (@e/*img* (first subs)) arch)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve image of %s in img fn of %s: %s"
                  arch gec @e/*img*))))

(defn ^:private arch-internal
  "Returns the archetype of `img` for GraphElementClass `gec`.
  Can only be called inside a `deftransformation`."
  [^GraphElementClass gec img]
  (let [m (@e/*arch* gec)]
    (or (and m (m img))
        (loop [subs (.getAllSubClasses gec)]
          (when (seq subs)
            (or (get (@e/*arch* (first subs)) img)
                (recur (rest subs)))))
        (u/errorf "Couldn't resolve archetype of %s in arch fn of %s: %s"
                  img gec @e/*arch*))))

;;# Creating Elements

;;## Creating Vertices

(defn create-vertices!
  "In graph `g` create one vertex of VertexClass `cls` for every archetype
  returned by `archfn`.  `archfn` must return a collection of arbitrary
  objects.  It's value is taken as a set.  Traceability mappings are
  established implicitly."
  [g cls archfn]
  (let [^VertexClass vc (tg/attributed-element-class g cls)]
    (loop [as (set (archfn))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [v (tg/create-vertex! g cls)
              a (first as)]
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
  (let [^EdgeClass ec (tg/attributed-element-class g cls)
        saec (-> ec (.getFrom) (.getVertexClass))
        eaec (-> ec (.getTo)   (.getVertexClass))]
    (loop [as (binding [resolve-alpha #(img-internal saec %)
                        resolve-omega #(img-internal eaec %)]
                (set (archfn)))
           im (transient {})
           am (transient {})]
      (if (seq as)
        (let [[a al om] (first as)
              e (tg/create-edge! g cls al om)]
          (recur (rest as) (assoc! im a e) (assoc! am e a)))
        (let [img  (persistent! im)
              arch (persistent! am)]
          (when e/*img*
            (swap! e/*img*  e/into-trace-map ec img))
          (when e/*arch*
            (swap! e/*arch* e/into-trace-map ec arch))
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
  (let [[aecname attrname _] (u/split-qname attrqn)
        aec (tg/attributed-element-class g aecname)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (img-internal aec arch))]
                         (doall (valfn)))]
      (tg/set-value! elem attrname val))))
