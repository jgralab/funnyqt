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

;;# Creating Elements

;;## Creating Vertices

(defn create-vertices!
  "In graph `g` create one vertex of VertexClass `cls` for every archetype
  returned by `archfn`.  `archfn` must return a collection of arbitrary
  objects.  It's value is taken as a set.  Traceability mappings are
  established implicitly.  Returns the sequence of new vertices."
  [g cls archfn]
  (let [^VertexClass vc (tg/attributed-element-class g cls)]
    (loop [as (set (archfn))
           im (transient {})]
      (if (seq as)
        (let [v (tg/create-vertex! g cls)
              a (first as)]
          ;;(println "Created" v "for" a)
          (recur (rest as)
                 (assoc! im a v)))
        (let [img  (persistent! im)]
          (e/add-trace-mappings! vc img)
          (vals img))))))

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
  edge's source/target vertex class.

  Returns the sequence of new edges."
  [g cls archfn]
  (let [^EdgeClass ec (tg/attributed-element-class g cls)
        saec (-> ec (.getFrom) (.getVertexClass))
        eaec (-> ec (.getTo)   (.getVertexClass))]
    (loop [as (binding [resolve-alpha (partial e/image saec)
                        resolve-omega (partial e/image eaec)]
                (set (archfn)))
           im (transient {})]
      (if (seq as)
        (let [[a al om] (first as)
              e (tg/create-edge! g cls al om)]
          (recur (rest as)
                 (assoc! im a e)))
        (let [img  (persistent! im)]
          (e/add-trace-mappings! ec img)
          (vals img))))))

;;## Setting Attribute Values

(defn set-values!
  "In graph `g` set the `attr` values for all `aec` elements according to
  `valfn`, i.e., `valfn` has to return a map {attr-elem attr-value} or a
  collection of pairs.

  In `valfn`, `resolve-element` is bound to a function that given an archetype
  of the class defining the attribute returns its image, that is, the instance
  of the defining class (or subclass) that has been created for the given
  archetype."
  [g aec attr valfn]
  (let [aec (tg/attributed-element-class g aec)]
    (doseq [[elem val] (binding [resolve-element (fn [arch] (e/image aec arch))]
                         (doall (valfn)))]
      (tg/set-value! elem attr val))))
