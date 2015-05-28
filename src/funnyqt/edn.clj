(ns funnyqt.edn
  "Printing/persisting and reading/loading query results and transformation
  traces as EDN."
  (:refer-clojure :exclude [pr prn pr-str read read-string slurp spit])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [funnyqt.utils :as u]
            [funnyqt.query :as q]
            [funnyqt.emf :as emf]
            [funnyqt.tg  :as tg])
  (:import (org.eclipse.emf.ecore.resource Resource ResourceSet)
           (org.eclipse.emf.ecore EObject)))

;;# Writing EDN

;;## Public API

(defprotocol IWriteEDN
  "Protocol for writing data as EDN.

  By default, all EDN types listed on http://edn-format.org are supported, plus
  several JGraLab types (Graph, Vertex, Edge) and EMF types (ResourceSet,
  Resource, EObject).

  To support other modeling frameworks, simply extend this protocol to its
  types, and also provide corresponding reader functions in `edn-readers`."
  (write-edn [obj out]
    "Write the EDN representation of `obj` to `out` (a java.util.Writer)."))

(defn pr
  "Prints `obj`s EDN representation to the current value of `*out*`."
  [obj]
  (write-edn obj *out*))

(defn prn
  "Prints `obj`s EDN representation + newline to the current value of `*out*`."
  [obj]
  (write-edn obj *out*)
  (.write *out* "\n"))

(defn ^java.lang.String pr-str
  "Returns `obj`s EDN representation as a string."
  ^java.lang.String [obj]
  (binding [*out* (java.io.StringWriter.)]
    (pr obj)
    (.toString *out*)))

(defn spit
  "Spits `obj`s EDN representation to `file`.
  If the file already exists, it will be overwritten."
  [obj file]
  (with-open [w (io/writer file)]
    (binding [*out* w]
      (prn obj))))

;;## Internals and implementation

(defn ^:private write-edn-sequential-contents [s ^java.io.Writer out]
  (let [first (volatile! true)]
    (doseq [x s]
      (if @first
        (vreset! first false)
        (.write out ", "))
      (write-edn x out))))

(defn ^:private write-edn-vector [v ^java.io.Writer out]
  (.write out "[")
  (write-edn-sequential-contents v out)
  (.write out "]"))

(defn ^:private write-edn-list [l ^java.io.Writer out]
  (.write out "(")
  (write-edn-sequential-contents l out)
  (.write out ")"))

(extend-protocol IWriteEDN
  ;; Standard Types supported in EDN
  nil
  (write-edn [n ^java.io.Writer out]
    (.write out "nil"))
  java.lang.Boolean
  (write-edn [b ^java.io.Writer out]
    (.write out (Boolean/toString b)))
  java.lang.Character
  (write-edn [c ^java.io.Writer out]
    (print-dup c out))
  clojure.lang.Symbol
  (write-edn [sym ^java.io.Writer out]
    (.write out (.toString sym)))
  clojure.lang.Keyword
  (write-edn [kw ^java.io.Writer out]
    (.write out (.toString kw)))
  java.lang.Number
  (write-edn [n ^java.io.Writer out]
    (.write out (str n)))
  java.lang.String
  (write-edn [n ^java.io.Writer out]
    (.write out (clojure.core/pr-str n)))
  java.util.Map
  (write-edn [m ^java.io.Writer out]
    (.write out "{")
    (let [first (volatile! true)]
      (doseq [[k v] m]
        (if @first
          (vreset! first false)
          (.write out ", "))
        (write-edn k out)
        (.write out " ")
        (write-edn v out)))
    (.write out "}"))
  java.util.Set
  (write-edn [s ^java.io.Writer out]
    (.write out "#{")
    (write-edn-sequential-contents s out)
    (.write out "}"))
  java.util.Collection
  (write-edn [coll ^java.io.Writer out]
    (if (instance? java.util.RandomAccess coll)
      (write-edn-vector coll out)
      (write-edn-list coll out)))
  ;; TGraphs, Vertices, and Edges
  de.uni_koblenz.jgralab.Graph
  (write-edn [g ^java.io.Writer out]
    (.write out "#funnyqt.tg/Graph ")
    (write-edn (tg/id g) out))
  de.uni_koblenz.jgralab.Vertex
  (write-edn [v ^java.io.Writer out]
    (.write out "#funnyqt.tg/Vertex ")
    (write-edn [(tg/graph v) (tg/id v)] out))
  de.uni_koblenz.jgralab.Edge
  (write-edn [e ^java.io.Writer out]
    (.write out "#funnyqt.tg/Edge ")
    (write-edn [(tg/graph e) (tg/id e)] out))
  ;; EMF Resources and EObjects
  Resource
  (write-edn [r ^java.io.Writer out]
    (.write out "#funnyqt.emf/Resource ")
    (write-edn (.toString (.getURI r)) out))
  ResourceSet
  (write-edn [rs ^java.io.Writer out]
    (.write out "#funnyqt.emf/ResourceSet ")
    (write-edn (set (.getResources rs)) out))
  EObject
  (write-edn [eo ^java.io.Writer out]
    (.write out "#funnyqt.emf/EObject ")
    (if-let [r (.eResource eo)]
      (write-edn [r (.getURIFragment r eo)] out)
      (u/errorf "Cannot write EObject not contained in a Resource: %s" eo))))

;;# Reading EDN

(def ^:dynamic *models*
  "Bound to a set of models during calls to `read-edn`, `read-string`, and
  `read-edn-file`."
  nil)

;;## Internals and Implementation

(defn ^:private edn-tg-graph-reader [id]
  (q/the #(and (instance? de.uni_koblenz.jgralab.Graph %)
               (= id (tg/id %)))
         *models*))

(defn ^:private edn-tg-vertex-reader [[g vid]]
  (tg/vertex g vid))

(defn ^:private edn-tg-edge-reader [[g eid]]
  (tg/edge g eid))

(defn ^:private edn-emf-resource-reader [uri]
  (let [uri (org.eclipse.emf.common.util.URI/createURI uri)
        rs (filter (fn [m]
                     (and (instance? Resource m)
                          (= uri (.getURI ^Resource m))))
                   (concat *models*
                           (sequence (comp (filter #(instance? ResourceSet %))
                                           (mapcat (fn [^ResourceSet rs]
                                                     (.getResources rs))))
                                     *models*)))]
    (if (seq rs)
      (first rs)
      (u/errorf "No Resource for URI %s" uri))))

(defn ^:private edn-emf-resource-set-reader [set-of-resources]
  (q/the (fn [m]
           (and (instance? ResourceSet m)
                (let [rs (set (.getResources ^ResourceSet m))]
                  (every? #(q/member? % rs) set-of-resources))))
         *models*))

(defn ^:private edn-emf-eobject-reader [[^Resource resource fragment]]
  (.getEObject resource fragment))

;;## Public API

(def edn-readers
  "A map of FunnyQT EDN tags to vars of functions that should be used for such
  an EDN data element.

  To add support for other data types, add readers here and implement the
  writing part by extending the `IWriteEDN` protocol upon these types."
  {'funnyqt.tg/Graph        #'edn-tg-graph-reader
   'funnyqt.tg/Vertex       #'edn-tg-vertex-reader
   'funnyqt.tg/Edge         #'edn-tg-edge-reader
   'funnyqt.emf/Resource    #'edn-emf-resource-reader
   'funnyqt.emf/ResourceSet #'edn-emf-resource-set-reader
   'funnyqt.emf/EObject     #'edn-emf-eobject-reader})

(defn read
  "Read and return one object from `stream` and consider references into/to `models`.
  For `opts` see `clojure.edn/read`."
  ([stream models]
   (read {} stream models))
  ([opts stream models]
   (binding [*models* (set models)]
     (edn/read (update-in opts [:readers] merge edn-readers)
               stream))))

(defn read-string
  "Read and return one object from `string` and consider references into/to `models`.
  For `opts` see `clojure.edn/read-string`."
  ([string models]
   (read-string {} string models))
  ([opts string models]
   (binding [*models* (set models)]
     (edn/read-string (update-in opts [:readers] merge edn-readers)
                      string))))

(defn slurp
  "Read and return one object from `stream` and consider references into/to `models`.
  For `opts` see `clojure.edn/read`."
  ([file models]
   (slurp {} file models))
  ([opts file models]
   (with-open [reader (java.io.PushbackReader. (io/reader file))]
     (read opts reader models))))
