(ns funnyqt.edn
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [funnyqt.utils :as u]
            [funnyqt.query :as q]
            [funnyqt.emf :as emf]
            [funnyqt.tg  :as tg]))

(defprotocol IPrEDN
  (write-edn [obj out]))

(defn ^:private write-edn-sequential-contents [s ^java.io.Writer out]
  (let [first (volatile! true)]
    (doseq [x s]
      (if @first
        (vreset! first false)
        (.write out ", "))
      (write-edn x out))))

(defn ^:private write-edn-map [m ^java.io.Writer out]
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

(extend-protocol IPrEDN
  java.lang.Number
  (write-edn [n ^java.io.Writer out]
    (.write out (str n)))
  java.lang.String
  (write-edn [n ^java.io.Writer out]
    (.write out (pr-str n)))
  clojure.lang.Keyword
  (write-edn [kw ^java.io.Writer out]
    (.write out (.toString kw)))
  clojure.lang.Symbol
  (write-edn [sym ^java.io.Writer out]
    (.write out (.toString sym)))
  clojure.lang.IRecord ;; FIXME: Why are records printed as maps?
  (write-edn [r ^java.io.Writer out]
    (.write out "#")
    (.write out (.getName (class r)))
    (write-edn-map r))
  java.util.Map
  (write-edn [m ^java.io.Writer out]
    (write-edn-map m out))
  clojure.lang.IPersistentSet
  (write-edn [s ^java.io.Writer out]
    (.write out "#{")
    (write-edn-sequential-contents s out)
    (.write out "}"))
  clojure.lang.IPersistentVector
  (write-edn [v ^java.io.Writer out]
    (.write out "[")
    (write-edn-sequential-contents v out)
    (.write out "]"))
  java.util.Collection
  (write-edn [coll ^java.io.Writer out]
    (.write out "(")
    (write-edn-sequential-contents coll out)
    (.write out ")"))
  ;; Model Data Types
  de.uni_koblenz.jgralab.Graph
  (write-edn [g ^java.io.Writer out]
    (.write out (pr-str (symbol "#funnyqt.tg/Graph")
                        (tg/id g))))
  de.uni_koblenz.jgralab.Vertex
  (write-edn [v ^java.io.Writer out]
    (.write out (pr-str (symbol "#funnyqt.tg/Vertex")
                        {:graph-id (tg/id (tg/graph v))
                         :id (tg/id v)})))
  de.uni_koblenz.jgralab.Edge
  (write-edn [e ^java.io.Writer out]
    (.write out (pr-str (symbol "#funnyqt.tg/Edge")
                        {:graph-id (tg/id (tg/graph e))
                         :id (tg/id e)}))))

(defn pr-edn [obj]
  (write-edn obj *out*))

(defn prn-edn [obj]
  (write-edn obj *out*)
  (.write *out* "\n"))

(defn pr-edn-str [obj]
  (binding [*out* (java.io.StringWriter.)]
    (pr-edn obj)
    (.toString *out*)))

(defn save-as-edn [obj file]
  (binding [*out* (io/writer file)]
    (prn-edn obj)))

(def ^:dynamic *models*
  "Bound to a set of models during calls to `read-edn`, `read-edn-string`, and
  `read-edn-file`."
  nil)

(defn ^:private read-graph [id]
  (q/the #(and (instance? de.uni_koblenz.jgralab.Graph %)
               (= id (tg/id %)))
         *models*))

(defn ^:private read-vertex [gid id]
  (let [g (read-graph gid)]
    (tg/vertex g id)))

(defn ^:private read-edge [gid id]
  (let [g (read-graph gid)]
    (tg/edge g id)))

(def edn-readers
  "A map of FunnyQT EDN tags to vars of functions that should be used for such
  an EDN data element."
  {'funnyqt.tg/Graph  #'read-graph
   'funnyqt.tg/Vertex #'read-vertex
   'funnyqt.tg/Edge   #'read-edge})

(defn read-edn-string
  ([string models]
   (read-edn-string {} string models))
  ([opts string models]
   (binding [*models* (set models)]
     (edn/read-string (update-in opts [:readers] merge edn-readers)
                      string))))
