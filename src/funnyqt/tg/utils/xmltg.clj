(ns funnyqt.tg.utils.xmltg
  "<p>Convert XML to DOM-like TGraphs.</p>

The DOM-like schema looks like this:

<img src=\"https://raw.github.com/jgralab/funnyqt/master/resources/xml-schema.png\">

To transform an XML document to a DOM-like TGraph, use

  (xml2graph \"example.xml\")

If the XML file has a DTD which describes what attributes are IDs, IDREFs, and
IDREFS, all references will be represented as References edges in the Graph.

If the XML file doesn't contain a DTD, then you can provide a function that
receives an element name and an attribute name and should return the correct
attribute type as string.

  (xml2graph \"example-without-dtd.xml\"
             #(cond ;; Here, we distingish only by 2nd arg, the attr name
                (= %2 \"id\")        \"ID\"
                (= %2 \"links\")     \"IDREF\"
                (= %2 \"children\")  \"IDREFS\"))"
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query)
  (:use funnyqt.generic)
  (:use [funnyqt.utils :only [error]])
  (:require clojure.string)
  (:require [clojure.java.io :as io])
  (:import
   (org.xml.sax Attributes SAXException)
   (org.xml.sax.helpers DefaultHandler)
   (javax.xml XMLConstants)
   (javax.xml.parsers SAXParser SAXParserFactory)
   (javax.xml.validation SchemaFactory)
   (de.uni_koblenz.jgralab Graph Vertex Edge)
   (de.uni_koblenz.jgralab.schema Schema)))

;;# XML Graph Utils

(defn xml-children
  "Returns the children Element vertices of Element vertex `e`.
  May be restricted to elemenst of `type` given as symbol, keyword, or string."
  ([e]
     (adjs e :children))
  ([e type]
     (filter (fn [c]
               (= (value c :name) (name type)))
             (adjs e :children))))

(defn xml-attr-value
  "Returns the value of `elem`s xml attribute `attr-name`."
  [elem attr-name]
  (let [attr (the (filter #(= (value % :name) (name attr-name))
                          (adjs elem :attributes)))]
    (value attr :value)))

(defn xml-describe-elem
  "Returns a map describing the given xml Element vertex `e`."
  ([e]
     (xml-describe-elem e true))
  ([e with-children]
     {:name (value e :name)
      :attrs (apply hash-map
                    (mapcat (fn [a]
                              [(keyword (value a :name)) (value a :value)])
                            (adjs e :attributes)))
      :children (if with-children
                  (map #(xml-describe-elem % false)
                       (adjs e :children))
                  :skipped)}))

;;# Parsing XML -> XMLGraph

;;## Internal vars

(def ^{:dynamic true :private true} *graph*)
(def ^{:dynamic true :private true} *stack*)
(def ^{:dynamic true :private true} *current*)
(def ^{:dynamic true :private true} *state*)   ;; :element :chars :between
(def ^{:dynamic true :private true} *sb*)
(def ^{:dynamic true :private true} *attr-type-fn*)
(def ^{:dynamic true :private true} *id2elem*) ;; map from ID to Element vertex

;; map from Attribute vertex to a collection
;; of referenced element IDs (an attr can
;; reference multiple elements in terms of a
;; IDREFS attr type)
(def ^{:dynamic true :private true} *attr2refd-ids*)

;;## Internal functions

(defn- handle-attributes
  [elem ^Attributes attrs]
  (when-not (zero? (.getLength attrs))
    (loop [i 0, l (.getLength attrs), as []]
      (if (== i l)
        (loop [a as]
          (when (seq a)
            (let [[n v t] (first a)
                  t (if (= t "CDATA")
                      (*attr-type-fn* (value elem :name) n)
                      t)
                  av (create-vertex! *graph* 'Attribute)]
              (set-value! av :name n)
              (set-value! av :value v)
              (create-edge! 'HasAttribute elem av)
              (cond
               (= t "ID")     (set! *id2elem* (assoc *id2elem* v elem))
               (= t "IDREF")  (set! *attr2refd-ids*
                                    (update-in *attr2refd-ids* [av]
                                               #(conj % v)))
               (= t "IDREFS") (set! *attr2refd-ids*
                                   (update-in *attr2refd-ids* [av]
                                              #(set (concat % (clojure.string/split v #" ")))))))
            (recur (rest a))))
        (recur (inc i)
               l
               (conj as [(.getLocalName attrs i)
                         (.getValue attrs i)
                         (.getType attrs i)]))))))
(defn- resolve-refs
  "Create References edges for ID/IDREF[S]s collected while parsing."
  []
  (loop [a2rs *attr2refd-ids*]
    (when (seq a2rs)
      (let [[attr refs] (first a2rs)]
        (loop [ref refs]
          (when (seq ref)
            (create-edge! 'References attr
                          (or (*id2elem* (first ref))
                              (error (str "No element for id " (first ref)))))
            (recur (rest ref)))))
      (recur (rest a2rs)))))

(defn- content-handler
  []
  (letfn [(push-text []
            (when (and (= *state* :chars)
                       (some (complement #(Character/isWhitespace (char %)))
                             (str *sb*)))
              (let [t (create-vertex! *graph* 'Text)]
                (set-value! t :content (clojure.string/trim (str *sb*)))
                (create-edge! 'HasText *current* t))))]
    (proxy [DefaultHandler] []
      ;; ContentHandler
      (startElement [uri local-name qname ^Attributes attrs]
        (let [type (if (seq *stack*) 'Element 'RootElement)
              e (create-vertex! *graph* type)]
          (set-value! e :name qname)
          (handle-attributes e attrs)
          (when *current*
            (push-text)
            (create-edge! 'HasChild *current* e))
          (set! *stack* (conj *stack* *current*))
          (set! *current* e)
          (set! *state* :element))
        nil)
      (endElement [uri local-name qname]
        (push-text)
        (set! *current* (peek *stack*))
        (set! *stack* (pop *stack*))
        (set! *state* :between)
        nil)
      (characters [^chars ch start length]
        (when-not (= *state* :chars)
          (set! *sb* (new StringBuilder)))
        (let [^StringBuilder sb *sb*]
          (.append sb ch (int start) (int length))
          (set! *state* :chars))
        nil)
      (setDocumentLocator [locator])
      (startDocument [])
      (endDocument []
        (resolve-refs))
      (startPrefixMapping [prefix uri])
      (endPrefixMapping [prefix])
      (ignorableWhitespace [ch start length])
      (processingInstruction [target data])
      (skippedEntity [name])
      ;; ErrorHandler
      (error [^org.xml.sax.SAXParseException ex]
        (println "ERROR:" (.getMessage ex)))
      (fatalError [^org.xml.sax.SAXParseException ex]
        (println "FATAL ERROR:" (.getMessage ex)))
      (warning [^org.xml.sax.SAXParseException ex]
        (println "WARNING:" (.getMessage ex))))))

(defn- startparse-sax
  [^String uri ^DefaultHandler ch]
  (let [pfactory (SAXParserFactory/newInstance)]
    (.setNamespaceAware pfactory true)
    (-> pfactory
        .newSAXParser
        (.parse uri ch))))

;;# The main function

(defn xml2graph
  "Parse the XML file `f` into a TGraph conforming the generic XML schema.
  IDREF resolving, which is needed for creating References edges, works
  automatically only for XML files containing a DTD describing them.  If you
  want IDREFs resolved anyway, you have to provide an `attr-type-fn` that takes
  2 arguments, an element's qname and an attribute name, and then returns that
  attribute's type as string: ID, IDREF, IDREFS, or nil (meaning CDATA)."
  ([f]
     (xml2graph f false))
  ([f attr-type-fn]
     (binding [*graph* (create-graph
                        (load-schema (io/resource "xml-schema.tg")) f)
               *stack*   nil
               *current* nil
               *state*   :between
               *sb*      nil
               *id2elem* {}
               *attr2refd-ids* {}
               *attr-type-fn* (or attr-type-fn
                                  (constantly nil))]
       (startparse-sax f (content-handler))
       *graph*)))

