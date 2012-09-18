(ns funnyqt.utils.xmltg
  "Convert XML to DOM-like TGraphs.

The DOM-like schema looks like this:

  https://raw.github.com/jgralab/funnyqt/master/resources/xml-schema.png

To transform an XML document to a DOM-like TGraph, use

  (xml2xml-graph \"example.xml\")

If the XML file has a DTD which describes what attributes are IDs, IDREFs, and
IDREFS, all references will be represented as References edges in the Graph.

If the XML file doesn't contain a DTD, then you can provide a function that
receives an element name and an attribute name and should return the correct
attribute type as string.

  (xml2xml-graph \"example-without-dtd.xml\"
             #(cond ;; Here, we distingish only by 2nd arg, the attr name
                (= %2 \"id\")        \"ID\"
                (= %2 \"links\")     \"IDREF\"
                (= %2 \"children\")  \"IDREFS\"))"
  (:use funnyqt.tg)
  (:use funnyqt.protocols)
  (:use funnyqt.query.tg)
  (:use funnyqt.query)
  (:use [funnyqt.utils :only [error]])
  (:require [clojure.string :as str])
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

(defn ^:private xml-filter-by-type [type coll]
  (filter (fn [c]
            (= (value c :name) (name type)))
          coll))

(defn xml-namespace
  "Returns the namespace of the given Element or Attribute.
  If that doesn't declare a namespace itself, returns the namespace of its
  container (recursively if needed)."
  [e]
  (if-let [n (value e :namespace)]
    n
    (when-let [p (adj e (if (has-type? e 'Element)
                          :parent :element))]
      (recur p))))

(defn xml-expanded-name
  "Returns the expanded name of the give Element, i.e., \"nsprefix:name\"."
  [e]
  (if-let [n (xml-namespace e)]
    (str n ":" (value e :name))
    (value e :name)))

(defn xml-prefixed-name
  "Returns the name of the given Element prefixed with its namespace.
  If it doesn't declare a namespace, the local name is returned."
  [e]
  (if-let [n (value e :namespace)]
    (str n ":" (value e :name))
    (value e :name)))

(defn xml-children
  "Returns the children Element vertices of Element vertex `e`.
  May be restricted to elements of `type` given as symbol, keyword, or string."
  ([e]
     (adjs e :children))
  ([e type]
     (xml-filter-by-type type (adjs e :children))))

(defn xml-siblings
  "Returns the sibling Element vertices of Element or Text `e`.
  May be restricted to elements of the given `type`.
  The result is a vector of two component seqs:

    [left-siblings right-siblings]

  left-siblings is a seq of siblings that occur left of (or above) `e`, and
  right-siblings is a seq of siblings that occur right of (or below) `e`.  `e`
  itself is not included."
  ([e]
     (xml-siblings e nil))
  ([e type]
     (let [all (if type
                 (xml-filter-by-type type (adjs (adj e :parent) :contents))
                 (adjs (adj e :parent) :contents))
           right (atom false)
           [l r] (partition-by (fn [s]
                                 (or @right
                                     (when (= s e)
                                       (swap! right (fn [& _] true)))))
                               all)]
       [l (next r)])))

(defn xml-attr-value
  "Returns the value of `elem`s xml attribute `attr-name`, a local name."
  [elem attr-name]
  (let [attr (the #(= (value % :name) (name attr-name))
                  (adjs elem :attributes))]
    (value attr :value)))

(defn xml-describe-elem
  "Returns a map describing the given xml Element vertex `e`."
  ([e]
     (xml-describe-elem e true))
  ([e with-children]
     {:expanded-name (xml-expanded-name e)
      :attrs (apply hash-map
                    (mapcat (fn [a]
                              [(keyword (xml-expanded-name a)) (value a :value)])
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

;; map from Attribute vertex to a vector
;; of referenced element IDs (an attr can
;; reference multiple elements in terms of a
;; IDREFS attr type)
(def ^{:dynamic true :private true} *attr2refd-ids*)

;;## Internal functions

(defn ^:private handle-attributes
  [elem ^Attributes attrs]
  (when-not (zero? (.getLength attrs))
    (loop [i 0, l (.getLength attrs), as []]
      (if (== i l)
        (loop [a as]
          (when (seq a)
            (let [[n v t] (first a)
                  t (if (= t "CDATA")
                      (*attr-type-fn* (xml-expanded-name elem) n)
                      t)
                  av (create-vertex! *graph* 'Attribute)
                  split (str/split n #":")]
              (set-value! av :value v)
              (if (= (count split) 1)
                (set-value! av :name (split 0))
                (do
                  (set-value! av :namespace (split 0))
                  (set-value! av :name (split 1))))
              (create-edge! (graph elem) 'HasAttribute elem av)
              (cond
               (= t "ID")     (set! *id2elem* (assoc *id2elem* v elem))
               (= t "IDREF")  (set! *attr2refd-ids*
                                    (update-in *attr2refd-ids* [av]
                                               #(conj (vec %) v)))
               (= t "IDREFS") (set! *attr2refd-ids*
                                   (update-in *attr2refd-ids* [av]
                                              #(into (vec %) (clojure.string/split v #" "))))))
            (recur (rest a))))
        (recur (inc i)
               l
               (conj as [(.getLocalName attrs i)
                         (.getValue attrs i)
                         (.getType attrs i)]))))))
(defn ^:private resolve-refs
  "Create References edges for ID/IDREF[S]s collected while parsing."
  []
  (loop [a2rs *attr2refd-ids*]
    (when (seq a2rs)
      (let [[attr refs] (first a2rs)]
        (loop [ref refs]
          (when (seq ref)
            (create-edge! (graph attr)
                          'References attr
                          (or (*id2elem* (first ref))
                              (error (str "No element for id " (first ref)))))
            (recur (rest ref)))))
      (recur (rest a2rs)))))

(defn ^:private content-handler
  []
  (letfn [(push-text []
            (when (and (= *state* :chars)
                       (some (complement #(Character/isWhitespace (char %)))
                             (str *sb*)))
              (let [t (create-vertex! *graph* 'Text)]
                (set-value! t :content (clojure.string/trim (str *sb*)))
                (create-edge! (graph t) 'HasText *current* t))))]
    (proxy [DefaultHandler] []
      ;; ContentHandler
      (startElement [uri local-name qname ^Attributes attrs]
        (let [type (if (seq *stack*) 'Element 'RootElement)
              e (create-vertex! *graph* type)
              split (str/split qname #":")]
          (if (= (count split) 1)
            (set-value! e :name (split 0))
            (do
              (set-value! e :namespace (split 0))
              (set-value! e :name (split 1))))
          (handle-attributes e attrs)
          (when *current*
            (push-text)
            (create-edge! (graph e) 'HasChild *current* e))
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

(defn ^:private startparse-sax
  [^String uri ^DefaultHandler ch]
  (let [pfactory (SAXParserFactory/newInstance)]
    (-> pfactory
        .newSAXParser
        (.parse uri ch))))

;;## The user API

(defn xml2xml-graph
  "Parse the XML file `f` into a TGraph conforming the generic XML schema.
  IDREF resolving, which is needed for creating References edges, works
  automatically only for XML files containing a DTD describing them.  If you
  want IDREFs resolved anyway, you have to provide an `attr-type-fn` that takes
  2 arguments, an element's qname (expanded name) and an attribute name, and
  then returns that attribute's type as string: ID, IDREF, IDREFS, or
  nil (meaning CDATA)."
  ([f]
     (xml2xml-graph f false))
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

;;# Saving back to XML

;;## Internal vars and fns

(def ^:private ^:dynamic ^java.io.Writer *writer*)
(def ^:private ^:dynamic *indent-level*)
(def ^:private ^:dynamic *last-node-was-text*)

(defn ^:private xml-escape-chars ^String [val]
  (str/escape val {\" "&quot;", \' "&apos;", \& "&amp;", \< "&lt;", \> "&gt;"}))

(defn ^:private attributes-str [elem]
  (let [s (str/join " " (map (fn [a]
                               (format "%s=\"%s\""
                                       (xml-prefixed-name a)
                                       (xml-escape-chars (value a :value))))
                             (adjs elem :attributes)))]
    (if (seq s)
      (str " " s)
      s)))

(defn ^:private indent []
  (.write *writer* ^String (apply str (repeat (* 2 *indent-level*) \space))))

(defn ^:private emit-text [txt]
  (set! *last-node-was-text* true)
  (.write *writer* (xml-escape-chars (value txt :content))))

(defn ^:private emit-element [elem]
  (indent)
  (let [has-contents (seq (iseq elem 'HasContent :out))
        contains-text-first (when-let [i (first (iseq elem 'HasContent :out))]
                              (has-type? i 'HasText))]
    (.write *writer* (format "<%s%s%s>%s"
                             (xml-prefixed-name elem)
                             (attributes-str elem)
                             (if has-contents "" "/")
                             (if contains-text-first
                               "" "\n")))
    (when has-contents
      (binding [*indent-level* (inc *indent-level*)]
        (doseq [c (adjs elem :contents)]
          (if (has-type? c 'Text)
            (emit-text c)
            (emit-element c))))
      (when-not *last-node-was-text*
        (indent))
      (.write *writer* (format "</%s>\n" (xml-prefixed-name elem)))
      (set! *last-node-was-text* false))))

;;## The user API

(defn xml-graph2xml
  "Serializes the given XMLGraph `g` back to an XML document `f`."
  [g f]
  (with-open [w (io/writer f)]
    (binding [*writer* w
              *indent-level* 0
              *last-node-was-text* false]
      (.write *writer* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
      (doseq [re (vseq g 'RootElement)]
        (emit-element re))
      (.flush *writer*))))

#_(xml-graph2xml (xml2xml-graph "test/input/xmltg-example-with-dtd.xml")
                 "/home/horn/xmltg-example-with-dtd.xml")

#_(show-graph (xml2xml-graph "test/input/example.families"))

#_(let [root (the (vseq (xml2xml-graph "test/input/example.families") 'RootElement))]
    (adjs root :attributes))

#_(xml-graph2xml (xml2xml-graph "test/input/example.families")
                 "/home/horn/example.families")

#_(attributes-str (the (vseq (xml2xml-graph "test/input/example.families") 'RootElement)))
