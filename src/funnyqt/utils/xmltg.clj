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
attribute name as string.

  (xml2xml-graph \"example-without-dtd.xml\"
             #(cond ;; Here, we distingish only by 2nd arg, the attr name
                (= %2 \"id\")        \"ID\"
                (= %2 \"links\")     \"IDREF\"
                (= %2 \"children\")  \"IDREFS\"))"
  (:use funnyqt.tg)
  (:use funnyqt.protocols)
  (:use funnyqt.query.tg)
  (:use funnyqt.query)
  (:use [funnyqt.utils :only [errorf]])
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

(defn ns-prefix
  "Returns the namespace prefix of the given Element or Attribute.
  If that doesn't declare a namespace itself, returns the namespace of its
  container (recursively if needed)."
  ;; TODO: Is that correct for attributes?
  [e]
  (if-let [n (value e :nsPrefix)]
    n
    (when-let [p (adj e (if (has-type? e 'Element)
                          :parent :element))]
      (recur p))))

(defn ns-uri
  "Returns the namespace URI of the current element."
  [e]
  (let [root (the (vseq (graph e) 'RootElement))
        nspref (ns-prefix e)
        attrs (filter #(and (= (value % :nsPrefix) "xmlns")
                            (= (value % :name) nspref))
                      (adjs root :attributes))]
    (when (seq attrs)
      (value (the attrs) :value))))

(defn expanded-name
  "Returns the expanded name of the give Element, i.e., \"nsprefix:name\"."
  [e]
  (if-let [n (ns-prefix e)]
    (str n ":" (value e :name))
    (value e :name)))

(defn declared-name
  "Returns the name of the given Element or Attribute, possibly prefixed with its namespace.
  If it doesn't declare a namespace itself, the local name is returned."
  [e]
  (if-let [n (value e :nsPrefix)]
    (str n ":" (value e :name))
    (value e :name)))

(defn ^:private filter-by-name
  ;; n = foo:bar => check declared and expanded name
  ;; n = bar     => only check local name
  [n coll]
  (let [n (name n)
        name-matches (if (re-matches #".*:.*" n)
                       #(or (= n (declared-name %))
                            (= n (expanded-name %)))
                       #(= n (value % :name)))]
    (filter name-matches coll)))

(defn children
  "Returns the children Element vertices of Element vertex `e`.
  May be restricted to elements of name `n`, an expanded, declared, or local
  name."
  ([e]
     (adjs e :children))
  ([e n]
     (filter-by-name n (adjs e :children))))

(defn siblings
  "Returns the sibling Element vertices of Element or Text `e`.
  May be restricted to elements of the given `name`, an expanded, declared, or
  local name.  The result is a vector of two component seqs:

    [left-siblings right-siblings]

  left-siblings is a seq of siblings that occur left of (or above) `e`, and
  right-siblings is a seq of siblings that occur right of (or below) `e`.  `e`
  itself is not included."
  ([e]
     (siblings e nil))
  ([e n]
     (let [all (if n
                 (filter-by-name n (adjs (adj e :parent) :contents))
                 (adjs (adj e :parent) :contents))
           right (atom false)
           [l r] (partition-by (fn [s]
                                 (or @right
                                     (when (= s e)
                                       (swap! right (fn [& _] true)))))
                               all)]
       [l (next r)])))

(defn attribute-value
  "Returns the value of `elem`s xml attribute `attr-name`.
  First the `attr-name' is compared to the declared name.  If no attribute
  matches, the local names are compared, too."
  [elem attr-name]
  (if-let [attr (first (filter #(= (declared-name %) (name attr-name))
                               (adjs elem :attributes)))]
    (value attr :value)
    (if-let [attrs (seq (filter #(= (value % :name) (name attr-name))
                                (adjs elem :attributes)))]
      (value (the attrs) :value)
      (errorf "No such attribute %s at element %s." elem attr-name))))

(defn describe-element
  "Returns a map describing the given xml Element vertex `e`."
  ([e]
     (describe-element e true))
  ([e with-children]
     {:expanded-name (expanded-name e)
      :attrs (apply hash-map
                    (mapcat (fn [a]
                              [(keyword (expanded-name a)) (value a :value)])
                            (adjs e :attributes)))
      :children (if with-children
                  (map #(describe-element % false)
                       (adjs e :children))
                  :skipped)}))

;;# Parsing XML -> XMLGraph

;;## Internal vars

(def ^:dynamic ^:private *graph*)
(def ^:dynamic ^:private *stack*)
(def ^:dynamic ^:private *current*)
(def ^:dynamic ^:private *state*)   ;; :element :chars :between
(def ^:dynamic ^:private *sb*)
(def ^:dynamic ^:private *attr-type-fn*)
(def ^:dynamic ^:private *id2elem*) ;; map from ID to Element vertex

;; map from Attribute vertex to a vector of referenced element IDs (an attr can
;; reference multiple elements in terms of a IDREFS attr name)
(def ^:dynamic ^:private *attr2refd-ids*)
;; set of Attribute vertices whose value is an EMFFragmentPath expression that
;; has to be resolved after the graph has successfully been created
(def ^:dynamic ^:private *emf-fragment-path-attrs*)

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
                      (*attr-type-fn* (expanded-name elem) n v)
                      t)
                  av (create-vertex! *graph* 'Attribute)
                  split (str/split n #":")]
              (set-value! av :value v)
              (if (= (count split) 1)
                (set-value! av :name (split 0))
                (do
                  (set-value! av :nsPrefix (split 0))
                  (set-value! av :name (split 1))))
              (create-edge! (graph elem) 'HasAttribute elem av)
              (cond
               (= t "EMFFragmentPath")  (set! *emf-fragment-path-attrs*
                                              (conj *emf-fragment-path-attrs* av))
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
                              (errorf "No element for id %s." (first ref))))
            (recur (rest ref)))))
      (recur (rest a2rs)))))

(defn ^:private eval-emf-fragment-1 [start exps]
  (println exps)
  (if (seq exps)
    (let [^String f (first exps)]
      (recur
       (cond
        ;; @members
        (.startsWith f "@") (children start (symbol (subs f 1)))
        ;; @members.17
        (re-matches #"[0-9]+" f) (nth (if (vertex? start)
                                        (adjs start :children)
                                        start)
                                      (Long/valueOf f))
        ;; @members[firstName='Hugo'] where firstName is an attribute set as
        ;; eKeys feature for the members reference
        (re-matches #".*=.*" f)
        (let [m (apply hash-map
                       (mapcat (fn [kv]
                                 (let [[k v] (str/split kv #"=")]
                                   [k (subs v 1 (dec (count v)))]))
                               (str/split f #",[ ]*")))]
          (the (fn [e]
                 (every? (fn [[a v]]
                           (= (attribute-value e a) v))
                         m))
               start))
        ;; Oh, we don't know that...
        :else (errorf "Don't know how to handle EMF fragment '%s'." f))
       (rest exps)))
    start))

(defn ^:private eval-emf-fragment [g frag]
  (let [r (if (graph? g)
            (the (vseq g 'RootElement))
            g)]
    (eval-emf-fragment-1 r (next (str/split frag #"[/.\[\]]+")))))

(defn ^:private resolve-emf-fragment-paths
  ""
  []
  (let [r (the (vseq *graph* 'RootElement))]
    (doseq [a *emf-fragment-path-attrs*
            :let [fe (value a :value)]]
      (doseq [exp (str/split fe #" ")]
        (if-let [t (eval-emf-fragment r exp)]
          (create-edge! *graph* 'References a t)
          (binding [*out* *err*]
            (println (format "Couldn't resolve EMF Fragment Path '%s'." exp))))))))

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
        (let [name (if (seq *stack*) 'Element 'RootElement)
              e (create-vertex! *graph* name)
              split (str/split qname #":")]
          (if (= (count split) 1)
            (set-value! e :name (split 0))
            (do
              (set-value! e :nsPrefix (split 0))
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
        (resolve-refs)
        (resolve-emf-fragment-paths))
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
  3 arguments: an element's expanded name, an attribute name, and that
  attribute's value.  It should return that attribute's type as string: \"ID\",
  \"IDREF\", \"IDREFS\", \"EMFFragmentPath\", or nil (meaning CDATA)."
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
                                  (constantly nil))
               *emf-fragment-path-attrs* #{}]
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
                                       (declared-name a)
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
                             (declared-name elem)
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
      (.write *writer* (format "</%s>\n" (declared-name elem)))
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

#_(show-graph (xml2xml-graph "test/input/example.families"
                             (fn [en a v]
                               (when (re-matches #".*@.*" v)
                                 "EMFFragmentPath"))))

#_(show-graph (xml2xml-graph "/home/horn/example.families"
                             (fn [en a v]
                               (when (re-matches #".*@.*" v)
                                 "EMFFragmentPath"))))

#_(let [root (the (vseq (xml2xml-graph "test/input/example.families") 'RootElement))]
    (adjs root :attributes))

#_(xml-graph2xml (xml2xml-graph "test/input/example.families")
                 "/home/horn/example.families")

#_(attributes-str (the (vseq (xml2xml-graph "test/input/example.families") 'RootElement)))
