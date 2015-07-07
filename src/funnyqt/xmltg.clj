(ns funnyqt.xmltg
  "Convert XML to DOM-like TGraphs.

The DOM-like schema looks like this:

  https://raw.github.com/jgralab/funnyqt/master/resources/xml-schema.png

To transform an XML document to a DOM-like TGraph, use

  (xml2xml-graph \"example.xml\")

If the XML file has a DTD which describes what attributes are IDs, IDREFs, and
IDREFS, all references will be represented as References edges in the Graph.

If the XML file has no DTD, you can influence the resolution by providing an
`attr-type-fn` and/or an `text-type-fn` as documented in `xml2xml-graph`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [funnyqt
             [generic :as g]
             [query :as q]
             [tg :as tg]
             [utils :as u]])
  (:import de.uni_koblenz.jgralab.Vertex
           javax.xml.namespace.QName
           [javax.xml.stream XMLInputFactory XMLStreamConstants]
           [javax.xml.stream.events Attribute Characters EndDocument EndElement
            Namespace StartDocument StartElement XMLEvent]))

;;# XML Graph Utils

(defn ^:private ns-get [what e]
  (if-let [n (tg/value e what)]
    n
    (when-let [p (g/adj e (if (g/has-type? e 'Element)
                            :parent :element))]
      (recur what p))))

(def ^{:doc "Returns the namespace prefix of the given Element or Attribute.
  If that doesn't declare a namespace itself, returns the namespace of its
  container (recursively if needed)."
       :arglists '([e])}
  ns-prefix (partial ns-get :nsPrefix))

(def ^{:doc "Returns the namespace URI of the given Element or Attribute.
  Lookup is done recursively."
       :arglists '([e])}
  ns-uri (partial ns-get :nsURI))

(defn expanded-name
  "Returns the expanded name of the give Element, i.e., \"nsprefix:name\"."
  [e]
  (if-let [n (ns-prefix e)]
    (str n ":" (tg/value e :name))
    (tg/value e :name)))

(defn declared-name
  "Returns the name of the given Element or Attribute, possibly prefixed with its namespace.
  If it doesn't declare a namespace itself, the local name is returned."
  [e]
  (if-let [n (tg/value e :nsPrefix)]
    (str n ":" (tg/value e :name))
    (tg/value e :name)))

(defn qualified-name
  "Returns the qualified name of Element `e` in the form \"{nsURI}localName\",
  or just \"localName\" if it's not namespaced."
  [e]
  (if-let [u (ns-uri e)]
    (str "{" u "}" (tg/value e :name))
    (tg/value e :name)))

(defn ^:private filter-by-name
  ;; n = {nsURI}bar => check qualified name
  ;; n = foo:bar    => check declared and expanded name
  ;; n = bar        => only check local name
  [n coll]
  (let [n (name n)
        name-matches (cond
                      (re-matches #"\{.*\}.*" n) #(= n (qualified-name %))
                      (re-matches #".*:.*" n) #(or (= n (declared-name %))
                                                   (= n (expanded-name %)))
                      :else #(= n (tg/value % :name)))]
    (filter name-matches coll)))

(defn children
  "Returns the children Element vertices of Element vertex `e`.
  May be restricted to elements of type `qn`, a qualified name (see
  `qualified-name`), an expanded (see `expanded-name`) or declared name (see
  `declared-name`), or a local name."
  ([e]
     (g/adjs e :children))
  ([e qn]
     (filter-by-name qn (g/adjs e :children))))

(defn siblings
  "Returns the sibling Element vertices of Element or CharContent `e`.
  May be restricted to elements of the given type `qn`, , a qualified name (see
  `qualified-name`), an expanded (see `expanded-name`) or declared name (see
  `declared-name`), or a local name.

  The result is a vector of two component seqs:

    [left-siblings right-siblings]

  left-siblings is a seq of siblings that occur left of (or above) `e`, and
  right-siblings is a seq of siblings that occur right of (or below) `e`.  `e`
  itself is not included."
  ([e]
     (siblings e nil))
  ([e qn]
     (let [all (if qn
                 (filter-by-name qn (g/adjs (g/adj e :parent) :contents))
                 (g/adjs (g/adj e :parent) :contents))
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
                               (g/adjs elem :attributes)))]
    (tg/value attr :value)
    (if-let [attrs (seq (filter #(= (tg/value % :name) (name attr-name))
                                (g/adjs elem :attributes)))]
      (tg/value (q/the attrs) :value)
      (u/errorf "No such attribute %s at element %s." elem attr-name))))

(defn describe-element
  "Returns a map describing the given xml Element vertex `e`."
  ([e]
     (describe-element e true))
  ([e with-children]
     {:expanded-name (expanded-name e)
      :attrs (apply hash-map
                    (mapcat (fn [a]
                              [(keyword (expanded-name a)) (tg/value a :value)])
                            (g/adjs e :attributes)))
      :children (if with-children
                  (map #(describe-element % false)
                       (g/adjs e :children))
                  :skipped)}))

;;# Parsing XML -> XMLGraph

;;## Internal vars

(def ^:dynamic ^:private *graph*)
(def ^:dynamic ^:private *stack*)
(def ^:dynamic ^:private *current*)
(def ^:dynamic ^:private *attr-type-fn*)
(def ^:dynamic ^:private *text-type-fn*)
(def ^:dynamic ^:private *id2elem*) ;; map from ID to Element vertex

;; map from Referent vertex to a vector of referenced element IDs (an attr or
;; CharContent can reference multiple elements in terms of a IDREFS attr name)
(def ^:dynamic ^:private *referent2refed-ids*)
;; set of Attribute vertices whose value is an EMFFragmentPath expression that
;; has to be resolved after the graph has successfully been created
(def ^:dynamic ^:private *emf-fragment-path-attrs*)

(defonce ^:private xml-stream-constants-map
  (apply hash-map
         (mapcat (fn [^java.lang.reflect.Field f]
                   [(.get f nil) (.getName f)])
                 (seq (.getDeclaredFields XMLStreamConstants)))))

;;## Internal functions

(defn ^:private resolve-refs
  "Create References edges for ID/IDREF[S]s collected while parsing."
  []
  (loop [a2rs *referent2refed-ids*]
    (when (seq a2rs)
      (let [[referent refs] (first a2rs)]
        (loop [ref refs]
          (when (seq ref)
            (tg/create-edge! *graph*
                             'References referent
                             (or (*id2elem* (first ref))
                                 (u/errorf "No element for id %s." (first ref))))
            (recur (rest ref)))))
      (recur (rest a2rs)))))

(defn ^:private eval-emf-fragment-1 [start exps]
  (if (seq exps)
    (let [^String f (first exps)]
      (recur
       (cond
        ;; @members
        (.startsWith f "@") (children start (subs f 1))
        ;; @members.17
        (re-matches #"[0-9]+" f) (nth (if (tg/vertex? start)
                                        (g/adjs start :children)
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
          (q/the (fn [e]
                   (every? (fn [[a v]]
                             (= (attribute-value e a) v))
                           m))
                 start))
        ;; Oh, we don't know that...
        :else (u/errorf "Don't know how to handle EMF fragment '%s'." f))
       (rest exps)))
    start))

(defn ^:private get-root-element [g]
  (let [roots (tg/vseq g 'RootElement)]
    (when (fnext roots)
      (binding [*out* *err*]
        (println "There are multiple roots. Cannot resolve elements"
                 "reliably! Assuming the first one is the right one."
                 "Probably you parsed many XMI files? That won't work.")))
    (first roots)))

(defn ^:private eval-emf-fragment [r frag]
  (eval-emf-fragment-1 r (next (str/split frag #"[/.\[\]]+"))))

(defn ^:private resolve-emf-fragment-paths []
  (when (seq *emf-fragment-path-attrs*)
    (let [r (get-root-element *graph*)]
      (u/doseq+ [a *emf-fragment-path-attrs*
                 :let [fe (tg/value a :value)]]
        (u/doseq+ [exp (str/split fe #" ")]
          (if-let [t (eval-emf-fragment r exp)]
            (tg/create-edge! *graph* 'References a t)
            (binding [*out* *err*]
              (println (format "Couldn't resolve EMF Fragment Path '%s'." exp)))))))))

(defn ^:private handle-start-document [^StartDocument ev]
  nil)

(defn ^:private handle-end-document [^EndDocument ev]
  (resolve-refs)
  (resolve-emf-fragment-paths))

(defn ^:private handle-type-semantics [t id-vertex referent-vertex val]
  (condp = t
    "EMFFragmentPath"  (set! *emf-fragment-path-attrs*
                             (conj *emf-fragment-path-attrs* referent-vertex))
    "ID"     (set! *id2elem* (assoc *id2elem* val id-vertex))
    "IDREF"  (set! *referent2refed-ids*
                   (update-in *referent2refed-ids* [referent-vertex]
                              #(conj (vec %) val)))
    "IDREFS" (set! *referent2refed-ids*
                   (update-in *referent2refed-ids* [referent-vertex]
                              #(into (vec %) (remove empty? (str/split val #"\s+")))))
    nil))

(defn ^:private handle-attribute [elem ^Attribute a]
  (let [qn (.getName a)
        val (.getValue a)
        t (if *attr-type-fn*
            (*attr-type-fn* (qualified-name elem) (str qn) val)
            (.getDTDType a))
        av (tg/create-vertex! *graph* 'Attribute)]
    (tg/set-value! av :value val)
    (tg/set-value! av :name (.getLocalPart qn))
    (let [p (.getPrefix qn)
          u (.getNamespaceURI qn)]
      (when (seq p)
        (tg/set-value! av :nsPrefix p))
      (when (seq u)
        (tg/set-value! av :nsURI u)))
    (tg/create-edge! *graph* 'HasAttribute elem av)
    (handle-type-semantics t elem av val)))

(defn ^:private handle-start-element [^StartElement ev]
  (let [type (if (seq *stack*) 'Element 'RootElement)
        e (tg/create-vertex! *graph* type)
        qn (.getName ev)]
    (tg/set-value! e :name (.getLocalPart qn))
    (let [p (.getPrefix qn)
          u (.getNamespaceURI qn)]
      (when (seq p)
        (tg/set-value! e :nsPrefix p))
      (when (seq u)
        (tg/set-value! e :nsURI u)))
    (u/doseq+ [a (iterator-seq (.getAttributes ev))]
      (handle-attribute e a))
    (let [nsmap (reduce (fn [m ^Namespace n]
                          (assoc m
                            (.getPrefix n)
                            (.getNamespaceURI n)))
                        {} (iterator-seq (.getNamespaces ev)))]
      (when (seq nsmap)
        (tg/set-value! e :declaredNamespaces nsmap)))
    (when *current*
      (tg/create-edge! (tg/graph e) 'HasChild *current* e))
    (set! *stack* (conj *stack* *current*))
    (set! *current* e)))

(defn ^:private handle-end-element [^EndElement ev]
  (set! *current* (peek *stack*))
  (set! *stack* (pop *stack*)))

(defn ^:private handle-characters [^Characters ev]
  (when-not (or (.isIgnorableWhiteSpace ev)
                (.isWhiteSpace ev))
    (let [txt (tg/create-vertex! *graph* 'CharContent)
          data (.getData ev)
          parent (g/adj *current* :parent)
          t (if *text-type-fn*
              (*text-type-fn* (qualified-name parent) (qualified-name *current*) data)
              "CDATA")]
      (tg/set-value! txt :content data)
      (tg/create-edge! *graph* 'HasCharContent *current* txt)
      (when *text-type-fn*
        (handle-type-semantics t parent txt data)))))

(defn ^:private parse [f]
  (let [xer (.createXMLEventReader
             (doto (XMLInputFactory/newFactory)
               (.setProperty "javax.xml.stream.isCoalescing" true))
             (io/input-stream f))
        conts (iterator-seq xer)]
    (u/doseq+ [^XMLEvent ev conts]
      (condp = (.getEventType ev)
        XMLStreamConstants/START_DOCUMENT (handle-start-document ev)
        XMLStreamConstants/END_DOCUMENT   (handle-end-document ev)
        XMLStreamConstants/START_ELEMENT  (handle-start-element ev)
        XMLStreamConstants/END_ELEMENT    (handle-end-element ev)
        XMLStreamConstants/CHARACTERS     (handle-characters ev)
        XMLStreamConstants/DTD            nil
        (binding [*out* *err*]
          (println "Unhandled XMLEvent of type"
                   (xml-stream-constants-map (.getEventType ev))))))))

;;## The user API

(defn xml2xml-graph
  "Parse the XML file `f` into a TGraph conforming the generic XML schema.
  IDREF resolving, which is needed for creating References edges, works
  automatically only for XML files containing a DTD describing them.

  If you want IDREFs resolved anyway, you have to provide an `attr-type-fn`
  that takes 3 arguments: an element's expanded name, an attribute name, and
  that attribute's value.  It should return that attribute's type as string:
  \"ID\", \"IDREF\", \"IDREFS\", \"EMFFragmentPath\", or nil (meaning CDATA).

  Also, some poor XML documents model references with custom elements, like

    <person>
      <pid>PID</pid>
      <spouse>PID1</spouse>
      <children>PID2 PID3</children>
    </person>

  So here, the pid element's text has the meaning of an ID attribute, the
  spouse element's text has the meaning of an IDREF attribute, and the
  children's text has the meaning of an IDREFS attribute.  To resolve
  references with such documents, you can specify an `text-type-fn` which
  receives 3 arguments: the parent Element's qualified name (see
  `qualified-name`, \"person\" in the example), the current Element's qualified
  name (\"pid\", \"spouse\", or \"children\" in the example) and the text
  value.  It should return the type of the text as string: \"ID\", \"IDREF\",
  \"IDREFS\", \"EMFFragmentPath\", or nil (meaning CDATA).

  By default, this function returns a new XML TGraph.  However, you can also
  provide a pre-existing `xml-graph` in which case new elements will be created
  in that instead of a new graph.  That's convenient if you want to put the
  contents of multiple XML files in one single graph.  Of course, then there
  will also be multiple RootElements."
  ([f]
     (xml2xml-graph f nil nil nil))
  ([f attr-type-fn]
     (xml2xml-graph f attr-type-fn nil nil))
  ([f attr-type-fn text-type-fn]
     (xml2xml-graph f attr-type-fn text-type-fn nil))
  ([f attr-type-fn text-type-fn xml-graph]
     (binding [*graph* (or xml-graph
                           (tg/new-graph
                            (tg/load-schema (io/resource "xml-schema.tg")) f))
               *stack*   nil
               *current* nil
               *id2elem* {}
               *referent2refed-ids* {}
               *attr-type-fn* attr-type-fn
               *text-type-fn* text-type-fn
               *emf-fragment-path-attrs* #{}]
       (parse f)
       *graph*)))

;;# Saving back to XML

;;## Internal vars and fns

(def ^:private ^:dynamic ^java.io.Writer *writer*)
(def ^:private ^:dynamic *indent-level*)
(def ^:private ^:dynamic *last-node-was-text*)

(defn ^:private xml-escape-chars ^java.lang.String [val]
  (str/escape val {\" "&quot;", \' "&apos;", \& "&amp;", \< "&lt;", \> "&gt;"}))

(defn ^:private attributes-str [elem]
  (let [s (str/join " " (map (fn [a]
                               (format "%s=\"%s\""
                                       (declared-name a)
                                       (xml-escape-chars (tg/value a :value))))
                             (g/adjs elem :attributes)))]
    (if (seq s)
      (str " " s)
      s)))

(defn ^:private indent []
  (.write *writer* ^String (str/join (repeat (* 2 *indent-level*) \space))))

(defn ^:private emit-text [txt]
  (set! *last-node-was-text* true)
  (.write *writer* (xml-escape-chars (tg/value txt :content))))

(defn ^:private namespaces-str [elem]
  (let [s (str/join " " (map (fn [[p u]]
                               (str "xmlns" (when p (str ":" p))
                                    "=\"" u "\""))
                             (seq (tg/value elem :declaredNamespaces))))]
    (if (seq s) (str " " s) s)))

(defn ^:private emit-element [elem]
  (indent)
  (let [has-contents (seq (tg/iseq elem 'HasContent :out))
        contains-text-first (when-let [i (first (tg/iseq elem 'HasContent :out))]
                              (g/has-type? i 'HasCharContent))]
    (.write *writer* (format "<%s%s%s%s>%s"
                             (declared-name elem)
                             (namespaces-str elem)
                             (attributes-str elem)
                             (if has-contents "" "/")
                             (if contains-text-first
                               "" "\n")))
    (when has-contents
      (binding [*indent-level* (inc *indent-level*)]
        (u/doseq+ [c (g/adjs elem :contents)]
          (if (g/has-type? c 'CharContent)
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
      (u/doseq+ [re (tg/vseq g 'RootElement)]
        (emit-element re))
      (.flush *writer*))))
