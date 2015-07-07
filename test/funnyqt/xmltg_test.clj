(ns funnyqt.xmltg-test
  (:require [funnyqt
             [tg :refer :all]
             [xmltg :refer :all]])
  (:use clojure.test))

(deftest test-example-with-dtd
  (let [g (xml2xml-graph "test/input/xmltg-example-with-dtd.xml")]
    (is (== 123 (vcount g)))
    (is (== 1   (vcount g 'RootElement)))
    (is (== 55  (vcount g 'Element)))
    (is (== 75  (vcount g 'Node)))
    (is (== 48  (vcount g 'Attribute)))
    (is (== 20  (vcount g 'CharContent)))
    (is (== 142 (ecount g)))
    (is (== 20 (ecount g 'References)))
    ;; Only the root element has no incoming HasContent edges
    (is (= (vseq g 'RootElement)
           (filter #(== 0 (degree % 'HasContent :in))
                   (vseq g 'Element))))
    ;; Any attribute has exactly one incoming HasAttribute edge
    (is (empty? (filter #(not= 1 (degree % 'HasAttribute :in))
                        (vseq g 'Attribute))))))

(defn ^:private asserts-for-idrefs-example
  [g]
  (is (== 112 (vcount g)))
  (is (== 1   (vcount g 'RootElement)))
  (is (== 49  (vcount g 'Element)))
  (is (== 69  (vcount g 'Node)))
  (is (== 43  (vcount g 'Attribute)))
  (is (== 20  (vcount g 'CharContent)))
  (is (== 131 (ecount g)))
  (is (== 20 (ecount g 'References)))
  ;; Only the root element has no incoming HasContent edges
  (is (= (vseq g 'RootElement)
         (filter #(== 0 (degree % 'HasContent :in))
                 (vseq g 'Element))))
  ;; Any attribute has exactly one incoming HasAttribute edge
  (is (empty? (filter #(not= 1 (degree % 'HasAttribute :in))
                      (vseq g 'Attribute))))
  ;; One attribute with 6 outgoing refs
  (is (== 1 (count (filter #(== 6 (degree % 'References :out))
                           (vseq g 'Attribute))))))

(deftest test-example-with-dtd-and-IDREFS
  (let [g (xml2xml-graph "test/input/xmltg-example-with-dtd-and-IDREFS.xml")]
    (asserts-for-idrefs-example g)))

(deftest test-example-without-dtd
  (let [g (xml2xml-graph "test/input/xmltg-example-without-dtd.xml"
                         ;; This is a function that gets 3 parameters: an
                         ;; elements expanded name, an attribute (declared)
                         ;; name, and the attribute value.  Given that, it
                         ;; should return the attribute's correct type: ID,
                         ;; IDREF, IDREFS, EMFFragmentPath, or nil/CDATA
                         ;; otherwise.
                         (fn [exp-name aname aval]
                           (cond
                            ;; In this simple example, the ID-typed attrs are named
                            ;; ID, the IDREF-typed attributes are named IDREF, and
                            ;; the IDREFS-typed attribute (of the element type
                            ;; FAMILY) in named CHILDREN.  Here, we can simply
                            ;; ignore the element qualified name given to the
                            ;; function as first parameter.
                            (= aname "ID")        "ID"
                            (= aname "IDREF")     "IDREF"
                            (= aname "CHILDREN")  "IDREFS")))]
    (asserts-for-idrefs-example g)))

(deftest test-example-with-semantically-importants-text
  (let [g (xml2xml-graph "test/input/xml-example-with-semantically-important-text.xml"
                         nil
                         (fn [p c v]
                           (condp = c
                             "pid"      "ID"
                             "spouse"   "IDREF"
                             "children" "IDREFS"
                             "parents"  "IDREFS")))]
    #_(show-graph g)
    (is (= 18 (vcount g 'Element)))
    (is (= 12 (vcount g 'CharContent)))
    (is (= 14 (ecount g 'References)))))

