(ns funnyqt.generic-protocols
  "Generic functions like quantified expressions."
  (:use [funnyqt.utils :only [add-long-doc!]]))

(add-long-doc! "TODO")

;;* Code

;;** Describing Elements

(defprotocol Describable
  "A protocol for elements supporting describe."
  (describe [this]
    "Describes `this' attributed element or attributed element class."))

;;** Qualified Names

(defprotocol QualifiedName
  "A protocol for qualified names."
  (qname [this]
    "Returns the qualified name of this named element's class or named element
    class as a symbol.  For collection domains, it returns a vector of symbols:
    [List Integer] where Integer is the base domain, or [Map Integer String]
    where Integer is the key domain and String is the value domain.  Of course,
    that may be recursive, so [Map Integer [List String]] corresponds to the
    java domain Map<Integer, List<String>>."))

;;** Abstractness

(defprotocol Abstractness
  "A protocol for checking if a element class is abstract."
  (abstract? [this]
    "Returns true, iff the element class is abstract."))
