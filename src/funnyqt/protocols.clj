(ns funnyqt.protocols
  "Generic protocols extended upon many different types.")


;;# Describing Elements

(defprotocol Describable
  "A protocol for elements supporting describe."
  (describe [this]
    "Describes `this` attributed element or attributed element class."))

;;# Qualified Names

(defprotocol QualifiedName
  "A protocol for qualified names."
  (qname [this]
    "Returns the qualified name of this named element's class or named element
  class as a symbol.  For collection domains, it returns a vector of symbols:
  [List Integer] where Integer is the base domain, or [Map Integer String]
  where Integer is the key domain and String is the value domain.  Of course,
  that may be recursive, so [Map Integer [List String]] corresponds to the java
  domain Map<Integer, List<String>>."))

;;# Abstractness

(defprotocol Abstractness
  "A protocol for checking if an element class is abstract."
  (abstract? [this]
    "Returns true, iff the element class is abstract.
  Implementations are provided for:

    - java.lang.Class: default impl in this namespace
    - de.uni_koblenz.jgralab.schema.GraphElementClass: funnyqt.tg
    - org.eclipse.emf.ecore.EClass: funnyqt.emf"))

(extend-protocol Abstractness
  java.lang.Class
  (abstract? [this]
    (java.lang.reflect.Modifier/isAbstract (.getModifiers this))))

;;# Instance Check

(defprotocol InstanceOf
  "A protocol for checking if an element is an instance of some meta-class."
  (is-instance? [object class]
    "Returns true, iff `object` is an instance of `class`.")
  (has-type? [object spec]
    "Returns true, iff `object`s type matches `spec`."))

;;# Deletion

(defprotocol Deletable
  "A protocol for deleting elements."
  (delete! [this] [this recursive]
    "Deletes this element and returns it.  If `recursive` is true (default),
  delete also elements contained by `this`.  Of course, `recursive` has no
  meaning for edges.  Implementations are provided for Vertex, Edge, EObject,
  and collections thereof."))

(extend-protocol Deletable
  java.util.Collection
  (delete!
    ([this]
       (doseq [x this]
         (delete! x))
       this)
    ([this recursive]
       (doseq [x this]
         (delete! x recursive))
       this)))


;;# Adjacencies

(defprotocol Adjacencies
  "A protocol for retrieving adjacent elements."
  (adj-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.")
  (adjs-internal [this] [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.")
  (adj*-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.
  Doesn't error on intermediate unset roles.")
  (adjs*-internal [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.
  Doesn't error on intermediate unset roles."))

