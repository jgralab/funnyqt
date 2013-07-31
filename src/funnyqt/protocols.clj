(ns funnyqt.protocols
  "Generic protocols extended upon many different types.")


;;# Describing Elements

(defprotocol IDescribable
  "A protocol for elements supporting describe."
  (describe [this]
    "Describes `this` attributed element or attributed element class."))

;;# Qualified Names

(defprotocol IQualifiedName
  "A protocol for qualified names."
  (qname [this]
    "Returns the qualified name of this named element's class or named element
  class as a symbol.  For collection domains, it returns a vector of symbols:
  [List Integer] where Integer is the base domain, or [Map Integer String]
  where Integer is the key domain and String is the value domain.  Of course,
  that may be recursive, so [Map Integer [List String]] corresponds to the java
  domain Map<Integer, List<String>>."))

;;# IAbstractness

(defprotocol IAbstractness
  "A protocol for checking if an element class is abstract."
  (abstract? [this]
    "Returns true, iff the element class is abstract.
  Implementations are provided for:

    - java.lang.Class: default impl in this namespace
    - de.uni_koblenz.jgralab.schema.GraphElementClass: funnyqt.tg
    - org.eclipse.emf.ecore.EClass: funnyqt.emf"))

(extend-protocol IAbstractness
  java.lang.Class
  (abstract? [this]
    (java.lang.reflect.Modifier/isAbstract (.getModifiers this))))

;;# Instance Check

(defprotocol IInstanceOf
  "A protocol for checking if an element is an instance of some meta-class."
  (is-instance? [object class]
    "Returns true, iff `object` is an instance of `class`.")
  (has-type? [object spec]
    "Returns true, iff `object`s type matches `spec`."))

;;# Generic Attribute Value Access

(defprotocol IAttributeValueAccess
  "A protocol for generically accessing attributes on some object."
  (aval [el attr]
    "Returns the value of `el`s `attr` attribute.
  `attr` is the attribute name given as keyword.")
  (set-aval! [el attr val]
    "Sets the value of `el`s `attr` attribute to `val`.
  `attr` is the attribute name given as keyword."))

;;# Generic Access to Vertices and EObjects

(defprotocol IElements
  (elements [model] [model type-spec]
    "Returns the lazy sequence of elements in `model` restricted by `type-spec`."))

;;# Generic creation of model elements

(defprotocol ICreateElement
  (create-element! [model cls]
    "Creates a new element of type `cls` in `model`."))

;;# Type Matcher

(defprotocol ITypeMatcher
  (type-matcher [model type-spec]
    "Returns a type-matcher function based on the metamodel of model.
  A type-matcher function accepts one object and returns true if the object
  matches the `type-spec`, or false otherwise.

  A type-spec may be composed of:

    - nil                     Every type is accepted
    - a predicate p           Accepted if (p obj) is true
    - a qname symbol
      - Foo                   Accepts objects of type Foo and subtypes
      - Foo!                  Accepts objects of exact type Foo
      - !Foo                  Accepts objects not of type Foo or subtypes
      - !Foo!                 Accepts objects not of exact type Foo
    - a metamodel type        Accepts instances of that type
    - a vector of the form    op is a logical operator (:or, :and, :nand, :nor, :xor),
      [op ts1 ts2 ...]        and ts1, ts2, etc are type-specs.  Accepts objects
                              whose type matches the individual type-specs ts1, ts2,
                              etc with the respective semantics of the logical
                              operator."))

;;# Deletion

(defprotocol IDeletable
  "A protocol for deleting elements."
  (delete! [this] [this recursive]
    "Deletes this element and returns it.  If `recursive` is true (default),
  delete also elements contained by `this`.  Of course, `recursive` has no
  meaning for edges.  Implementations are provided for Vertex, Edge, EObject,
  and collections thereof."))

(extend-protocol IDeletable
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


;;# IAdjacencies

(defprotocol IAdjacencies
  "A protocol for retrieving adjacent elements."
  (adj-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.")
  (adjs-internal [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.")
  (adj*-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.
  Doesn't error on intermediate unset roles.")
  (adjs*-internal [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.
  Doesn't error on intermediate unset roles."))

(defprotocol IModifyAdjacencies
  (set-adj!  [obj role obj])
  (set-adjs! [obj role objs])
  (add-adj!  [obj role obj])
  (add-adjs! [obj role objs]))

;;# Container

(defprotocol Container
  (container [this]
    "Returns the container of this element.
  A container is an element that references this element by some link with
  composition semantics."))

;;# (Meta-)Model Object predicates

(defprotocol IModelObject
  (model-object? [this]
    "Returns true if `this` is a supported model object."))

(extend-protocol IModelObject
  Object
  (model-object? [this] false)
  nil
  (model-object? [this] false))

(defprotocol IMetaModelObject
  (meta-model-object? [this]
    "Returns true if `this` is a supported meta model object."))

(extend-protocol IMetaModelObject
  Object
  (meta-model-object? [this] false)
  nil
  (meta-model-object? [this] false))

;;# Metamodel Protocols

(defprotocol IMMClasses
  (mm-classes [cls]
    "Returns all classes in the metamodel containing `cls`."))

(defprotocol IMMClass
  (mm-class [model-element] [model mm-class-sym]
    "Returns the given model-element's metamodel class,
  or the metamodel class named mm-class-sym (a symbol)."))

(defprotocol IMMDirectSuperClasses
  (mm-direct-super-classes [metamodel-type]
    "Returns the direct superclasses of metamodel-type."))

(defprotocol IMMSuperClassOf
  (mm-super-class? [super sub]
    "Return true iff super is a direct or indirect super class of sub.
  (mm-super-class? c c) is false."))
