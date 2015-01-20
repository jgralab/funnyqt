(ns funnyqt.generic
  "Generic protocols extended upon many different types, and generic functions."
  (:require [clojure.string   :as str]
            [funnyqt.internal :as i]
            [funnyqt.utils    :as u]
            inflections.core))

;;# Describing Elements

(defprotocol IDescribe
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

;;# Uniqe Names

(defprotocol IUniqueName
  (uname [this]
    "Returns the unique name of element class `this` as a symbol.
  The unique name is the class' simple name if it is unique in the complete
  metamodel, else, i.e., there's a foo.X and a bar.X class in the metamodel, it
  is the qualified name."))

(defn escaped-uname-str
  "Returns the unique name of metamodel `cls` as a string where dots are
  replaced by $ (in the fully qualified case).
  Utility function used by the API generator macros."
  [cls]
  (str/replace (str (uname cls)) \. \$))

;;# Unset properties

(defprotocol IUnset
  (unset? [this prop]
    "Returns true iff the property `prop` (given as keyword) is unset."))

;;# Instance Check

;;## Type Matcher

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

(defn has-type?
  "Returns true if model element `el` matches the type specification `type-spec`.
  See protocol funnyqt.generic/ITypeMatcher."
  [el type-spec]
  ((type-matcher el type-spec) el))

(defprotocol IInstanceOf
  "A protocol for checking if an element is an instance of some meta-class."
  (is-instance? [object class]
    "Returns true, iff `object` is an instance of `class`."))

;;## type-case

(defmacro type-case
  "Takes an element `elem` (a GraphElement or EObject) and a set of `clauses`.
  Every clause is a pair of the form:

    type-spec result-expr

  The type-specs are tested one after the other, and if a type-spec matches the
  type of `elem`, the return value of type-case is the result-expr paired with
  the succeeding type-spec.  Note that the type-specs must not be quoted!

  A single default-expr may follow the pairs.  If no type-spec matches, the
  return value of type-case is the value of that default expression.  If there
  is no default expression and no type-spec matches, an
  IllegalArgumentException is thrown.

  Example:

    (type-case obj
      TypeA (do-a-stuff obj)
      TypeB (do-b-stuff obj)
      (do-default-stuff obj))

  Note that the type-specs must not be quoted."
  [elem & clauses]
  (let [normal-clauses (partition 2 clauses)
        default-clause (when (odd? (count clauses))
                         (last clauses))]
    `(condp (fn [t# e#] (has-type? e# t#)) ~elem
       ~@(mapcat (fn [[type exp]]
                   `['~type ~exp])
                 normal-clauses)
       ~@(when default-clause
           `[~default-clause]))))

;;# Generic Attribute Value Access

(defprotocol IAttributeValueAccess
  "A protocol for generically accessing attributes on some object."
  (aval [el attr]
    "Returns the value of `el`s `attr` attribute.
  `attr` is the attribute name given as keyword.")
  (set-aval! [el attr val]
    "Sets the value of `el`s `attr` attribute to `val`.
  `attr` is the attribute name given as keyword."))

;;# Generic Access to Elements and Relationships

(defprotocol IElements
  (elements [model] [model type-spec]
    "Returns the lazy sequence of elements in `model` restricted by `type-spec`."))

(defprotocol IRelationships
  (relationships [model] [modes type-spec]
    "Returns the lazy seq of relationships in `model` restricted by `type-spec`.
  This is intended to be extended upon models with first-class relationships such as
  JGraLab where `type-spec` is a type specification on relationship classes."))

(defprotocol IIncidentRelationships
  (incident-relationships [elem] [elem type-spec] [elem type-spec dir-spec]
    "Returns the lazy seq of relationships incident to `elem`.
  May be restricted by the type specification `type-spec` and the direction
  specification `dir-spec` (:in, :out, or :inout)."))

(defprotocol IRelationshipSourceTarget
  (source [rel]
    "Returns the element where this `rel` starts.")
  (target [rel]
    "Returns the element where this `rel` ends."))

;;# Generic access to enumeration constants

(defprotocol IEnumConstant
  (enum-constant [m const]
    "Returns the enumeration constant with the qualified name `const` in the
  metamodel of model `m`.
  In case of EMF, `m` is ignored."))

;;# Generic creation of model elements

(defprotocol ICreateElement
  (create-element! [model cls] [model cls prop-map]
    "Creates a new element of type `cls` in `model`.
  Properties are set according to `prop-map`, a map from property name keywords
  to property values."))

(defprotocol ICreateRelationship
  (create-relationship!
    [model cls src-elem trg-elem]
    [model cls src-elem trg-elem attr-map]
    "Creates a new relationship of type `cls` in `model` connecting `src-elem`
  to `trg-elem`.  The valid values for `cls` are framework specific.  For
  TGraphs it is a symbol denoting an EdgeClass name, for EMF it is a keyword
  denoting an EReference name.  The return value is also framework specific.
  For TGraphs it is an Edge, for EMF it is the tuple [src-elem trg-elem].
  `attr-map` is a map from attribute names (as keywords) to values to be set.
  Clearly, this is unsupported by frameworks without explicit relationships with
  attributes."))

;;# Deletion

(defprotocol IDelete
  "A protocol for deleting elements."
  (delete! [this] [this recursively]
    "Deletes this element and returns it.  If `recursively` is true (default),
  delete also elements contained by `this`.  Of course, `recursively` has no
  meaning for edges.  Implementations are provided for Vertex, Edge, EObject,
  and collections thereof."))

(extend-protocol IDelete
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

;;# Adacencies

(defn ^:private zero-or-one [s]
  (if (next s)
    (u/errorf "More than one adjacent element found: %s" s)
    (first s)))

(defn ^:private adjs-transducer-1 [roles allow-unknown-ref single-valued]
  (let [step-fn (fn [r]
                  (fn [v]
                    (i/adjs-internal v r allow-unknown-ref single-valued)))]
    (if (= 1 (count roles))
      (mapcat (step-fn (first roles)))
      (apply comp (for [r roles]
                    (mapcat (step-fn r)))))))

(defn adjs-transducer
  "Returns a transducer traversing `role` and `roles` one after the other.
  Errors if some role is not defined for an element."
  [role & roles]
  (adjs-transducer-1 (cons role roles) false false))

(defn adjs*-transducer
  "Returns a transducer traversing `role` and `roles` one after the other."
  [role & roles]
  (adjs-transducer-1 (cons role roles) true false))

(defn adj
  "Traverses single-valued `role` and more `roles` starting at `elem`.
  Returns the target object.
  Errors if a role is undefined, intermediate targets are nil, or there are
  more elements that can be reached that way."
  [elem role & roles]
  (zero-or-one (sequence (adjs-transducer-1 (cons role roles) false true)
                         [elem])))

(defn adj*
  "Like `adj`, but doesn't error if some role is not defined.  In that case, it
  simply returns nil."
  [elem role & roles]
  (zero-or-one (sequence (adjs-transducer-1 (cons role roles) true true)
                         [elem])))

(defn adjs
  "Traverses `role` and more `roles` starting at `elem`.
  Returns a vector of target objects.
  Errors if a role is undefined."
  [elem role & roles]
  (into [] (adjs-transducer-1 (cons role roles) false false)
        [elem]))

(defn adjs*
  "Like `adjs`, but doesn't error if some role is not defined.  In that case,
  it simply returns the empty vector."
  [elem role & roles]
  (into [] (adjs-transducer-1 (cons role roles) true false)
        [elem]))

;;# INeighbors

(defprotocol INeighbors
  (neighbors [elem]
    "Returns the sequence of `elem`s neighbors.
  Neighbors are all elements that are referenced by `elem` using an arbitrary
  reference."))

;;# IModifyAdjacencies

(defprotocol IModifyAdjacencies
  (set-adj!  [obj role robj]
    "Sets the single-valued `role` of `obj` to `robj`.")
  (set-adjs! [obj role robjs]
    "Sets the multi-valued `role` of `obj` to `robjs` (a collection of model
  elements).")
  (add-adj!  [obj role robj]
    "Adds `robj` to `obj`s `role`.")
  (add-adjs! [obj role robjs]
    "Adds all `robjs` to `obj`s `role`."))

;;# IContainer

(defprotocol IContainer
  (container [this]
    "Returns the container of this element.
  A container is an element that references `this` element by some link with
  containment semantics."))

;;# IContents

(defprotocol IContents
  (contents [this] [this ts]
    "Returns the seq of `this` element's direct contents possibly restricted by
    the type-spec `ts`."))

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

;;# Metamodel Access & Metamodel Protocols

(defonce ^{:doc "A map from regular expressions to functions to load a metamodel."}
  mm-load-handlers
  {})

(defn mm-load [file]
  (loop [handlers mm-load-handlers]
    (if (seq handlers)
      (let [[rx handler] (first handlers)]
        (if (re-matches rx file)
          (handler file)
          (recur (rest handlers))))
      (u/errorf "No mm-load-handler for %s." file))))

(defprotocol IMMAbstract
  "A protocol for checking if an element class is abstract."
  (mm-abstract? [this]
    "Returns true, iff the element class is abstract.
  Implementations are provided for:

    - java.lang.Class: default impl in this namespace
    - de.uni_koblenz.jgralab.schema.GraphElementClass: funnyqt.tg
    - org.eclipse.emf.ecore.EClass: funnyqt.emf"))

(extend-protocol IMMAbstract
  java.lang.Class
  (mm-abstract? [this]
    (java.lang.reflect.Modifier/isAbstract (.getModifiers this))))

(defprotocol IMMElementClasses
  (mm-element-classes [mm-or-cls]
    "Returns all element classes in the metamodel `mm-or-cls` or in the
    metamodel containing class `mm-or-cls`."))

(defprotocol IMMRelationshipClasses
  (mm-relationship-classes [mm-or-cls]
    "Returns all relationship classes in the metamodel `mm-or-cls` or in the
    metamodel containing class `mm-or-cls`."))

(defprotocol IMMClass
  (mm-class [model-element] [model mm-class-sym]
    "Returns the given model-element's metamodel class,
  or the metamodel class named mm-class-sym (a symbol)."))

(defprotocol IMMDirectSuperclasses
  (mm-direct-superclasses [metamodel-type]
    "Returns the direct superclasses of metamodel-type."))

(defprotocol IMMSuperclass
  (mm-superclass? [super sub]
    "Return true iff super is a direct or indirect super class of sub.
  (mm-superclass? c c) is false."))

(defprotocol IMMAllSubclasses
  (mm-all-subclasses [cls]
    "Returns the sequence of all subclasses of `cls`.  Those are direct and
    indirect subclasses."))

(defprotocol IMMAttributes
  (mm-attributes [cls]
    "Returns the sequence of attributes declared for class `cls`.
  Each attribute is represented as a keyword."))

(defprotocol IMMReferences
  (mm-references [cls]
    "Returns the sequence of references declared for class `cls`.
  Each reference is represented as a keyword."))

(defprotocol IMMBooleanAttribute
  (mm-boolean-attribute? [cls attr]
    "Returns true iff `attr` (given as keyword) is a boolean attribute of
  metamodel class `cls`."))

(defprotocol IMMMultiValuedProperty
  (mm-multi-valued-property? [cls prop]
    "Returns true iff `prop` (given as keyword) is a multi-valued property
  of `cls`."))

(defprotocol IMMContainmentReference
  (mm-containment-reference? [class role]
    "Returns true if `role` (given as keyword) is a containment reference of `class`,
  i.e., the target objects are contained by `class`."))

;;# Metamodel-specific API generator

(defn ^:private no-nils [coll]
  (doall (remove nil? coll)))

(defmacro metamodel-api-generator
  "A helper macro to generate metamodel specific APIs in some namespace.

  `mm-file` is the file containing the metamodel (an ecore or tg file, or
  something that's recognized by `mm-load`).

  The `nssym` denotes the name of the namespace in which to generate the API.
  If `nssym` is nil, generate it in the current namespace.

  The new namespace (in case nssym was given) is required using the given
  `alias` (if non-nil): (require '[nssym :as alias])

  `prefix` is an optional prefix all generated functions should have given as
  symbol or string.  (This is mainly useful if the functions are generated in
  the current namespace in order not to clash with standard functions such as
  clojure.core/name.)

  `element-class-fn` is a function receiving a metamodel class and the
  `prefix`.  It should return a valid definition-form, e.g., a (defn
  <prefix>do-eclass [...]  ...).

  `relationship-class-fn` is to be a function that receives a relationship
  class and `prefix` and returns a valid definition-form.

  `attr-fn` is a function receiving an attribute name as keyword, a set of
  classes that have such an attribute, and the `prefix`.  It should return a
  valid definition-form.

  `ref-fn` is a function receiving a reference name as keyword, a set of
  element classes that have such a reference, and the `prefix`.  It should
  return a valid definition-form.

  The functions are called with all classes/attributes/references of the
  metamodel."
  [mm-file nssym alias prefix element-class-fn relationship-class-fn attr-fn ref-fn]
  (let [metamodel (mm-load mm-file)
        atts (atom {}) ;; map from attribute kws to set of eclasses that have it
        refs (atom {}) ;; map from reference kws to set of eclasses that have it
        old-ns *ns*]
    `(do
       ~@(when nssym
           `[(ns ~nssym
               ;; Don't refer anything from clojure.core so that we don't get
               ;; warnings about redefinitions.
               (:refer-clojure :only []))
             ;; Remove all java.lang imports so that clashes with generated
             ;; vars cannot occur.
             (doseq [[sym# cls#] (ns-imports *ns*)]
               (ns-unmap *ns* sym#))])
       ~@(concat
          (no-nils
           (for [mm-cls (and (u/satisfies-protocol?
                              metamodel IMMElementClasses
                              "Generating no element class functions.")
                             (mm-element-classes metamodel))]
             (do
               (doseq [a (and (u/satisfies-protocol?
                               mm-cls IMMAttributes
                               "Generating no attribute functions.")
                              (mm-attributes mm-cls))]
                 (swap! atts
                        #(update-in %1 [%2] conj mm-cls)
                        a))
               (doseq [r (and (u/satisfies-protocol?
                               mm-cls IMMReferences
                               "Generating no reference functions.")
                              (mm-references mm-cls))]
                 (swap! refs
                        #(update-in %1 [%2] conj mm-cls)
                        r))
               (when element-class-fn
                 ((resolve element-class-fn) mm-cls prefix)))))
          (no-nils
           (for [rel-cls (and (u/satisfies-protocol?
                               metamodel IMMRelationshipClasses
                               "Generating no relationship class functions.")
                              (mm-relationship-classes metamodel))]
             (do
               ;; Collect attributes
               (doseq [a (and (u/satisfies-protocol?
                               rel-cls IMMAttributes
                               "Generating to relationship attribute functions.")
                              (mm-attributes rel-cls))]
                 (swap! atts
                        #(update-in %1 [%2] conj rel-cls)
                        a))
               (when relationship-class-fn
                 ((resolve relationship-class-fn) rel-cls prefix)))))
          (no-nils
           (when attr-fn
             (for [[a owners] @atts]
               ((resolve attr-fn) a owners prefix))))
          (no-nils
           (when ref-fn
             (for [[r owners] @refs]
               ((resolve ref-fn) r owners prefix)))))
       (in-ns '~(ns-name old-ns))
       ~@(when alias
           [`(require '~(vector nssym :as alias))]))))

;;# Metamodel-specific functional API

(defn ^:private create-element-class-fns [cls prefix]
  `(do
     ~(when-not (mm-abstract? cls)
        `(defn ~(symbol (str prefix "create-" (escaped-uname-str cls) "!"))
           ~(format "Creates a new %s object and adds it to model `m`.
  Properties are set according to `prop-map`.
  Shorthand for (create-element! m '%s prop-map)."
                    (qname cls)
                    (qname cls))
           ([~'m]
            (create-element! ~'m '~(qname cls)))
           ([~'m ~'prop-map]
            (create-element! ~'m '~(qname cls) ~'prop-map))))

     (defn ~(symbol (str prefix "all-" (inflections.core/plural
                                        (escaped-uname-str cls))))
       ~(format "Returns the sequence of %s elements in `m`.
  Shorthand for (elements m '%s)."
                (qname cls)
                (qname cls))
       [~'m]
       (elements ~'m '~(qname cls)))

     ;; TYPE PRED
     (defn ~(symbol (str prefix "isa-" (escaped-uname-str cls) "?"))
       ~(format "Returns true if `el` is a %s-element."
                (qname cls))
       [~'el]
       (has-type? ~'el '~(qname cls)))))

(defn ^:private create-relationship-class-fns [rel-cls prefix]
  `(do
     ~(when-not (mm-abstract? rel-cls)
        ;; CREATE FN
        `(defn ~(symbol (str prefix "create-" (escaped-uname-str rel-cls) "!"))
           ~(format "Create a new %s relationship from `alpha` to `omega` in graph `model`.
  An additional `attr-map` may be supplied.
  Shorthand for (create-relationship! model '%s alpha omega attr-map)."
                    (qname rel-cls)
                    (qname rel-cls))
           ([~'model ~'alpha ~'omega]
            (create-relationship! ~'model '~(qname rel-cls) ~'alpha ~'omega))
           ([~'model ~'alpha ~'omega ~'attr-map]
            (create-relationship! ~'model '~(qname rel-cls) ~'alpha ~'omega ~'attr-map))))

     ;; SEQ FN
     (defn ~(symbol (str prefix "all-" (inflections.core/plural
                                        (escaped-uname-str rel-cls))))
       ~(format "Returns the lazy sequence of %s relationships in `model`."
                (qname rel-cls))
       [~'model]
       (relationships ~'model '~(qname rel-cls)))

     (defn ~(symbol (str prefix "incident-" (inflections.core/plural
                                             (escaped-uname-str rel-cls))))
       ~(format "Returns the lazy sequence of `el`s incident %s relationships.
  May be restricted to incoming or outgoing relationships using the direction
  specification `ds` (:in, :out, or :inout (default))."
                (qname rel-cls))
       ([~'el]
        (incident-relationships ~'el '~(qname rel-cls) :inout))
       ([~'el ~'ds]
        (incident-relationships ~'el '~(qname rel-cls) ~'ds)))

     ;; TYPE PRED
     (defn ~(symbol (str prefix "isa-" (escaped-uname-str rel-cls) "?"))
       ~(format "Returns true if `e` is a %s-relationship."
                (qname rel-cls))
       [~'e]
       (has-type? ~'e '~(qname rel-cls)))))

(defn ^:private create-attribute-fns [attr owners prefix]
  (let [owner-string (str/join ", " (apply sorted-set (map qname owners)))
        bool? (group-by #(and (u/satisfies-protocol? % IMMBooleanAttribute
                                                     "Generating no ?-predicates.")
                              (mm-boolean-attribute? % attr))
                        owners)]
    `(do
       ~@(when (bool? true)
           `[(defn ~(symbol (str prefix (name attr) "?"))
               ~(format "Returns the value of `el`s %s attribute.
  Possible types for `el`: %s"
                        (name attr)
                        owner-string)
               [~'el]
               (aval ~'el ~attr))])
       ~@(when (bool? false)
           `[(defn ~(symbol (str prefix (name attr)))
               ~(format "Returns the value of `el`s %s attribute.
  Possible types for `el`: %s"
                        (name attr)
                        owner-string)
               [~'el]
               (aval ~'el ~attr))])
       (defn ~(symbol (str prefix "set-" (name attr) "!"))
         ~(format "Sets the value of `el`s %s attribute to `val`.
  Possible types for `el`: %s"
                  (name attr)
                  owner-string)
         [~'el ~'val]
         (set-aval! ~'el ~attr ~'val)))))

(defn ^:private create-reference-fns [ref owners prefix]
  (let [multi? (group-by #(mm-multi-valued-property? % ref)
                         owners)
        owner-string (str/join ", " (apply sorted-set (map qname owners)))]
    `(do
       ;; GETTER
       (defn ~(symbol (str prefix "->" (name ref)))
         ~(format "Returns the %s in `el`s %s reference.
  Possible types for `el`: %s"
                  (cond
                    ;; This ref is always multi-valued
                    (and (multi? true) (not (multi? false))) "objects"
                    ;; This ref is always single-valued
                    (and (not (multi? true)) (multi? false)) "object"
                    :else "object[s]")
                  (name ref)
                  owner-string)
         [~'el]
         (if (mm-multi-valued-property? (mm-class ~'el) ~ref)
           (adjs ~'el ~ref)
           (adj ~'el ~ref)))

       ;; SETTER
       (defn ~(symbol (str prefix "->set-" (name ref) "!"))
         ~(format "Sets `el`s %s reference to `refed`.
  `refed` must be a %s.
  Possible types for `el`: %s"
                  (name ref)
                  (cond
                    ;; This ref is always multi-valued
                    (and (multi? true) (not (multi? false))) "collection of objects"
                    ;; This ref is always single-valued
                    (and (not (multi? true)) (multi? false)) "single object"
                    :else "single object or coll of objects, depending on `el`s type")
                  owner-string)
         [~'el ~'refed]
         (if (mm-multi-valued-property? (mm-class ~'el) ~ref)
           (set-adjs! ~'el ~ref ~'refed)
           (set-adj! ~'el ~ref ~'refed)))

       ;; ADDER
       ~@(when (multi? true)
           `[(defn ~(symbol (str prefix "->add-" (name ref) "!"))
               ~(format "Adds `obj` and `more` elements to `el`s %s reference.
  Possible types for `el`: %s"
                        (name ref)
                        owner-string)
               [~'el ~'obj ~'& ~'more]
               (add-adj! ~'el ~ref ~'obj)
               (doseq [o# ~'more]
                 (add-adj! ~'el ~ref o#))
               ~'el)
             (defn ~(symbol (str prefix "->addall-" (name ref) "!"))
               ~(format "Adds all `objs` to `el`s %s reference.
  Possible types for `el`: %s"
                        (name ref)
                        owner-string)
               [~'el ~'objs]
               (add-adjs! ~'el ~ref ~'objs))]))))

(defmacro generate-metamodel-functions
  "Generates a metamodel-specific API consisting of functions for creating
  elements and functions for accessing properties (attributes and references).

  `mm-file` is the file containing the metamodel for which to generate the API.
  This file must be loadable with `mm-load` which see.

  `nssym` is a symbol denoting the new namespace in which to generate.  It may
  be nil in which case the current namespace is used.

  `alias` is an alias under which the newly generated namespace will be
  required.

  `prefix` is an optional prefix all generated functions should use.  (That's
  mostly useful when generating in an existing namespace to prevent name
  clashes.)

  The following functions are generated:

  For any element class Foo in the metamodel, a (create-Foo! model) function,
  an (all-Foos model) function, and a (isa-Foo? el) type check predicate is
  generated.

  For any relationship class Bar in the metamodel, a (create-Bar! model start
  end) function, an (all-Bars model) function, an (incident-Bars el dir)
  function (dir is optional and may be :in, :out, or :inout [default]), and
  a (isa-Foo?  el) type check predicate is generated.

  For any attribute name attr, the following functions are generated:

    (attr el)          ;; Returns the attr value of el
    (set-attr! el val) ;; Sets the attr value of el to val

  For boolean attributes the getter is named attr?.

  For any reference name ref, the following functions are generated:

    (->ref el)                  ;; Returns the element(s) in el's ref role
    (->set-ref! el refed)       ;; Sets el's ref reference to refed
    (->add-ref! el r1 r2 r3...) ;; Adds r1, r2, and r3 to el's ref reference
    (->addall-ref! el rs)       ;; Adds all elements in rs to el's ref reference

  The add-* functions are only generated if ref occurs as a multi-valued
  reference.  If el's ref-reference is multi-valued, then the setter wants a
  collection of elements, else a single element."
  ([mm-file]
   `(generate-metamodel-functions ~mm-file nil nil nil))
  ([mm-file nssym]
   `(generate-metamodel-functions ~mm-file ~nssym nil nil))
  ([mm-file nssym alias]
   `(generate-metamodel-functions ~mm-file ~nssym ~alias nil))
  ([mm-file nssym alias prefix]
   `(metamodel-api-generator ~mm-file
                             ~nssym
                             ~alias
                             ~prefix
                             create-element-class-fns
                             create-relationship-class-fns
                             create-attribute-fns
                             create-reference-fns)))
