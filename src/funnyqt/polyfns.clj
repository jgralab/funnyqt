(ns funnyqt.polyfns
  "Polymorphic functions dispatching on types of model elements.
  Every polyfn must be declared first, and then arbitrary many implementations
  for several metamodel types may be provided.

  There are two kinds of polyfns:

    1. polyfns defined for a given type or subtype whose semantics are similar
       to Java methods, i.e., the most specific polyfn implementation is
       invoked.  See `declare-polyfn` and `defpolyfn`.

    2. polyfns dispatching with a funnyqt.protocols/type-matcher.  These are
       more flexible, but harder to get right.  The first implementation whose
       type-matcher succeeds is invoked.  See `declare-polyfn*` and
       `defpolyfn*`.

  Both kinds of polyfns must have a model element as first argument which is
  used to dispatch among implementations.

  Polyfns of kind 1
  =================

  Every polyfn of kind 1 has to be declared once using `declare-polyfn`, and
  then implementations for specific types map be provided using `defpolyfn`.
  When a polyfn of kind 1 is called, the most specific implementation for the
  model element type is looked up and invoked.

  Example
  -------

  Let's consider our metamodel has the types TypeA and TypeB, and a TypeC that
  extends both TypeA and TypeB.  Furthermore, TypeD extends TypeC.  Lastly,
  there's a TypeE with no inheritance relationships.

    ;; Declare a polyfn
    (declare-polyfn foo1 [elem]
      ;; Optional default behavior
      (str \"Don't know how to handle \" elem))

    ;; Define implementations for several types
    (defpolyfn foo1 'TypeA [elem] ...)
    (defpolyfn foo1 'TypeB [elem] ...)
    (defpolyfn foo1 'TypeC [elem] ...)

  Then, (foo1 objOfTypeA) invokes the first implementation, (foo1 objOfTypeB)
  invokes the second implementation, both (foo1 objOfTypeC) and (foo1
  objOfTypeD) invoke the third implementation, and (foo1 objOfTypeE) invokes
  the default behavior.

  Polyfns of kind 2
  =================

  Every polyfn of kind 2 has to be declared once using `declare-polyfn*`, and
  then implementations with specific type specifications may be provided using
  `defpolyfn*`.  When a polyfn of kind 2 is called, its first argument (the
  model element) is checked against the type specifications of the provided
  implementations.  The first implementation (in definition order) whose type
  specification matches the type of the model element is invoked.

  Note that when providing implementations for a polyfn of kind 2 in different
  namespaces, the actual declaration order of the implementation is determined
  by the order in which the namespaces are loaded (so you probably don't want
  to do that).

  Example
  -------

  Let's consider our metamodel has the types TypeA and TypeB, and a TypeC that
  extends both TypeA and TypeB.  Furthermore, TypeD extends TypeC.  Lastly,
  there's a TypeE with no inheritance relationships.

    ;; Declare a polyfn
    (declare-polyfn* foo2
      \"Returns a string representation of elem\".
      [elem]
      ;; Optional default behavior
      (str \"Don't know how to handle \" elem))

    ;; Define implementations for several types
    (defpolyfn* foo2 '[:and TypeA TypeB !TypeD] [elem] ...)
    (defpolyfn* foo2 'TypeA! [elem] ...)
    (defpolyfn* foo2 'TypeB! [elem] ...)

  Then, both (foo2 objOfTypeD) and (foo2 objOfTypeE) invoke the default
  behavior, (foo2 objOfTypeC) invokes the first implementation, (foo2
  objOfTypeA) invokes the second implementation, and (foo2 objOfTypeB) invokes
  the third implementation."
  (:use [funnyqt.protocols   :only [has-type? type-matcher model-element-type
                                    meta-model-type-super-types qname]])
  (:use [funnyqt.utils       :only [errorf]])
  (:use [clojure.tools.macro :only [name-with-attributes]])
  (:require [ordered.map :as om])
  (:require clojure.pprint))

;;# Type 1 polyfns

;; {PolyFnVar {TypeQName ImplFn}}
(defonce polyfn-dispatch-table (atom {}))

(defn find-polyfn-impl [m t]
  (loop [ts [t]]
    (if (seq ts)
      (let [fns (remove nil? (map #(m (qname %)) ts))]
        (if (seq fns)
          (if (fnext fns)
            (errorf "%s polyfns are applicable for type %s" (count fns) t)
            (first fns))
          (recur (set (mapcat meta-model-type-super-types ts)))))
      (errorf "No polyfn implementation defined for type %s" t))))

(defmacro declare-polyfn
  "Decares a polymorphic function dispatching on a model element type.
  `name` is the name of the new polyfn, an optional `doc-string` may be
  provided.  The argument list's first element must be the model element on
  whose type the dispatch is done.  Polymorphic functions for several metamodel
  types are provided later using `defpolyfn`.  If an optional `body` is
  provided, this is executed if no implementation for `model-elem`s type was
  added using `defpolyfn`.  The default behavior in that case (i.e., `body`
  omitted) is to throw an exception."

  {:arglists '([name doc-string? [model-elem & more] & body])}
  [name & more]
  (let [[name more] (name-with-attributes name more)
        argvec      (first more)
        body        (next more)]
    `(do
       (declare ~name)
       (when-not (find @polyfn-dispatch-table #'~name)
         (swap! polyfn-dispatch-table assoc #'~name {}))

       (defn ~name ~(meta name)
         ~argvec
         (let [dispatch-map# (@polyfn-dispatch-table #'~name)
               elem# ~(first argvec)
               type# (model-element-type elem#)]
           ((find-polyfn-impl dispatch-map# type#) elem#))))))

(defmacro defpolyfn
  "Defines an implementation of the polyfn `name` for objects of type `type`.
  The polyfn has to be already declared using `declare-polyfn`.  `type` is a
  fully qualified type name that is used to check if the polyfn implementation
  is matching the type of `model-elem`.  The arity of the argument vector has
  to match the one of the corresponding `declare-polyfn`."

  {:arglists '([name type [model-elem & more] & body])}
  [name & more]
  (let [[name more]   (name-with-attributes name more)
        [type more]   [(first more) (next more)]
        [argvec body] [(first more) (next more)]]
    `(do
       (when-not (find @polyfn-dispatch-table #'~name)
         (errorf "%s is not declared as a polyfn." #'~name))
       (when-not (symbol? ~type)
         (errorf "The type given to a defpolyfn must be a symbol but was %s." ~type))
       (let [^String n# (clojure.core/name ~type)]
         (when (or (.startsWith n# "!")
                   (.endsWith n# "!"))
           (errorf "The type given to defpolyfn must be a plain qname symbol but was %s."
                   ~type)))
       (swap! polyfn-dispatch-table update-in [#'~name]
              assoc ~type (fn ~argvec ~@body)))))

(defn reset-polyfn-dispatch-table
  "Resets the `polyfn-dispatch-table`.
  If a polyfn var is given, only resets the impls for that var."
  ([]
     (swap! polyfn-dispatch-table (fn [& ignore] {})))
  ([v]
     (when-not (find @polyfn-dispatch-table v)
       (errorf "No such polyfn %s to be resetted." v))
     (swap! polyfn-dispatch-table update-in [v] (fn [& ign] {}))))

;;# Type 2 polyfns

;; {PolyFnVar {TypeSpec [ImplFn CachedTypeMatcher]}}
(defonce polyfn*-dispatch-table (atom {}))

(defmacro declare-polyfn*
  "Decares a polymorphic function dispatching on a model element type.
  `name` is the name of the new polyfn, an optional `doc-string` may be
  provided.  The argument list's first element must be the model element on
  whose type the dispatch is done.  Polymorphic functions for several metamodel
  types are provided later using `defpolyfn*`.  If an optional `body` is
  provided, this is executed if no implementation for `model-elem`s type was
  added using `defpolyfn*`.  The default behavior in that case (i.e., `body`
  omitted) is to throw an exception."

  {:arglists '([name doc-string? [model-elem & more] & body])}
  [name & more]
  (let [[name more] (name-with-attributes name more)
        argvec      (first more)
        body        (next more)]
    `(do
       (declare ~name)
       (when-not (find @polyfn*-dispatch-table #'~name)
         (swap! polyfn*-dispatch-table assoc #'~name (om/ordered-map)))

       (defn ~name ~(meta name)
         ~argvec
         (loop [dispatch-map# (@polyfn*-dispatch-table #'~name)]
           (if (seq dispatch-map#)
             (let [[ts# [f# tm#]] (first dispatch-map#)
                   [existed# tm#] (if tm#
                                    [true tm#]
                                    [false (type-matcher ~(first argvec) ts#)])]
               (when-not existed#
                 (swap! polyfn*-dispatch-table update-in [#'~name] assoc
                        ts# [f# tm#]))
               (if (tm# ~(first argvec))
                 (f# ~@argvec)
                 (recur (rest dispatch-map#))))
             ~(if (seq body)
                `(do ~@body)
                `(errorf "No polyfn for handling '%s'." ~(first argvec)))))))))

(defmacro defpolyfn*
  "Defines an implementation of the polyfn `name` matching with `type-spec`.
  The polyfn has to be declared using `declare-polyfn*`.  `type-spec` is a type
  specification that is used to check if the polyfn implementation is matching
  the type of `model-elem` (see `funnyqt.protocols/type-matcher`).  The arity
  of the argument vector has to match the one of the corresponding
  `declare-polyfn*`."

  {:arglists '([name type-spec [model-elem & more] & body])}
  [name & more]
  (let [[name more]     (name-with-attributes name more)
        [type-spec more] [(first more) (next more)]
        [argvec body]   [(first more) (next more)]]
    `(do
       (when-not (find @polyfn*-dispatch-table #'~name)
         (errorf "%s is not declared as a polyfn." #'~name))
       (swap! polyfn*-dispatch-table update-in [#'~name] assoc
              ~type-spec
              [(fn ~argvec ~@body) nil]))))

(defn reset-polyfn*-dispatch-table
  "Resets the `polyfn*-dispatch-table`.
  If a polyfn var is given, only resets the impls for that var."
  ([]
     (swap! polyfn*-dispatch-table (fn [& ignore] {})))
  ([v]
     (when-not (find @polyfn*-dispatch-table v)
       (errorf "No such polyfn %s to be resetted." v))
     (swap! polyfn*-dispatch-table update-in [v] (fn [& ign] (om/ordered-map)))))
