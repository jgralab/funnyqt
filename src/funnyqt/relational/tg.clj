(ns funnyqt.relational.tg
  "Querying graphs as if they were prolog fact bases.

Getting Started
===============

For a basic understanding of logic programming using miniKanren, have a look
here: http://tinyurl.com/czn2nxz

The first thing you want to do is to generate relations (derived from the
schema) and populate them with facts (from the graph).  Here's how to do this.

First, we require the FunnyQT TG core namespace with prefix core, so that we
can use its general graph loading function.

    user> (require '[funnyqt.tg :as core])

Then, we load the GReQL test graph and bind it to a var `g`.

    user> (def g (core/load-graph \"test/input/greqltestgraph.tg\"))

We import the relational namespace.

    user> (use 'funnyqt.relational.tg)

Now, we can generate relations and facts for a graph and its schema.  The first
argument is the graph, and the second argument is a new namespace name.  That
will be created, and all relations (including an implicit reference to the
graph) are defined in it.

    user> (create-schema-relations-ns g 'roadmap)

This procedure creates 4 relations per attributed element class of the schema
of our graph.  For any vertex class Foo, there's a relation (+Foo! x) which
succeeds if `x` can be unified with a vertex of exact type Foo.  Additionally,
`there`s a relation (+Foo! x) which succeeds if `x` can be unified with a vertex
of exact type Foo or a subtype thereof.  Furthermore, there are +!Foo and
+!Foo!, which succeed for vertices not of (exact) type Foo.

For an edge class Bar, there is the relation (+Bar! e a o) which succeeds if
`e` can be unified with an edge of exact type Bar, and `a` and `o` can be
unified with the start and end vertex of `e`.  Similarly to vertex classes,
relations +Bar, +!Bar, and +!Bar! are also created with the same signature.

For any attribute name baz, a relation (+baz el val) is generated, which
succeeds if the attributed element `el` is an instance of an attributed element
class that has such an attribute defined, and its value is set to `val`.

To use our new relations, we change into the roadmap namespace.

    user> (in-ns 'roadmap)

Now, we are ready to go.

Asking Questions
================

After we've populated our user namespace with relations and facts about the
route graph, we can ask questions.  That's done with the `run` and `run*'
macros provided by clojure.core.logic.  For example

    (run 3 [q] <goals>)

returns at most 3 answers unifying `q` with the given goals, or () if there's
no answer at all, and

    (run* [q] <goals>)

returns all possible answers.

Now, let's ask what's the capital of the County the Village Kammerforst is
located in.

    user> (run* [q]
            (fresh [kammerforst county e1 e2]
              (+Village kammerforst)
              (+name kammerforst \"Kammerforst\")
              (+ContainsLocality e1 county kammerforst)
              (+HasCapital e2 county q)))

    (#<CityImpl v6: localities.City>)

We use `run*` because we want to get all answers.  `fresh` introduces new logic
vars that should be unified.  In its body, we declare that `kammerforst` has to
be unified with a Village vertex, whose name is \"Kammerforst\".  Furthermore,
there has to be a ContainsLocality edge `e1' starting at some `county` and
leading to `kammerforst`.  `county` has to be the capital of `q`, which is
exactly what we wanted to ask.  Because `kammerforst` and `county` occur
multiple times, they are subject to unification.  Likewise, our question `q` is
unified with the end vertex of the edge `e2`.

Now let's try to pose a question about what capitals reign which localities,
e.g., what are the localities contained in the county of some capital, for all
capitals.  We want to get all pairs of the form [capital-name locality-name] as
answer.

Here, we use the `with-fresh` macro for convenience.  It creates one fresh
logic variable for any symbol in its body starting with a question mark (?).
Additionally, it creates one anonymous fresh logic variable per occurence of
`_`.  In the former example, we had the logic vars `e1` and `e2` explicit,
although we never unified them with some other var.  So `_` is a shortcut for
'I don't care for anything except existence'.

    user> (run* [q]
            (with-fresh
              (+Locality ?loc)
              (+ContainsLocality _ ?county ?loc)
              (+HasCapital _ ?county ?capital)
              (!= ?capital ?loc)
              (+name ?capital ?cname)
              (+name ?loc ?lname)
              (== q [?cname ?lname])))

    ([\"Main\" \"Lautzenhausen\"] [\"Main\" \"Montabaur\"]
     [\"Main\" \"Flughafen Frankfurt-Hahn\"] [\"Main\" \"Winningen\"]
     [\"Main\" \"Koblenz\"] [\"Main\" \"Kammerforst\"]
     [\"Frankfurt am Main\" \"Frankfurt-Flughafen\"]
     [\"Main\" \"Flugplatz Koblenz-Winningen\"] [\"Main\" \"Höhr-Grenzhausen\"])

Looks like in the graph we've misspelled Mainz as Main, but anyway.  We say
that `?loc` has to be a Locality that is contained in `?county` that in turn
has a `?capital`.  We don't want to get the captial as ruled by itself, so we
declare that `?capital` and `?loc` must not be unified with each other.

To define our result, we declare `?cname` and `?lname` to be the names of
`?capital` and `?loc`, respectively.  Finally, we declare that our question `q`
should be unified with a vector containing `?cname` and `?lname`.

Custom Relations
================

We think that being able to query for the capital of a location or the
locations of some capital is a thing we're going to do frequently.  So we can
factor that out into a custom relation `(capitalo c l)` that succeeds if `c` is
the captial of `l` simply by defining a function.

    user> (defn capitalo
            \"Succeeds, if c is the capital of Locality l.\"
            [c l]
            (with-fresh
              (+Locality l)
              (+ContainsLocality _ ?county l)
              (+HasCapital _ ?county c)
              (!= c l)))

We can pose our question now using this new relation and get the same answer.

    user> (run* [q]
            (with-fresh
              (capitalo ?capital ?loc)
              (+name ?capital ?cname)
              (+name ?loc ?lname)
              (== q [?cname ?lname])))

Have fun!"
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:use funnyqt.relational.util
        [funnyqt.relational :only [*model*]])
  (:require [funnyqt.tg :as core])
  (:require [funnyqt.protocols :as genprots])
  (:require [funnyqt.query.tg :as query])
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass)))

;;# Utilities

(defn- class->rel-symbols
  "Returns a relation symbol for the class `c`."
  [^AttributedElementClass c]
  (let [n (.getUniqueName c)
        fqn (.getQualifiedName c)]
    (mapv (fn [s]
            (with-meta (symbol s)
              {:unique-name
               (symbol (str "+" (clojure.string/replace
                                 s #"([!])?.*[.]" #(or (nth % 1) ""))))}))
          [fqn (str fqn "!") (str "!" fqn) (str "!" fqn "!")])))

(defn- create-vc-relations
  "Creates relations for the given vertex class."
  [vc]
  (let [v 'v]
    (for [na (class->rel-symbols vc)]
      `(defn ~(:unique-name (meta na))
         {:doc ~(format "A relation where `%s` is a %s vertex." v na)}
         [~v]
         (fn [a#]
           (let [v# (walk a# ~v)]
             (if (fresh? v#)
               (to-stream
                (->> (map #(unify a# ~v %)
                          (query/vseq *model* '~na))
                     (remove not)))
               (if (and (core/vertex? v#)
                        (core/contains-vertex? *model* v#)
                        (genprots/has-type? v# '~na))
                 a#
                 (fail a#)))))))))

(defn- create-ec-relations
  "Creates relations for the given edge class."
  [ec]
  (let [e  'e
        al 'alpha
        om 'omega]
    (for [na (class->rel-symbols ec)]
      `(defn ~(:unique-name (meta na))
         {:doc ~(format "A relation where `%s' is a %s edge from `%s' to `%s'."
                        e na al om)}
         [~e ~al ~om]
         (fn [a#]
           (let [e#  (walk a# ~e)
                 al# (walk a# ~al)
                 om# (walk a# ~om)]
             (cond
              (and (ground? e#)
                   (core/edge? e#))
              (or (unify a# [~al ~om] [(core/alpha e#) (core/omega e#)])
                  (fail a#))

              (and (ground? al#)
                   (core/vertex? al#))
              (to-stream
               (->> (map #(unify a# [~e ~om] [% (core/omega %)])
                         (query/iseq al# '~na :out))
                    (remove not)))

              (and (ground? om#)
                   (core/vertex? om#))
              (to-stream
               (->> (map #(unify a# [~e ~al] [% (core/alpha %)])
                         (query/iseq om# '~na :in))
                    (remove not)))

              :else (to-stream
                     (->> (for [edge# (query/eseq *model* '~na)]
                            (unify a# [~e ~al ~om]
                                   [edge# (core/alpha edge#) (core/omega edge#)]))
                          (remove not))))))))))

(defn- create-attr-relation
  "Creates relations for the given attribute."
  [[attr aecs]] ;; attr is an attr name symbol, aecs the set of classes having
  ;; such an attr
  (let [ts     (mapv #(genprots/qname %) aecs) ;; a type spec
        seqf   (cond
                (every? #(instance? VertexClass %) aecs) 'funnyqt.query.tg/vseq
                (every? #(instance? EdgeClass %)   aecs) 'funnyqt.query.tg/eseq
                :else `(fn [graph# ts#]
                         (apply concat ((juxt funnyqt.query.tg/vseq
                                              funnyqt.query.tg/eseq)
                                        graph# ts#))))
        elem   'ae
        val    'val]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `%s' has value `%s' for its %s attribute."
               elem val attr)}
       [~elem ~val]
       (fn [a#]
         (let [elem# (walk a# ~elem)]
           (cond
            (and (ground? elem#)
                 (core/attributed-element? elem#))
            (or (unify a# ~val (core/value elem# ~attr))
                (fail a#))

            :else (to-stream
                   (->> (for [e# (~seqf *model* '~ts)
                              :let [v# (core/value e# ~attr)]]
                          (unify a# [~elem ~val] [e# v#]))
                        (remove not)))))))))

;;# Main

(defn vertexo
  "A relation where `v` is a vertex."
  [v]
  (fn [a]
    (let [gv (walk a v)]
      (if (fresh? gv)
        (to-stream
         (->> (map #(unify a v %)
                   (query/vseq *model*))
              (remove not)))
        (if (and (core/vertex? gv)
                 (core/contains-vertex? *model* gv))
          a
          (fail a))))))

(defn edgeo
  "A relation where `e` is an edge from `alpha` to `omega`."
  [e alpha omega]
  (fn [a]
    (let [ge     (walk a e)
          galpha (walk a alpha)
          gomega (walk a omega)]
      (cond
       (and (ground? ge)
            (core/edge? ge))
       (or (unify a [alpha omega] [(core/alpha ge) (core/omega ge)])
           (fail a))

       (and (ground? galpha)
            (core/vertex? galpha))
       (to-stream
        (->> (map #(unify a [e omega] [% (core/omega %)])
                  (query/iseq galpha nil :out))
             (remove not)))

       (and (ground? gomega)
            (core/vertex? gomega))
       (to-stream
        (->> (map #(unify a [e alpha] [% (core/alpha %)])
                  (query/iseq gomega nil :in))
             (remove not)))

       :else (to-stream
              (->> (for [edge (query/eseq *model*)]
                     (unify a [e alpha omega]
                            [edge (core/alpha edge) (core/omega edge)]))
                   (remove not)))))))

(defn valueo
  "A relation where `ae` has value `val` for its `at` attribute."
  [ae at val]
  (fn [a]
    (let [gae  (walk a ae)
          gat  (walk a at)
          gval (walk a val)]
      (cond
       (and (ground? gae)
            (ground? gat)
            (core/attributed-element? gae)
            (or (keyword? gat) (string? gat) (symbol? gat)))
       (or (unify a [ae at val] [gae gat (core/value gae gat)])
           (fail a))

       (and (ground? gae)
            (core/vertex? gae))
       (to-stream
        (->> (for [^Attribute attr (seq (.getAttributeList
                                         ^AttributedElementClass
                                         (core/attributed-element-class gae)))
                   :let [an (keyword (.getName attr))]]
               (unify a [ae at val] [gae an (core/value gae an)]))
             (remove not)))

       :else (to-stream
              (->> (for [elem (concat (query/vseq *model*)
                                      (query/eseq *model*))
                         ^Attribute attr (seq (.getAttributeList
                                               ^AttributedElementClass
                                               (core/attributed-element-class elem)))
                         :let [an (keyword (.getName attr))]]
                     (unify a [ae at val] [elem an (core/value elem an)]))
                   (remove not)))))))


(defn create-schema-relations-ns
  "Populates the namespace `nssym` (a symbol) with relations reflecting the
  schema of `schema`."
  [^Schema schema nssym]
  (let [atts (atom {}) ;; map from attribute names to set of attributed element classes that have it
        old-ns *ns*
        code `(do
                (ns ~nssym
                  (:refer-clojure :exclude [~'==])
                  (:use [clojure.core.logic])
                  (:use funnyqt.relational
                        funnyqt.relational.tg))

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Schema specific relations ;;
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ~@(concat
                   (doall
                    (mapcat
                     (fn [^VertexClass vc]
                       (doseq [a (map #(keyword (.getName ^Attribute %))
                                      (seq (.getOwnAttributeList vc)))]
                         (swap! atts
                                #(update-in %1 [%2] conj vc)
                                a))
                       (create-vc-relations vc))
                     (seq (-> schema .getGraphClass .getVertexClasses))))
                   (doall
                    (mapcat
                     (fn [^EdgeClass ec]
                       (doseq [a (map #(keyword (.getName ^Attribute %))
                                      (seq (.getOwnAttributeList ec)))]
                         (swap! atts
                                #(update-in %1 [%2] conj ec)
                                a))
                       (create-ec-relations ec))
                     (seq (-> schema .getGraphClass .getEdgeClasses))))
                   (for [^Attribute a @atts]
                     (create-attr-relation a))))]
    ;; (clojure.pprint/pprint code)
    (eval code)
    (in-ns (ns-name old-ns))
    nssym))

;;# How to use...

(comment
  ;; SETUP
  (require '[funnyqt.tg :as core])
  (def g (core/load-graph "/home/horn/Repos/uni/funtg/test/greqltestgraph.tg"))
  (use 'funnyqt.tg.funrl)
  (create-schema-relations-ns g 'roadmap)
  (in-ns 'roadmap)

  ;; All Capitals of the County in which the Village Kammerforst is located in.
  (run* [q]
    (with-fresh
      (+Village ?kammerforst)
      (+name ?kammerforst "Kammerforst")
      (+ContainsLocality _ ?county ?kammerforst)
      (+HasCapital _ ?county q)))
  ;;=> (#<v6: localities.City>)

  ;; All Capitals with their reigned Localities (names as result)
  (run* [q]
    (with-fresh
      (+Locality ?loc)
      (+ContainsLocality _ ?county ?loc)
      (+HasCapital _ ?county ?capital)
      (!= ?capital ?loc)
      (+name ?capital ?cname)
      (+name ?loc ?lname)
      (== q [?cname ?lname])))
  ;;=> (["Main" "Kammerforst"]
  ;;    ["Main" "Lautzenhausen"]
  ;;    ["Main" "Höhr-Grenzhausen"]
  ;;    ["Main" "Winningen"]
  ;;    ["Main" "Montabaur"]
  ;;    ["Main" "Koblenz"]
  ;;    ["Frankfurt am Main" "Frankfurt-Flughafen"]
  ;;    ["Main" "Flughafen Frankfurt-Hahn"]
  ;;    ["Main" "Flugplatz Koblenz-Winningen"])

  ;; We can factor out the is-capital-of relation as a function
  (defn capitalo
    "Succeeds, if c is the capital of l."
    [c l]
    (with-fresh
      (+Locality l)
      (+ContainsLocality _ ?county l)
      (+HasCapital _ ?county c)
      (!= c l)))

  ;; Now we can refactor our query
  (run* [q]
    (with-fresh
      (capitalo ?capital ?loc)
      (+name ?capital ?cname)
      (+name ?loc ?lname)
      (== q [?cname ?lname])))

  (defn two-airportso
    [a1 a2]
    (+Airport a1) (+Airport a2))

  (defn connectedo
    "Succeeds, if the junctions j1 and j2 are connected by connections (Ways,
    Streets, AirRoutes)."
    [j1 j2]
    (with-fresh
      (conde
       ;; A direct street connection, either from j1->j2
       [(+Way _ j1 j2)]
       ;; or the other way round...
       [(+Way _ j2 j1)]
       ;; If we have 2 Airports, then check only if there's a direct airroute
       [(two-airportso j1 j2)
        (conda
         [(+AirRoute _ j1 j2)]
         [(+AirRoute _ j2 j1)])]
       ;; or an indirect connection, i.e, there's another crossroad in the
       ;; middle.
       [(connectedo j1 ?middle) (connectedo ?middle j2)])))

  (defn connected-locso
    "Succeeds, if the localities l1 and l2 are connected.
    Localities are connected, if they contain crossroads that are connected by
    connections (Ways, Streets, AirRoutes)."
    [l1 l2]
    (+Locality l1)
    (+Locality l2)
    (with-fresh
      (conde
       ;; Airports can be connected by AirRoutes directly, everything else is
       ;; connected by streets connecting crossroads that are contained in
       ;; localities.
       [(two-airportso l1 l2) (connectedo l1 l2)]
       [(+Airport l1) (+ContainsCrossroad _ l2 ?c2) (connectedo ?c2 l1)]
       [(+Airport l2) (+ContainsCrossroad _ l1 ?c1) (connectedo l2 ?c1)]
       ;; both are no airports
       [(+ContainsCrossroad _ l1 ?c1)
        (+ContainsCrossroad _ l2 ?c2)
        (connectedo ?c1 ?c2)])))

  ;; What locality is connected with Kammerforst?
  (run 3 [q]
    (with-fresh
      (+Locality ?l1)
      (+name ?l1 "Kammerforst")
      (+City ?l2)
      (!= ?l1 ?l2)
      (connected-locso ?l1 ?l2)
      (== q [?l1 ?l2])))

  ;; Some tests with a java graph
  (require '[funnyqt.query.tg :as query])
  (require '[funnyqt.tg :as core])
  (def g (core/load-graph "/home/horn/Repos/uni/funtg-pub/funql-whitepaper/jgralab.tg.gz"))
  (create-schema-relations-ns g 'jgralab)

  ;; VSeq vs relation
  (time (dorun (run* [q] (+ClassDefinition q))))
  ; "Elapsed time: 66.21493 msecs"
  (time (dorun (query/vseq *model* 'ClassDefinition)))
  ; "Elapsed time: 36.796144 msecs"

  (defn direct-subclasso
    [sub super]
    (with-fresh
      (+IsSuperClassOf _ ?ts sub)
      (+IsTypeDefinitionOf _ super ?ts)))

  (defn subclasso
    "A relation where `sub` is a subclass of `super`."
    [sub super]
    (conda
     [(direct-subclasso sub super)]
     [(with-fresh
        (subclasso sub ?mid)
        (subclasso ?mid super))]))

  (run* [q]
    (with-fresh
      (+ClassDefinition ?c)
      (subclasso ?c ?super)
      (+fullyQualifiedName ?c ?cname)
      (+fullyQualifiedName ?super ?sname)
      (== q [?cname ?sname])))
  )
