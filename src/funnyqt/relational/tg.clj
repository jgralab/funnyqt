(ns funnyqt.relational.tg
  "Querying graphs as if they were prolog fact bases.

CAUTION: These docs are a bit outdated!!!

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

    user> (generate-schema-relations g 'roadmap)

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
  (:use clojure.core.logic
        funnyqt.relational.util
        [funnyqt.relational :only [get-*model*]])
  (:require funnyqt.tg
            funnyqt.protocols
            funnyqt.query.tg
            funnyqt.query
            clojure.java.io)
  (:import
   (de.uni_koblenz.jgralab Graph Vertex Edge AttributedElement)
   (de.uni_koblenz.jgralab.schema AggregationKind Schema Domain RecordDomain
                                  AttributedElementClass NamedElement
                                  GraphClass VertexClass EdgeClass Attribute
                                  GraphElementClass IncidenceClass)))

(defn typeo
  "A relation where vertex or edge `e` has the type `t`, a graph element class
  name.  In fact, `t` may be any type specification (see `type-matcher`)."
  [e t]
  (fn [a]
    (let [ge (walk a e)
          gt (walk a t)]
      (cond
       (and (ground? ge) (ground? gt))
       (if (and (funnyqt.tg/attributed-element? ge)
                (or (coll? gt) (symbol? gt))
                (funnyqt.protocols/has-type? ge gt))
         (succeed a)
         (fail a))

       (ground? ge)
       (or (and (funnyqt.tg/attributed-element? ge)
                (unify a t (funnyqt.protocols/qname ge)))
           (fail a))

       (ground? gt)
       (to-stream
          (->> (map #(unify a e %)
                    (concat (funnyqt.query.tg/vseq (get-*model*) gt)
                            (funnyqt.query.tg/eseq (get-*model*) gt)))
               (remove not)))

       :else (to-stream
              (->> (for [elem (concat (funnyqt.query.tg/vseq (get-*model*))
                                      (funnyqt.query.tg/eseq (get-*model*)))]
                     (unify a [e t] [elem (funnyqt.protocols/qname elem)]))
                   (remove not)))))))

(defn vertexo
  "A relation where `v` is a vertex."
  [v]
  (fn [a]
    (let [gv (walk a v)]
      (if (fresh? gv)
        (to-stream
         (->> (map #(unify a v %)
                   (funnyqt.query.tg/vseq (get-*model*)))
              (remove not)))
        (if (and (funnyqt.tg/vertex? gv)
                 (funnyqt.tg/contains-vertex? (get-*model*) gv))
          (succeed a)
          (fail a))))))

(defn edgeo
  "A relation where `e` is an edge from `alpha` to `omega`."
  [e alpha omega]
  (fn [a]
    (let [ge     (walk a e)
          galpha (walk a alpha)
          gomega (walk a omega)]
      (cond
       (ground? ge)
       (or (and (funnyqt.tg/edge? ge)
                (unify a [alpha omega] [(funnyqt.tg/alpha ge) (funnyqt.tg/omega ge)]))
           (fail a))

       (ground? galpha)
       (if (funnyqt.tg/vertex? galpha)
         (to-stream
          (->> (map #(unify a [e omega] [% (funnyqt.tg/omega %)])
                    (funnyqt.query.tg/iseq galpha nil :out))
               (remove not)))
         (fail a))

       (ground? gomega)
       (if (funnyqt.tg/vertex? gomega)
         (to-stream
          (->> (map #(unify a [e alpha] [% (funnyqt.tg/alpha %)])
                    (funnyqt.query.tg/iseq gomega nil :in))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [edge (funnyqt.query.tg/eseq (get-*model*))]
                     (unify a [e alpha omega]
                            [edge (funnyqt.tg/alpha edge) (funnyqt.tg/omega edge)]))
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
            (ground? gat))
       (or (and (funnyqt.tg/attributed-element? gae)
                (keyword? gat)
                (.getAttribute ^AttributedElementClass (funnyqt.tg/attributed-element-class gae)
                               (name gat))
                (unify a val (funnyqt.tg/value gae gat)))
           (fail a))

       (ground? gae)
       (if (funnyqt.tg/vertex? gae)
         (to-stream
          (->> (for [^Attribute attr (seq (.getAttributeList
                                           ^AttributedElementClass
                                           (funnyqt.tg/attributed-element-class gae)))
                     :let [an (keyword (.getName attr))]]
                 (unify a [at val] [an (funnyqt.tg/value gae an)]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [elem (concat (funnyqt.query.tg/vseq (get-*model*))
                                      (funnyqt.query.tg/eseq (get-*model*)))
                         ^Attribute attr (seq (.getAttributeList
                                               ^AttributedElementClass
                                               (funnyqt.tg/attributed-element-class elem)))
                         :let [an (keyword (.getName attr))]]
                     (unify a [ae at val] [elem an (funnyqt.tg/value elem an)]))
                   (remove not)))))))

(defn- edge-class-roles [^EdgeClass ec from-or-to]
  (remove empty? (.getAllRoles ^IncidenceClass
                               (if (= :to from-or-to)
                                 (.getTo ec)
                                 (.getFrom ec)))))

(defn adjo
  "A relation where `rv` is in the `role` role of `v`."
  [v role rv]
  (fn [a]
    (let [gv    (walk a v)
          grole (walk a role)
          grv   (walk a rv)]
      (cond
       (and (ground? gv) (ground? grole))
       (if (and (funnyqt.tg/vertex? gv) (keyword? grole))
         (to-stream
          (->> (for [refed (funnyqt.query/adjs* gv grole)]
                 (unify a [rv] [refed]))
               (remove not)))
         (fail a))

       (ground? gv)
       (if (funnyqt.tg/vertex? gv)
         (to-stream
          (->> (for [e (funnyqt.query.tg/iseq gv)
                     rn (edge-class-roles (funnyqt.tg/attributed-element-class e)
                                          (if (funnyqt.tg/normal-edge? e) :to :from))
                     :when rn
                     :let [rn (keyword rn)]]
                 (unify a [role rv] [rn (funnyqt.tg/that e)]))
               (remove not)))
         (fail a))

       (ground? grv)
       (if (funnyqt.tg/vertex? grv)
         (to-stream
          (->> (for [e (funnyqt.query.tg/iseq grv)
                     rn (edge-class-roles (funnyqt.tg/attributed-element-class e)
                                          (if (funnyqt.tg/normal-edge? e) :from :to))
                     :when rn
                     :let [rn (keyword rn)]]
                 (unify a [v role] [(funnyqt.tg/that e) rn]))
               (remove not)))
         (fail a))

       :else (to-stream
              (->> (for [s (funnyqt.query.tg/vseq (get-*model*))
                         e (funnyqt.query.tg/iseq s)
                         rn (edge-class-roles (funnyqt.tg/attributed-element-class e)
                                              (if (funnyqt.tg/normal-edge? e) :to :from))
                         :when rn
                         :let [rn (keyword rn)]]
                     (unify a [v role rv] [(funnyqt.tg/this e) rn (funnyqt.tg/that e)]))
                   (remove not)))))))


;;# Metamodel specific

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
  (for [na (class->rel-symbols vc)]
    `(defn ~(:unique-name (meta na))
       {:doc ~(format "A relation where `v` is a %s vertex." na)}
       [~'v]
       (fn [~'a]
         (let [~'gv (walk ~'a ~'v)]
           (if (fresh? ~'gv)
             (to-stream
              (->> (map (fn [~'vertex] (unify ~'a ~'v ~'vertex))
                        (funnyqt.query.tg/vseq ~'*model* '~na))
                   (remove not)))
             (if (and (funnyqt.tg/vertex? ~'gv)
                      (funnyqt.tg/contains-vertex? ~'*model* ~'gv)
                      (funnyqt.protocols/has-type? ~'gv '~na))
               (succeed ~'a)
               (fail ~'a))))))))

(defn- create-ec-relations
  "Creates relations for the given edge class."
  [ec]
  (for [na (class->rel-symbols ec)]
    `(defn ~(:unique-name (meta na))
       {:doc ~(format "A relation where `e' is a %s edge from `al' to `om'." na)}
       [~'e ~'al ~'om]
       (fn [~'a]
         (let [~'ge  (walk ~'a ~'e)
               ~'gal (walk ~'a ~'al)
               ~'gom (walk ~'a ~'om)]
           (cond
            (ground? ~'ge)
            (or (and (funnyqt.tg/edge? ~'ge)
                     (unify ~'a [~'al ~'om]
                            [(funnyqt.tg/alpha ~'ge) (funnyqt.tg/omega ~'ge)]))
                (fail ~'a))

            (ground? ~'gal)
            (if (funnyqt.tg/vertex? ~'gal)
              (to-stream
               (->> (map (fn [~'incidence]
                           (unify ~'a [~'e ~'om]
                                  [~'incidence (funnyqt.tg/omega ~'incidence)]))
                         (funnyqt.query.tg/iseq ~'gal '~na :out))
                    (remove not)))
              (fail ~'a))

            (ground? ~'gom)
            (if (funnyqt.tg/vertex? ~'gom)
              (to-stream
               (->> (map (fn [~'incidence]
                           (unify ~'a [~'e ~'al]
                                  [~'incidence (funnyqt.tg/alpha ~'incidence)]))
                         (funnyqt.query.tg/iseq ~'gom '~na :in))
                    (remove not)))
              (fail ~'a))

            :else (to-stream
                   (->> (for [~'edge (funnyqt.query.tg/eseq ~'*model* '~na)]
                          (unify ~'a [~'e ~'al ~'om]
                                 [~'edge (funnyqt.tg/alpha ~'edge) (funnyqt.tg/omega ~'edge)]))
                        (remove not)))))))))

(defn- create-attr-relation
  "Creates relations for the given attribute."
  [[attr aecs]]   ;;; attr is an attr name symbol, aecs the set of classes
                  ;;; having such an attr
  (let [ts     (mapv #(funnyqt.protocols/qname %) aecs) ;; a type spec
        seqf   (cond
                (every? #(instance? VertexClass %) aecs) 'funnyqt.query.tg/vseq
                (every? #(instance? EdgeClass %)   aecs) 'funnyqt.query.tg/eseq
                :else `(fn [graph# ts#]
                         (apply concat ((juxt funnyqt.query.tg/vseq
                                              funnyqt.query.tg/eseq)
                                        graph# ts#))))]
    `(defn ~(symbol (str "+" (name attr)))
       {:doc ~(format
               "A relation where `ae' has value `val' for its %s attribute." attr)}
       [~'ae ~'val]
       (fn [~'a]
         (let [~'gae (walk ~'a ~'ae)]
           (cond
            (ground? ~'gae)
            (or (and (funnyqt.tg/attributed-element? ~'gae)
                     (.getAttribute (funnyqt.tg/attributed-element-class ~'gae) ~(name attr))
                     (unify ~'a ~'val (funnyqt.tg/value ~'gae ~attr)))
                (fail ~'a))

            :else (to-stream
                   (->> (for [~'oae (~seqf ~'*model* '~ts)
                              :let [~'oval (funnyqt.tg/value ~'oae ~attr)]]
                          (unify ~'a [~'ae ~'val] [~'oae ~'oval]))
                        (remove not)))))))))

(defn- create-reference-relation
  "Creates a relation for the given role name."
  [rn owners]
  (let [role-rel-sym (symbol (str "+->" rn))
        make-one   (fn [[^EdgeClass ec dir]]
                     (let [ec-rel-sym (symbol (str "+" (.getUniqueName ec)))]
                       (if (= dir :omega)
                          `(~ec-rel-sym ~'ign ~'sv ~'tv)
                          `(~ec-rel-sym ~'ign ~'tv ~'sv))))
        make (fn [tups]
               (if (> (count tups) 1)
                 `(conde
                   ~@(mapv (fn [t] [(make-one t)]) tups))
                 (make-one (first tups))))]
    `(defn ~role-rel-sym
       ~(format "A relation where `sv` references `tv` in its `%s` role." rn)
       [~'sv ~'tv]
       (fresh [~'ign]
         ~(make owners)))))

(defmacro generate-schema-relations
  "Generates schema-specific relations in the namespace denoted by `nssym`.
  If `nssym` is nil (or not given), generate them in the current namespace.
  `schema-file` is the TG file with the schema."
  ([schema-file] `(generate-schema-relations ~schema-file nil))
  ([schema-file nssym]
     (let [^Schema schema (funnyqt.tg/load-schema
                           (if (.exists (clojure.java.io/file schema-file))
                             schema-file
                             (clojure.java.io/resource schema-file)))
           atts (atom {}) ;; map from attribute names to set of attributed element classes that have it
           refs (atom {}) ;; map from role names to set of [edgeclass dir] tuples  that have it
           old-ns *ns*]
       `(do
          ~@(when nssym
              `[(ns ~nssym
                  (:refer-clojure :exclude [~'==]))

                (def ^:dynamic ~'*model*)
                (.setDynamic (var ~'*model*) true)
                (alter-meta! (var ~'*model*) assoc :dynamic true)])
          ~@(concat
             (doall
              (mapcat
               (fn [^VertexClass vc]
                 (doseq [a (mapv #(keyword (.getName ^Attribute %))
                                 (seq (.getOwnAttributeList vc)))]
                   (swap! atts
                          #(update-in %1 [%2] conj vc)
                          a))
                 (create-vc-relations vc))
               (seq (-> schema .getGraphClass .getVertexClasses))))
             (doall
              (mapcat
               (fn [^EdgeClass ec]
                 (doseq [a (mapv #(keyword (.getName ^Attribute %))
                                 (seq (.getOwnAttributeList ec)))]
                   (swap! atts
                          #(update-in %1 [%2] conj ec)
                          a))
                 (when-let [from-rn (-> ec .getFrom .getRolename)]
                   ;; Skip empty role names!
                   (when (seq from-rn)
                     (swap! refs
                            #(update-in %1 [%2] conj [ec :alpha])
                            from-rn)))
                 (when-let [to-rn (-> ec .getTo .getRolename)]
                   (when (seq to-rn)
                     (swap! refs
                            #(update-in %1 [%2] conj [ec :omega])
                            to-rn)))
                 (create-ec-relations ec))
               (seq (-> schema .getGraphClass .getEdgeClasses))))
             (doall
              (for [^Attribute a @atts]
                (create-attr-relation a)))
             (doall
              (for [[role owners] @refs]
                (create-reference-relation role owners))))
          (in-ns '~(ns-name old-ns))))))

