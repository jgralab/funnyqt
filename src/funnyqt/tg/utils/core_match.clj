(ns funnyqt.tg.utils.core-match
  "An interface for using core.match on Vertices, Edges, and Graphs."
  (:use [clojure.core.match :only [IMatchLookup match]])
  (:use [funnyqt.utils :only [error add-long-doc!]])
  (:require [funnyqt.tg.core :as core])
  (:require [funnyqt.tg.query :as query]))

(add-long-doc!
 "Yeah, this doesn't have any public vars.  It just extends IMatchLookup to
vertices, edges, and graphs.")

;;* Code

(defn- match-attribute [this k not-found]
  (let [aec (core/attributed-element-class this)]
    (if (.containsAttribute aec (name k))
      (core/value this k)
      not-found)))

(defn- unsupported
  [k]
  (error (format "Key %s will be implemented when core.match supports predicate dispatch."
                 k)))

(extend-protocol IMatchLookup
  de.uni_koblenz.jgralab.Graph
  (val-at [this k not-found]
    (case k
      :+graph    this
      :+type     (unsupported k)
      (match-attribute this k not-found)))

  de.uni_koblenz.jgralab.Vertex
  (val-at [this k not-found]
    (case k
      :+vertex   this
      :+type     (unsupported k)
      :+incs     (query/iseq this)
      :+in-incs  (query/iseq this nil :in)
      :+out-incs (query/iseq this nil :out)
      (match-attribute this k not-found)))

  de.uni_koblenz.jgralab.Edge
  (val-at [this k not-found]
    (case k
      :+edge  this
      :+type  (unsupported k)
      :+alpha (core/alpha this)
      :+omega (core/omega this)
      (match-attribute this k not-found))))

(comment
  (def g (core/load-graph "test/greqltestgraph.tg"))

  (match [node]
    ;; That (:+type) will be doable with predicate dispatch
    [{:+type 'Cat :mice 4}]   :1
    [{:+type 'Dog :cats 3}]   :2
    [{:+type 'Mammal}]        :3
    :else :nope)

  (match [node]
    ;; That works right now...
    [{:+vertex (a :when (core/type-matcher node 'Cat)) :mice 4}]   :1
    [{:+vertex (b :when (core/type-matcher node 'Dog)) :cats 3}]   :2
    [{:+vertex (c :when (core/type-matcher node 'Mammal))}]        :3
    :else :nope))
