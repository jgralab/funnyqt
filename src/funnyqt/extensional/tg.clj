(ns funnyqt.extensional.tg
  "Specify TGraphs extensionally."
  (:require [funnyqt
             [extensional :as e]
             [utils :as u]]))

;;# Creating Elements

;;## Creating Vertices

(def ^{:arglists (:arglists (meta #'e/create-elements!))
       :doc (-> (:doc (meta #'e/create-elements!))
                (u/replace-word #"\bmodel\b" "graph")
                (u/replace-word #"\belements\b" "vertices")
                (u/replace-word #"\belement\b" "vertex")
                (u/replace-word #"\bmetamodel\sclass\b" "VertexClass"))}
  create-vertices! e/create-elements!)

;;## Creating Edges

(def ^{:arglists (:arglists (meta #'e/create-relationships!))
       :doc (-> (:doc (meta #'e/create-relationships!))
                (u/replace-word #"\bmodel\b" "graph")
                (u/replace-word #"\brelationship\b" "edge")
                (u/replace-word #"\brelationships\b" "edges")
                (u/replace-word #"\belement\b" "vertex")
                (u/replace-word #"\bmetamodel\sclass\b" "EdgeClass"))}
  create-edges! e/create-relationships!)

;;### Creating Edges implicitly via roles

(def ^{:arglists (:arglists (meta #'e/set-adjs!))
       :doc (-> (:doc (meta #'e/set-adjs!))
                (u/replace-word #"\bmodel\b" "graph")
                (u/replace-word #"\breference\b" "role")
                (u/replace-word #"\belements\b" "vertices")
                (u/replace-word #"\belement\b" "vertex")
                (u/replace-word #"\bmetamodel\sclass\b" "VertexClass"))}
  set-adjs! e/set-adjs!)

(def ^{:arglists (:arglists (meta #'e/add-adjs!))
       :doc (-> (:doc (meta #'e/add-adjs!))
                (u/replace-word #"\bmodel\b" "graph")
                (u/replace-word #"\breference\b" "role")
                (u/replace-word #"\belements\b" "vertices")
                (u/replace-word #"\belement\b" "vertex")
                (u/replace-word #"\bmetamodel\sclass\b" "VertexClass"))}
  add-adjs! e/add-adjs!)

;;## Setting Attribute Values

(def ^{:arglists (:arglists (meta #'e/set-avals!))
       :doc (-> (:doc (meta #'e/set-avals!))
                (u/replace-word #"\bmodel\b" "graph"))}
  set-values! e/set-avals!)
