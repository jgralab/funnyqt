(ns funnyqt.extensional.emf
  "Specify EMF models extensionally."
  (:require clojure.set
            [funnyqt
             [extensional :as e]
             [utils :as u]]))

;;# Creating Elements

;;## Creating EObjects

(def ^{:arglists (:arglists (meta #'e/create-elements!))
       :doc (-> (:doc (meta #'e/create-elements!))
                (u/replace-word #"\bmodel\b" "resource")
                (u/replace-word #"\belements\b" "eobjects")
                (u/replace-word #"\belement\b" "eobject")
                (u/replace-word #"\bmetamodel\sclass\b" "EClass"))}
  create-eobjects! e/create-elements!)

;;## Setting Features Values

(def ^{:arglists (:arglists (meta #'e/set-avals!))
       :doc (-> (:doc (meta #'e/set-avals!))
                (u/replace-word #"\bmodel\b" "resource"))}
  set-values! e/set-avals!)

(def ^{:arglists (:arglists (meta #'e/set-adjs!))
       :doc (-> (:doc (meta #'e/set-adjs!))
                (u/replace-word #"\bmodel\b" "resource")
                (u/replace-word #"\breference\b" "EReference")
                (u/replace-word #"\belements\b" "eobjects")
                (u/replace-word #"\belement\b" "eobject")
                (u/replace-word #"\bmetamodel\sclass\b" "EClass"))}
  set-erefs! e/set-adjs!)

(def ^{:arglists (:arglists (meta #'e/set-adjs!))
       :doc (-> (:doc (meta #'e/set-adjs!))
                (u/replace-word #"\bmodel\b" "resource")
                (u/replace-word #"\breference\b" "EReference")
                (u/replace-word #"\belements\b" "eobjects")
                (u/replace-word #"\belement\b" "eobject")
                (u/replace-word #"\bmetamodel\sclass\b" "EClass"))}
  add-erefs! e/add-adjs!)
