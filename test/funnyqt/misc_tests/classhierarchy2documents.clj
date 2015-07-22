(ns funnyqt.classhierarchy2documents
  (:require [funnyqt.generic :as g]
            [funnyqt.visualization :as v]
            [funnyqt.tg :as tg]
            [funnyqt.relational :as r]
            [funnyqt.bidi :refer :all]
            [clojure.core.logic :as ccl]
            [clojure.test :as test]))

(r/generate-metamodel-relations "test/input/classhierarchy.tg" test.classhierarchyschema c)
(r/generate-metamodel-relations "test/input/documents.tg" test.documentschema d)

(defn sample-class-graph []
  (let [g (tg/new-graph (tg/load-schema "test/input/classhierarchy.tg"))
        obj (doto (tg/create-vertex! g 'Class)
              (tg/set-value! :name "Object"))
        obs (doto (tg/create-vertex! g 'Class)
              (tg/set-value! :name "Observable"))
        ser (doto (tg/create-vertex! g 'Class)
              (tg/set-value! :name "Serializable"))
        per (doto (tg/create-vertex! g 'Class)
              (tg/set-value! :name "Person"))
        employer (doto (tg/create-vertex! g 'Class)
                   (tg/set-value! :name "Employer"))
        employee (doto (tg/create-vertex! g 'Class)
                   (tg/set-value! :name "Employee"))]
    (tg/create-edge! g 'HasSuperClass obs obj)
    (tg/create-edge! g 'HasSuperClass ser obj)
    (tg/create-edge! g 'HasSuperClass per obs)
    (tg/create-edge! g 'HasSuperClass per ser)
    (tg/create-edge! g 'HasSuperClass employer per)
    (tg/create-edge! g 'HasSuperClass employee per)
    g))

;; (v/print-model (sample-class-graph) :gtk)

(deftransformation classhierarchy2documents [[classes docs]]
  (^:top class2doc
         :left [(c/Class classes ?c)
                (c/name classes ?c ?name)]
         :right [(d/Document docs ?d)
                 (d/name docs ?d ?name)])
  (^:top generalization2directlinks
         :when [(relateo class2doc :?c ?subclass :?d ?srcdoc)
                (relateo class2doc :?c ?superclass :?d ?trgdoc)]
         :left [(c/->superclasses classes ?subclass ?superclass)]
         :right [(d/->trgs docs ?srcdoc ?trgdoc)])
  (^:top generalization2transitivelinks
         :when [(ccl/conde
                 [(relateo generalization2directlinks :?srcdoc ?a :?trgdoc ?b)
                  (relateo generalization2directlinks :?srcdoc ?b :?trgdoc ?c)]
                 [(d/->alltrgs docs ?a ?b)
                  (d/->alltrgs docs ?b ?c)])]
         :right [(d/->alltrgs docs ?a ?b)
                 (d/->alltrgs docs ?b ?c)
                 (d/->alltrgs docs ?a ?c)]))

(test/deftest test-classhierarchy2documents
  (let [c (sample-class-graph)
        d (tg/new-graph (tg/load-schema "test/input/documents.tg"))
        c2 (tg/new-graph (tg/schema c))]
    (classhierarchy2documents c d :right)
    ;;(v/print-model d :gtk)
    (classhierarchy2documents c2 d :left)
    ;;(v/print-model c2 :gtk)
    (test/is (g/equal-models? c c2))))
