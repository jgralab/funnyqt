(ns funnyqt.misc-tests.classhierarchy2documents
  (:require [funnyqt.query :as q]
            [funnyqt.generic :as g]
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
                   (tg/set-value! :name "Employee"))
        subemployee (doto (tg/create-vertex! g 'Class)
                      (tg/set-value! :name "SubEmployee"))]
    (tg/create-edge! g 'HasSuperClass obs obj)
    (tg/create-edge! g 'HasSuperClass ser obj)
    (tg/create-edge! g 'HasSuperClass per obs)
    (tg/create-edge! g 'HasSuperClass per ser)
    (tg/create-edge! g 'HasSuperClass employer per)
    (tg/create-edge! g 'HasSuperClass employee per)
    (tg/create-edge! g 'HasSuperClass subemployee employee)
    g))

;; (v/print-model (sample-class-graph) :gtk)

(deftransformation classhierarchy2documents [classes docs]
  (^:top class2doc
         :left [(c/Class classes ?c)
                (c/name classes ?c ?name)]
         :right [(d/Document docs ?d)
                 (d/name docs ?d ?name)])
  (^:top generalization2directlinks
         :when [(relateo :class2doc :?c ?subclass :?d ?srcdoc)
                (relateo :class2doc :?c ?superclass :?d ?trgdoc)]
         :left [(c/->superclasses classes ?subclass ?superclass)]
         :right [(d/->trgs docs ?srcdoc ?trgdoc)])
  (transitive-linko [a b]
                    (ccl/conde
                     [(relateo :generalization2directlinks :?srcdoc a :?trgdoc b)]
                     [(ccl/fresh [x]
                        (relateo :generalization2directlinks :?srcdoc a :?trgdoc x)
                        (transitive-linko x b))]))
  (^:top generalization2transitivelinks
         :when [(transitive-linko ?a ?b)]
         :right [(d/->alltrgs docs ?a ?b)]))

(defn doc-by-name [docs name]
  (q/the #(= name (tg/value % :name))
         (tg/vseq docs 'Document)))

(defn assert-all-trgs [d src & trgs]
  (test/is (= (set (map (partial doc-by-name d) trgs))
              (set (g/adjs (doc-by-name d src) :alltrgs)))))

(test/deftest test-classhierarchy2documents
  (let [c (sample-class-graph)
        d (tg/new-graph (tg/load-schema "test/input/documents.tg"))
        c2 (tg/new-graph (tg/schema c))]
    (classhierarchy2documents c d :right)
    (assert-all-trgs d "SubEmployee" "Employee" "Person" "Observable" "Serializable" "Object")
    (assert-all-trgs d "Employee" "Person" "Observable" "Serializable" "Object")
    (assert-all-trgs d "Employer" "Person" "Observable" "Serializable" "Object")
    (assert-all-trgs d "Person" "Observable" "Serializable" "Object")
    (assert-all-trgs d "Observable" "Object")
    (assert-all-trgs d "Serializable" "Object")
    ;;(v/print-model d :gtk)
    (classhierarchy2documents c2 d :left)
    ;;(v/print-model c2 :gtk)
    (test/is (g/equal-models? c c2))))
