(ns funnyqt.coevo.tg-test
  (:require [funnyqt.query :as q]
            [funnyqt.tg :as tg]
            [funnyqt.coevo.tg :as coevo]
            [funnyqt.extensional :as e]
            [funnyqt.extensional.tg :as etg]
            [funnyqt.utils :as u]
            [funnyqt.generic :as g])
  (:use [clojure.test :only [deftest is test-all-vars]])
  (:import
   (de.uni_koblenz.jgralab.schema Attribute AttributedElementClass)
   (de.uni_koblenz.jgralab.schema.exception SchemaException)))

;;# Tests
;;## Basic Tests
(e/deftransformation transformation-0
  "Creates 1 VC and one EC."
  [g]
  (coevo/create-vertex-class! g 'Person (fn [] [1 2 3 4 5]))
  (coevo/create-attribute! g 'Person :name 'String "\"Fritz\""
                           (fn [] {(e/resolve-element 1) "Hugo"
                                   (e/resolve-element 2) "Peter"
                                   (e/resolve-element 3) "August"}))
  (coevo/create-attribute! g 'Person :birthday 'String
                           (fn [] {(e/resolve-element 3) "1980-11-01"
                                   (e/resolve-element 4) "1970-06-22"
                                   (e/resolve-element 5) "1975-01-01"}))

  (coevo/create-vertex-class! g 'SpecialPerson (fn [] [:a :b]))
  (coevo/create-attribute! g 'SpecialPerson :lastName 'String
                           (fn [] {(e/resolve-element :a) "Müller"
                                   (e/resolve-element :b) "Meier"}))

  (coevo/create-specialization! g 'Person 'SpecialPerson)

  (coevo/create-edge-class! g 'Knows 'Person 'Person
                            (fn [] (map (fn [[arch a o]]
                                          [arch (e/resolve-source a) (e/resolve-target o)])
                                        [[1 1 2] [2 2 3] [3 3 4] [4 4 5] [5 5 1]
                                         [6 1 :a] [7 2 :b]]))))

(deftest test-transformation-0
  (let [g (coevo/empty-graph 'test.transformation2.T2Schema 'T2Graph)]
    (transformation-0 g)
    (is (== 7 (tg/vcount g)))
    (is (== 7 (tg/ecount g)))
    (is (== 2 (tg/vcount g 'SpecialPerson)))
    ;; Every person has its name set
    (is (q/forall? #(tg/value % :name)
                   (tg/vseq g 'Person)))
    ;; Every special person has its lastName set to Müller or Meier
    (is (q/forall? #(#{"Müller" "Meier"} (tg/value % :lastName))
                   (tg/vseq g 'SpecialPerson)))
    ;; There are 3 persons with a set birthday tg/value
    (is (== 3 (count (filter (fn [p] (tg/value p :birthday))
                             (tg/vseq g 'Person)))))))

;;## Inheritance hierarchy

;;### Creating Specializations

(defn ^:private top-sibs-bottom [g]
  (when (seq (.getVertexClasses (.getGraphClass (tg/schema g))))
    (throw (RuntimeException. "BANG")))
  (coevo/create-vertex-class! g 'Top (fn [] [:t]))
  (coevo/create-vertex-class! g 'Sibling1 (fn [] [:s1]))
  (coevo/create-vertex-class! g 'Sibling2 (fn [] [:s2]))
  (coevo/create-vertex-class! g 'Bottom (fn [] [:b]))
  [@e/*arch* @e/*img*])

(e/deftransformation multiple-inheritance-0
  [g]
  (top-sibs-bottom g)
  (coevo/create-attribute! g 'Top :name 'String
                           (fn [] {(e/resolve-element :t) "Top"}))
  (coevo/create-specialization! g 'Top 'Sibling1)
  (coevo/create-specialization! g 'Top 'Sibling2)
  (coevo/create-specialization! g 'Sibling1 'Bottom)
  (coevo/create-specialization! g 'Sibling2 'Bottom))

(deftest test-multiple-inheritance-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (multiple-inheritance-0 g)
    (is (== 4 (tg/vcount g)))
    (is (== 4 (tg/vcount g 'Top)))
    (is (== 2 (tg/vcount g 'Sibling1)))
    (is (== 2 (tg/vcount g 'Sibling1)))
    (is (== 1 (tg/vcount g 'Bottom)))
    (q/forall? #(is (== 1 (tg/vcount g %1)))
               '[Top! Sibling1! Sibling2! Bottom!])))


(e/deftransformation multiple-inheritance-1
  [g]
  (top-sibs-bottom g)
  (coevo/create-attribute! g 'Top :name 'String
                           (fn [] {(e/resolve-element :t) "Top"}))

  (coevo/create-attribute! g 'Bottom :name 'String
                           (fn [] {(e/resolve-element :b) "Bottom"}))

  (coevo/create-specialization! g 'Top 'Sibling1)
  (coevo/create-specialization! g 'Top 'Sibling2)
  ;; This must error because Bottom already has a name attribute so it must not
  ;; inherit another one.
  (coevo/create-specialization! g 'Sibling1 'Bottom)
  (coevo/create-specialization! g 'Sibling2 'Bottom))

(deftest test-multiple-inheritance-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Bottom already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-1 g)))))

(e/deftransformation multiple-inheritance-2
  [g]
  (top-sibs-bottom g)
  (coevo/create-attribute! g 'Sibling1 :name 'String
                           (fn [] {(e/resolve-element :s1) "Sib1"}))

  (coevo/create-attribute! g 'Sibling2 :name 'String
                           (fn [] {(e/resolve-element :s2) "Sib2"}))
  (coevo/create-specialization! g 'Top 'Sibling1)
  (coevo/create-specialization! g 'Top 'Sibling2)
  ;; This must fail, cause Bottom inherits name from both Sibling1 and
  ;; Sibling2.
  (coevo/create-specialization! g 'Sibling1 'Bottom)
  (coevo/create-specialization! g 'Sibling2 'Bottom))

(deftest test-multiple-inheritance-2
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception
                          #"Bottom tries to inherit two different :name attributes, one from Sibling1 and one from Sibling2"
                          (multiple-inheritance-2 g)))))

(e/deftransformation multiple-inheritance-3
  [g]
  (top-sibs-bottom g)
  (coevo/create-attribute! g 'Top :name 'String
                           (fn [] {(e/resolve-element :t) "Top"}))

  (coevo/create-attribute! g 'Sibling1 :name 'Long
                           (fn [] {(e/resolve-element :s1) 11}))

  ;; This must fail, cause Sibling1 inherits name from Top, but defines a name
  ;; attribute itself.
  (coevo/create-specialization! g 'Top 'Sibling1))

(deftest test-multiple-inheritance-3
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Sibling1 already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-3 g)))))

(e/deftransformation ec-inheritance-0 [g]
  (top-sibs-bottom g)
  (coevo/create-edge-class! g 'SuperEdge 'Sibling1 'Sibling2)
  (coevo/create-edge-class! g 'SubEdge 'Bottom 'Sibling2)
  ;; This must fail because SubEdge's source VC Bottom is no subcass of
  ;; Sibling1.
  (coevo/create-specialization! g 'SuperEdge 'SubEdge))

(deftest test-ec-inheritance-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Can't make SubEdge subclass of SuperEdge because SubEdge's source element class Bottom is no subclass of or equal to SuperEdge's source element class Sibling1."
                          (ec-inheritance-0 g)))))

(e/deftransformation ec-inheritance-1 [g]
  (top-sibs-bottom g)
  (coevo/create-edge-class! g 'SuperEdge 'Sibling1 'Sibling2)
  (coevo/create-edge-class! g 'SubEdge 'Sibling1 'Bottom)
  ;; This must fail because SubEdge's target VC Bottom is no subcass of
  ;; Sibling2.
  (coevo/create-specialization! g 'SuperEdge 'SubEdge))

(deftest test-ec-inheritance-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Can't make SubEdge subclass of SuperEdge because SubEdge's target element class Bottom is no subclass of or equal to SuperEdge's target element class Sibling2."
                          (ec-inheritance-1 g)))))

(e/deftransformation ec-inheritance-2 [g]
  (top-sibs-bottom g)
  (etg/create-vertices! g 'Sibling1 (fn [] [1 2 3]))
  (etg/create-vertices! g 'Sibling2 (fn [] [1 2 3]))
  (coevo/create-edge-class! g 'SuperEdge 'Sibling1 'Sibling2
                            (fn []
                              [[1 (e/resolve-source 1) (e/resolve-target 1)]]))
  (coevo/create-edge-class! g 'SubEdge 'Sibling1 'Sibling2
                            (fn []
                              [[1 (e/resolve-source 1) (e/resolve-target 1)]]))
  ;; This must fail because the archetypes aren't disjoint.
  (coevo/create-specialization! g 'SuperEdge 'SubEdge))

(deftest test-ec-inheritance-2
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Bijectivity violation: can't make SubEdge subclass of SuperEdge because their sets of archetypes are not disjoint. Common archetypes: \(1\)"
                          (ec-inheritance-2 g)))))

;;### Deleting Specializations

(e/deftransformation delete-spec-base [g]
  (e/with-merged-trace-mappings (top-sibs-bottom g)
    (coevo/create-specialization! g 'Top 'Sibling1)
    (coevo/create-specialization! g 'Top 'Sibling2)
    (coevo/create-specialization! g 'Sibling1 'Bottom)
    (coevo/create-specialization! g 'Sibling2 'Bottom)
    (coevo/create-attribute! g 'Top :t 'String (fn [] {(e/resolve-element :t)  "t-top"
                                                       (e/resolve-element :s1) "t-s1"
                                                       (e/resolve-element :s2) "t-s2"
                                                       (e/resolve-element :b)  "t-bottom"}))
    (coevo/create-attribute! g 'Sibling1 :s1 'String (fn [] {(e/resolve-element :s1) "s1-s1"
                                                             (e/resolve-element :b)  "s1-bottom"}))
    (coevo/create-attribute! g 'Sibling2 :s2 'String (fn [] {(e/resolve-element :s2) "s2-s2"
                                                             (e/resolve-element :b)  "s2-bottom"}))
    (coevo/create-attribute! g 'Bottom :b 'String (fn [] {(e/resolve-element :b)  "b-bottom"}))

    [@e/*arch* @e/*img*]))

(e/deftransformation delete-spec-0 [g]
  (delete-spec-base g)
  (coevo/delete-specialization! g 'Top 'Sibling2))

(deftest test-delete-spec-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (delete-spec-0 g)
    (let [s2 (first (tg/vseq g 'Sibling2))
          s2-vc (tg/attributed-element-class s2)]
      (is (= 1 (.getAttributeCount s2-vc)))
      (is (= ["s2"] (map #(.getName ^Attribute %)
                         (.getAttributeList s2-vc))))
      (is (= "s2-s2" (tg/value s2 :s2))))))

(e/deftransformation delete-spec-1 [g]
  (delete-spec-0 g)
  (coevo/delete-specialization! g 'Top 'Sibling1))

(deftest test-delete-spec-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (delete-spec-1 g)
    ;; (funnyqt.visualization/print-model g :gtk)
    (let [s1 (first (tg/vseq g 'Sibling1))
          s1-vc (tg/attributed-element-class s1)
          s2 (first (tg/vseq g 'Sibling2))
          s2-vc (tg/attributed-element-class s2)
          b (first (tg/vseq g 'Bottom))
          b-vc (tg/attributed-element-class b)]
      (is (= 1 (.getAttributeCount s1-vc)))
      (is (= ["s1"] (map #(.getName ^Attribute %)
                         (.getAttributeList s1-vc))))
      (is (= "s1-s1" (tg/value s1 :s1)))

      (is (= 1 (.getAttributeCount s2-vc)))
      (is (= ["s2"] (map #(.getName ^Attribute %)
                         (.getAttributeList s2-vc))))
      (is (= "s2-s2" (tg/value s2 :s2)))

      (is (= 3 (.getAttributeCount b-vc)))
      (is (= ["b" "s1" "s2"] (map #(.getName ^Attribute %)
                                  (.getAttributeList b-vc))))
      (is (= "s1-bottom" (tg/value b :s1)))
      (is (= "s2-bottom" (tg/value b :s2)))
      (is (= "b-bottom"  (tg/value b :b))))))

(e/deftransformation delete-indirect-spec [g]
  (delete-spec-base g)
  ;; This must fail because Bottom is an indirect subclass of Top.
  (coevo/delete-specialization! g 'Top 'Bottom))

(deftest test-delete-indirect-spec
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"Top is no direct superclass of Bottom"
                          (delete-indirect-spec g)))))

(e/deftransformation delete-nonexisting-spec [g]
  (delete-spec-base g)
  ;; This must fail because Sibling1 and Sibling2 are completely unrelated wrt
  ;; specialization.
  (coevo/delete-specialization! g 'Sibling1 'Sibling2))

(deftest test-delete-nonexisting-spec
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"Sibling1 is no direct superclass of Sibling2"
                          (delete-nonexisting-spec g)))))

(e/deftransformation delete-ec-spec-base [g]
  (e/with-merged-trace-mappings (delete-spec-base g)
    (coevo/create-edge-class! g 'T2T 'Top 'Top
                              (fn []
                                [[:t2t (e/resolve-source :t) (e/resolve-target :t)]]))
    (coevo/create-attribute! g 'T2T :t2t 'String
                             (fn []
                               {(e/resolve-element :t2t) "t-t2t"}))
    (coevo/create-edge-class! g 'T2S2 'Top 'Sibling2
                              (fn []
                                [[:t2s2 (e/resolve-source :t) (e/resolve-target :s2)]]))
    (coevo/create-specialization! g 'T2T 'T2S2)
    (coevo/create-edge-class! g 'S12T 'Sibling1 'Top
                              (fn []
                                [[:s12t (e/resolve-source :s1) (e/resolve-target :t)]]))
    (coevo/create-specialization! g 'T2T 'S12T)
    (coevo/create-edge-class! g 'S12S2 'Sibling1 'Sibling2
                              (fn []
                                [[:s12s2 (e/resolve-source :s1) (e/resolve-target :s2)]]))
    (coevo/create-specialization! g 'S12T 'S12S2)
    (coevo/create-specialization! g 'T2S2 'S12S2)
    [@e/*arch* @e/*img*]))

(e/deftransformation delete-vc-spec-with-conn-ecs-0 [g]
  (delete-ec-spec-base g)
  ;; This must fail because Sibling1 has ECs which are derived from an T2T
  ;; from/to Top.
  (coevo/delete-specialization! g 'Top 'Sibling1))

(deftest test-delete-vc-spec-with-conn-ecs-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"Cannot remove superclass Top from Sibling1 because the EdgeClass S12S2 specializes an EdgeClass starting or ending at Sibling1 or one of its superclasses."
                          (delete-vc-spec-with-conn-ecs-0 g)))))

(e/deftransformation delete-indirect-ec-spec [g]
  (delete-ec-spec-base g)
  ;; This must fail because Bottom is only an indirect subclass of Top
  (coevo/delete-specialization! g 'T2T 'S12S2))

(deftest test-delete-indirect-ec-spec
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"T2T is no direct superclass of S12S2 so cannot remove it as such."
                          (delete-indirect-ec-spec g)))))

(e/deftransformation delete-ec-spec-0 [g]
  (delete-ec-spec-base g)
  (coevo/delete-specialization! g 'T2T 'T2S2))

(deftest test-delete-ec-spec-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (delete-ec-spec-0 g)
    (let [e (first (tg/eseq g 'T2S2))
          e-ec (tg/attributed-element-class e)]
      (is (= 0 (.getAttributeCount e-ec)))
      (is (= [] (.getAttributeList e-ec))))))

;;## GC/VC/EC renames

(e/deftransformation aec-renames-0 [g]
  (top-sibs-bottom g)
  (coevo/create-edge-class! g 'Top2Bottom 'Top 'Bottom
                            (fn []
                              [[1 (e/resolve-source :t) (e/resolve-target :b)]]))
  (coevo/rename-attributed-element-class! g 'Top 'vcs.T)
  (coevo/rename-attributed-element-class! g 'Bottom 'vcs.B)
  (coevo/rename-attributed-element-class! g 'Top2Bottom 'ecs.T2B))

(deftest test-aec-renames-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (aec-renames-0 g)
    (is (= 4 (tg/vcount g)))
    (is (= 1 (tg/ecount g)))
    (is (= 1
           (tg/vcount g 'vcs.T)
           (tg/vcount g 'vcs.B)
           (tg/vcount g 'Sibling1)
           (tg/vcount g 'Sibling2)))
    (is (= 1 (tg/ecount g 'ecs.T2B)))))

(e/deftransformation aec-renames-1 [g]
  (top-sibs-bottom g)
  ;; Ensure *img*/*arch* are hash-maps instead of array maps which have no
  ;; problem with hash changes in the keys.
  (reset! e/*img* (apply hash-map (mapcat identity @e/*img*)))
  (reset! e/*arch* (apply hash-map (mapcat identity @e/*arch*)))
  (when-not (= clojure.lang.PersistentHashMap
               (class @e/*img*)
               (class @e/*arch*))
    (u/errorf "Error during setup of test: *img*/*arch* aren't hash-sets."))
  (coevo/rename-attributed-element-class! g 'Top 'T)
  [(tg/attributed-element-class g 'T) @e/*img* @e/*arch*])

(deftest test-aec-renames-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)
        [tvc img arch] (aec-renames-1 g)]
    ;; The hash of VC T (formerly Top) changed.  It must still be uplookable in
    ;; *img*/*arch*.
    (is (img tvc))
    (is (= (img tvc) {:t (first (tg/vseq g 'T))}))
    (is (arch tvc))
    (is (= (arch tvc) {(first (tg/vseq g 'T)) :t}))))

;;## GC/VC/EC deletes

(e/deftransformation ec-delete-0 [g]
  (e/with-merged-trace-mappings (delete-ec-spec-base g)
    ;; Should delete all EdgeClasses and all edges
    (coevo/delete-graph-element-class! g 'T2T)
    [@e/*arch* @e/*img*]))

(deftest test-ec-delete-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)
        [arch img] (ec-delete-0 g)]
    (is (= [] (.getEdgeClasses (.getGraphClass (tg/schema g)))))
    (is (= [] (tg/eseq g)))
    (doseq [[k _] arch]
      (is (tg/vertex-class? k)))
    (doseq [[k _] img]
      (is (tg/vertex-class? k)))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class T2T"
                          (tg/attributed-element-class g 'T2T)))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class T2S2"
                          (tg/attributed-element-class g 'T2S2)))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class S12T"
                          (tg/attributed-element-class g 'S12T)))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class S12S2"
                          (tg/attributed-element-class g 'S12S2)))))

(e/deftransformation ec-delete-1 [g]
  (e/with-merged-trace-mappings (delete-ec-spec-base g)
    ;; Should delete EdgeClasses T2S2 and S12S2 because that's a subclass.
    (coevo/delete-graph-element-class! g 'T2S2)
    [@e/*arch* @e/*img*]))

(deftest test-ec-delete-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)
        [arch img] (ec-delete-1 g)]
    (is (= ['T2T 'S12T] (map g/qname (.getEdgeClasses (.getGraphClass (tg/schema g))))))
    (is (q/forall? #(g/has-type? % '[:or T2T S12T]) (tg/eseq g)))
    (doseq [[k _] arch]
      (is (or (tg/vertex-class? k)
              (and (tg/edge-class? k)
                   (#{'T2T 'S12T} (g/qname k))))))
    (doseq [[k _] img]
      (is (or (tg/vertex-class? k)
              (and (tg/edge-class? k)
                   (#{'T2T 'S12T} (g/qname k))))))
    (is (= 2 (count (tg/eseq g))))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class T2S2"
                          (tg/attributed-element-class g 'T2S2)))
    (is (thrown-with-msg? Exception
                          #"No such attributed element class S12S2"
                          (tg/attributed-element-class g 'S12S2)))))

(e/deftransformation vc-delete-with-conn-ecs-0 [g]
  (delete-ec-spec-base g)
  ;; Must not work because there's still the EC T2T connected to Top.
  (coevo/delete-graph-element-class! g 'Top))

(deftest test-vc-delete-with-conn-ecs-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"Cannot delete vertex class Top because there are still connected edge classes: \[S12T, T2S2, T2T\]"
                          (vc-delete-with-conn-ecs-0 g)))))

(e/deftransformation vc-delete-with-conn-ecs-1 [g]
  (delete-ec-spec-base g)
  ;; Must not work because there's still the ECs S12T S12S2 connected to
  ;; Sibling1.
  (coevo/delete-graph-element-class! g 'Sibling1))

(deftest test-vc-delete-with-conn-ecs-1
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? SchemaException
                          #"Cannot delete vertex class Sibling1 because there are still connected edge classes: \[S12T, T2S2, T2T, S12S2\]"
                          (vc-delete-with-conn-ecs-1 g)))))

(e/deftransformation vc-ec-delete-0 [g]
  (delete-ec-spec-base g)
  ;; Delete all edge classes
  (coevo/delete-graph-element-class! g 'T2T)
  ;; Now delete VC Top non-recursively
  (coevo/delete-specialization! g 'Top 'Sibling1)
  (coevo/delete-specialization! g 'Top 'Sibling2)
  (coevo/delete-graph-element-class! g 'Top))

(deftest test-vc-ec-delete-0
  (let [g (coevo/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (vc-ec-delete-0 g)
    (is (= 0 (count (tg/eseq g))))
    (is (= 3 (count (tg/vseq g))))
    (let [s1 (first (tg/vseq g 'Sibling1))
          s1-vc (tg/attributed-element-class s1)
          s2 (first (tg/vseq g 'Sibling2))
          s2-vc (tg/attributed-element-class s2)
          b  (first (tg/vseq g 'Bottom))
          b-vc (tg/attributed-element-class b)]
      (is s1)
      (is s2)
      (is b)
      (is (= ["s1"] (map #(.getName ^Attribute %)
                         (.getAttributeList s1-vc))))
      (is (= ["s2"] (map #(.getName ^Attribute %)
                         (.getAttributeList s2-vc))))
      (is (= ["b" "s1" "s2"] (map #(.getName ^Attribute %)
                                  (.getAttributeList b-vc))))
      (is (= "s1-s1" (tg/value s1 :s1)))
      (is (= "s2-s2" (tg/value s2 :s2)))
      (is (= "s1-bottom" (tg/value b :s1)))
      (is (= "s2-bottom" (tg/value b :s2)))
      (is (= "b-bottom" (tg/value b :b))))))

;;## Attribute renames

(e/deftransformation attr-rename-0
  [g]
  ;; Must error, cause name is actually inherited by NamedElement and not
  ;; declared for Locality itself
  (coevo/rename-attribute! g 'Locality :name :inhabitants))

(deftest test-attr-rename-0
  (is (thrown-with-msg? Exception #"Cannot rename attribute :name for class localities.Locality because it's owned by NamedElement"
                        (attr-rename-0 (tg/load-graph "test/input/greqltestgraph.tg")))))


(e/deftransformation attr-rename-1
  [g]
  ;; Must error, cause subclass Locality already declares inhabitants
  (coevo/rename-attribute! g 'NamedElement :name :inhabitants))

(deftest test-attr-rename-1
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (tg/load-graph "test/input/greqltestgraph.tg")))))

(e/deftransformation attr-rename-2
  [g]
  ;; Must error, cause Locality already declares inhabitants
  (coevo/rename-attribute! g 'localities.Locality :year :inhabitants))

(deftest test-attr-rename-2
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (tg/load-graph "test/input/greqltestgraph.tg")))))

(e/deftransformation attr-rename-3
  [g]
  ;; Should work
  (coevo/rename-attribute! g 'NamedElement :name :id))

(deftest test-attr-rename-3
  (let [g (tg/load-graph "test/input/greqltestgraph.tg")
        attr-map (fn [attr]
                   (apply hash-map (mapcat (fn [ne]
                                             [ne (tg/value ne attr)])
                                           (tg/vseq g 'NamedElement))))
        name-map (attr-map :name)]
    (attr-rename-3 g)
    (is (not (.containsAttribute
              ^AttributedElementClass (tg/attributed-element-class g 'NamedElement)
              "name")))
    (is (= name-map (attr-map :id)))))

;;## Attribute deletions

(defn ^:private attr-seq
  [ae]
  (map #(.getName ^Attribute %1)
       (.getAttributeList (tg/attributed-element-class ae))))

(e/deftransformation delete-attr-1-setup
  [g]
  (let [abc [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z]]
    (coevo/create-vertex-class! g 'Node (fn [] abc))
    (doseq [a abc]
      (coevo/create-attribute! g 'Node a 'String
                               (fn [] (zipmap (map e/resolve-element abc)
                                              (repeat (name a))))))))

(e/deftransformation delete-attr-1
  "Deletes a random Node attribute."
  [g]
  (coevo/delete-attribute! g 'Node (rand-nth (attr-seq (tg/first-vertex g)))))

(deftest test-delete-attr-1
  (let [g (coevo/empty-graph 'foo.bar.BazSchema 'BazGraph)
        check (fn [g]
                (is (== 26 (tg/vcount g)))
                ;; For all nodes, all existing attributes have a tg/value that
                ;; corresponds to the attribute name.
                (is (q/forall? (fn [n]
                                 (q/forall? (fn [a] (= a (tg/value n a)))
                                            (attr-seq n)))
                               (tg/vseq g))))]
    (delete-attr-1-setup g)
    (check g)
    (dotimes [i 26]
      (delete-attr-1 g)
      (check g))))
