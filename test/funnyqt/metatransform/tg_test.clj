(ns funnyqt.metatransform.tg-test
;  (:use funnyqt.tg-test)
  (:require [funnyqt.query :as q]
            [funnyqt.tg :as tg]
            [funnyqt.metatransform.tg :as mtg]
            [funnyqt.extensional :as e]
            [funnyqt.extensional.tg :as etg])
  (:use [clojure.test :only [deftest is]])
  (:import
   (de.uni_koblenz.jgralab.schema Attribute AttributedElementClass)))

;;* Tests


(e/deftransformation transformation-2
  "Creates 1 VC and one EC."
  [g]
  (mtg/create-vertex-class! g 'Person (fn [] [1 2 3 4 5]))
  (mtg/create-attribute! g 'Person :name 'String "\"Fritz\""
                         (fn [] {(etg/resolve-element 1) "Hugo"
                                 (etg/resolve-element 2) "Peter"
                                 (etg/resolve-element 3) "August"}))
  (mtg/create-attribute! g 'Person :birthday 'String
                         (fn [] {(etg/resolve-element 3) "1980-11-01"
                                 (etg/resolve-element 4) "1970-06-22"
                                 (etg/resolve-element 5) "1975-01-01"}))

  (mtg/create-vertex-class! g 'SpecialPerson (fn [] [:a :b]))
  (mtg/create-attribute! g 'SpecialPerson :lastName 'String
                         (fn [] {(etg/resolve-element :a) "Müller"
                                 (etg/resolve-element :b) "Meier"}))

  (mtg/add-sub-classes! g 'Person 'SpecialPerson)

  (mtg/create-edge-class! g 'Knows 'Person 'Person
                          (fn [] (map (fn [[arch a o]]
                                        [arch (etg/resolve-alpha a) (etg/resolve-omega o)])
                                      [[1 1 2] [2 2 3] [3 3 4] [4 4 5] [5 5 1]
                                       [6 1 :a] [7 2 :b]]))))

(deftest test-transformation-2
  (let [g (mtg/empty-graph 'test.transformation2.T2Schema 'T2Graph)]
    (transformation-2 g)
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

;;** Inheritance hierarchy

(defn ^:private top-sibs-bottom [g]
  (when (seq (.getVertexClasses (.getGraphClass (tg/schema g))))
    (throw (RuntimeException. "BANG")))
  (mtg/create-vertex-class! g 'Top (fn [] [:t]))
  (mtg/create-vertex-class! g 'Sibling1 (fn [] [:s1]))
  (mtg/create-vertex-class! g 'Sibling2 (fn [] [:s2]))
  (mtg/create-vertex-class! g 'Bottom (fn [] [:b])))

(e/deftransformation multiple-inheritance-0
  [g]
  (top-sibs-bottom g)
  (mtg/create-attribute! g 'Top :name 'String
                         (fn [] {(etg/resolve-element :t) "Top"}))
  (mtg/add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  (mtg/add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-0
  (let [g (mtg/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
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
  (mtg/create-attribute! g 'Top :name 'String
                         (fn [] {(etg/resolve-element :t) "Top"}))

  (mtg/create-attribute! g 'Bottom :name 'String
                         (fn [] {(etg/resolve-element :b) "Bottom"}))

  (mtg/add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  ;; This must error because Bottom already has a name attribute so it must not
  ;; inherit another one.
  (mtg/add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-1
  (let [g (mtg/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Bottom already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-1 g)))))

(e/deftransformation multiple-inheritance-2
  [g]
  (top-sibs-bottom g)
  (mtg/create-attribute! g 'Sibling1 :name 'String
                         (fn [] {(etg/resolve-element :s1) "Sib1"}))

  (mtg/create-attribute! g 'Sibling2 :name 'String
                         (fn [] {(etg/resolve-element :s2) "Sib2"}))
  (mtg/add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  ;; This must fail, cause Bottom inherits name from both Sibling1 and
  ;; Sibling2.
  (mtg/add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-2
  (let [g (mtg/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception
                          #"Bottom tries to inherit two different :name attributes, one from Sibling1 and one from Sibling2"
                          (multiple-inheritance-2 g)))))

(e/deftransformation multiple-inheritance-3
  [g]
  (top-sibs-bottom g)
  (mtg/create-attribute! g 'Top :name 'String
                         (fn [] {(etg/resolve-element :t) "Top"}))

  (mtg/create-attribute! g 'Sibling1 :name 'Long
                         (fn [] {(etg/resolve-element :s1) 11}))

  ;; This must fail, cause Sibling1 inherits name from Top, but defines a name
  ;; attribute itself.
  (mtg/add-sub-classes! g 'Top 'Sibling1))

(deftest test-multiple-inheritance-3
  (let [g (mtg/empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Sibling1 already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-3 g)))))

;;## Attribute renames

(e/deftransformation attr-rename-0
  [g]
  ;; Must error, cause name is actually inherited by NamedElement and not
  ;; declared for Locality itself
  (mtg/rename-attribute! g 'Locality :name :inhabitants))

(deftest test-attr-rename-0
  (is (thrown-with-msg? Exception #"Cannot rename attribute :name for class localities.Locality because it's owned by NamedElement"
                        (attr-rename-0 (tg/load-graph "test/input/greqltestgraph.tg")))))


(e/deftransformation attr-rename-1
  [g]
  ;; Must error, cause subclass Locality already declares inhabitants
  (mtg/rename-attribute! g 'NamedElement :name :inhabitants))

(deftest test-attr-rename-1
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (tg/load-graph "test/input/greqltestgraph.tg")))))

(e/deftransformation attr-rename-2
  [g]
  ;; Must error, cause Locality already declares inhabitants
  (mtg/rename-attribute! g 'localities.Locality :year :inhabitants))

(deftest test-attr-rename-2
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (tg/load-graph "test/input/greqltestgraph.tg")))))

(e/deftransformation attr-rename-3
  [g]
  ;; Should work
  (mtg/rename-attribute! g 'NamedElement :name :id))

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

;;** Attribute deletions

(defn ^:private attr-seq
  [ae]
  (map #(.getName ^Attribute %1)
       (.getAttributeList (tg/attributed-element-class ae))))

(e/deftransformation delete-attr-1-setup
  [g]
  (let [abc [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z]]
    (mtg/create-vertex-class! g 'Node (fn [] abc))
    (doseq [a abc]
      (mtg/create-attribute! g 'Node a 'String
                             (fn [] (zipmap (map etg/resolve-element abc)
                                            (repeat (name a))))))))

(e/deftransformation delete-attr-1
  "Deletes a random Node attribute."
  [g]
  (mtg/delete-attribute! g 'Node (rand-nth (attr-seq (tg/first-vertex g)))))

(deftest test-delete-attr-1
  (let [g (mtg/empty-graph 'foo.bar.BazSchema 'BazGraph)
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
