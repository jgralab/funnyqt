(ns funnyqt.metatransform.tg-test
  (:use funnyqt.tg)
  (:use funnyqt.metatransform.tg)
  (:use funnyqt.extensional)
  (:use funnyqt.extensional.tg)
  (:use funnyqt.tg-test)
  (:require [funnyqt.query :as q])
  (:use clojure.test)
  (:import
   (de.uni_koblenz.jgralab.schema Attribute AttributedElementClass)))

;;* Tests


(deftransformation transformation-2
  "Creates 1 VC and one EC."
  [g]
  (create-vertex-class! g 'Person (fn [] [1 2 3 4 5]))
  (create-attribute! g 'Person :name 'String "\"Fritz\""
                     (fn [] {(resolve-element 1) "Hugo"
                             (resolve-element 2) "Peter"
                             (resolve-element 3) "August"}))
  (create-attribute! g 'Person :birthday 'String
                     (fn [] {(resolve-element 3) "1980-11-01"
                             (resolve-element 4) "1970-06-22"
                             (resolve-element 5) "1975-01-01"}))

  (create-vertex-class! g 'SpecialPerson (fn [] [:a :b]))
  (create-attribute! g 'SpecialPerson :lastName 'String
                     (fn [] {(resolve-element :a) "Müller"
                             (resolve-element :b) "Meier"}))

  (add-sub-classes! g 'Person 'SpecialPerson)

  (create-edge-class! g 'Knows 'Person 'Person
                      (fn [] (map (fn [[arch a o]]
                                    [arch (resolve-alpha a) (resolve-omega o)])
                                  [[1 1 2] [2 2 3] [3 3 4] [4 4 5] [5 5 1]
                                   [6 1 :a] [7 2 :b]]))))

(deftest test-transformation-2
  (let [g (empty-graph 'test.transformation2.T2Schema 'T2Graph)]
    (transformation-2 g)
    (is (== 7 (vcount g)))
    (is (== 7 (ecount g)))
    (is (== 2 (vcount g 'SpecialPerson)))
    ;; Every person has its name set
    (is (q/forall? #(value % :name)
                   (vseq g 'Person)))
    ;; Every special person has its lastName set to Müller or Meier
    (is (q/forall? #(#{"Müller" "Meier"} (value % :lastName))
                   (vseq g 'SpecialPerson)))
    ;; There are 3 persons with a set birthday value
    (is (== 3 (count (filter (fn [p] (value p :birthday))
                             (vseq g 'Person)))))))

;;** Inheritance hierarchy

(defn ^:private top-sibs-bottom [g]
  (when (seq (.getVertexClasses (.getGraphClass (schema g))))
    (throw (RuntimeException. "BANG")))
  (create-vertex-class! g 'Top (fn [] [:t]))
  (create-vertex-class! g 'Sibling1 (fn [] [:s1]))
  (create-vertex-class! g 'Sibling2 (fn [] [:s2]))
  (create-vertex-class! g 'Bottom (fn [] [:b])))

(deftransformation multiple-inheritance-0
  [g]
  (top-sibs-bottom g)
  (create-attribute! g 'Top :name 'String
                     (fn [] {(resolve-element :t) "Top"}))
  (add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  (add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-0
  (let [g (empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (multiple-inheritance-0 g)
    (is (== 4 (vcount g)))
    (is (== 4 (vcount g 'Top)))
    (is (== 2 (vcount g 'Sibling1)))
    (is (== 2 (vcount g 'Sibling1)))
    (is (== 1 (vcount g 'Bottom)))
    (q/forall? #(is (== 1 (vcount g %1)))
               '[Top! Sibling1! Sibling2! Bottom!])))


(deftransformation multiple-inheritance-1
  [g]
  (top-sibs-bottom g)
  (create-attribute! g 'Top :name 'String
                     (fn [] {(resolve-element :t) "Top"}))

  (create-attribute! g 'Bottom :name 'String
                     (fn [] {(resolve-element :b) "Bottom"}))

  (add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  ;; This must error because Bottom already has a name attribute so it must not
  ;; inherit another one.
  (add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-1
  (let [g (empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Bottom already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-1 g)))))

(deftransformation multiple-inheritance-2
  [g]
  (top-sibs-bottom g)
  (create-attribute! g 'Sibling1 :name 'String
                     (fn [] {(resolve-element :s1) "Sib1"}))

  (create-attribute! g 'Sibling2 :name 'String
                     (fn [] {(resolve-element :s2) "Sib2"}))
  (add-sub-classes! g 'Top 'Sibling1 'Sibling2)
  ;; This must fail, cause Bottom inherits name from both Sibling1 and
  ;; Sibling2.
  (add-super-classes! g 'Bottom 'Sibling1 'Sibling2))

(deftest test-multiple-inheritance-2
  (let [g (empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception
                          #"Bottom tries to inherit two different :name attributes, one from Sibling1 and one from Sibling2"
                          (multiple-inheritance-2 g)))))

(deftransformation multiple-inheritance-3
  [g]
  (top-sibs-bottom g)
  (create-attribute! g 'Top :name 'String
                     (fn [] {(resolve-element :t) "Top"}))

  (create-attribute! g 'Sibling1 :name 'Long
                     (fn [] {(resolve-element :s1) 11}))

  ;; This must fail, cause Sibling1 inherits name from Top, but defines a name
  ;; attribute itself.
  (add-sub-classes! g 'Top 'Sibling1))

(deftest test-multiple-inheritance-3
  (let [g (empty-graph 'test.multi_inherit.MISchema 'MIGraph)]
    (is (thrown-with-msg? Exception #"Sibling1 already has a :name attribute so cannot inherit another one"
                          (multiple-inheritance-3 g)))))

;;## Attribute renames

(deftransformation attr-rename-0
  [g]
  ;; Must error, cause name is actually inherited by NamedElement and not
  ;; declared for Locality itself
  (rename-attribute! g 'Locality :name :inhabitants))

(deftest test-attr-rename-0
  (is (thrown-with-msg? Exception #"Cannot rename attribute :name for class localities.Locality because it's owned by NamedElement"
                        (attr-rename-0 (load-graph "test/input/greqltestgraph.tg")))))


(deftransformation attr-rename-1
  [g]
  ;; Must error, cause subclass Locality already declares inhabitants
  (rename-attribute! g 'NamedElement :name :inhabitants))

(deftest test-attr-rename-1
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (load-graph "test/input/greqltestgraph.tg")))))

(deftransformation attr-rename-2
  [g]
  ;; Must error, cause Locality already declares inhabitants
  (rename-attribute! g 'localities.Locality :year :inhabitants))

(deftest test-attr-rename-2
  (is (thrown-with-msg? Exception #"NamedElement subclass localities.Locality already has a :inhabitants attribute"
                        (attr-rename-1 (load-graph "test/input/greqltestgraph.tg")))))

(deftransformation attr-rename-3
  [g]
  ;; Should work
  (rename-attribute! g 'NamedElement :name :id))

(deftest test-attr-rename-3
  (let [g (load-graph "test/input/greqltestgraph.tg")
        attr-map (fn [attr]
                   (apply hash-map (mapcat (fn [ne]
                                             [ne (value ne attr)])
                                           (vseq g 'NamedElement))))
        name-map (attr-map :name)]
    (attr-rename-3 g)
    (is (not (.containsAttribute
              ^AttributedElementClass (attributed-element-class g 'NamedElement)
              "name")))
    (is (= name-map (attr-map :id)))))

;;** Attribute deletions

(defn ^:private attr-seq
  [ae]
  (map #(.getName ^Attribute %1)
       (.getAttributeList (attributed-element-class ae))))

(deftransformation delete-attr-1-setup
  [g]
  (let [abc [:a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z]]
    (create-vertex-class! g 'Node (fn [] abc))
    (doseq [a abc]
      (create-attribute! g 'Node a 'String
                         (fn [] (zipmap (map resolve-element abc)
                                        (repeat (name a))))))))

(deftransformation delete-attr-1
  "Deletes a random Node attribute."
  [g]
  (delete-attribute! g 'Node (rand-nth (attr-seq (first-vertex g)))))

(deftest test-delete-attr-1
  (let [g (empty-graph 'foo.bar.BazSchema 'BazGraph)
        check (fn [g]
                (is (== 26 (vcount g)))
                ;; For all nodes, all existing attributes have a value that
                ;; corresponds to the attribute name.
                (is (q/forall? (fn [n]
                                 (q/forall? (fn [a] (= a (value n a)))
                                            (attr-seq n)))
                               (vseq g))))]
    (delete-attr-1-setup g)
    (check g)
    (dotimes [i 26]
      (delete-attr-1 g)
      (check g))))
