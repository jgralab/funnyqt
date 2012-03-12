(ns funnyqt.tg.test.transform
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query)
  (:use funnyqt.tg.transform)
  (:use funnyqt.tg.test.core)
  (:use funnyqt.generic)
  (:use clojure.test)
  (:import [de.uni_koblenz.jgralab.schema.impl SchemaImpl]))

(deftransformation transformation-1
  "Creates a graph with 4 vertices and 3 edges."
  [s]
  (create-vertices! s 'localities.City (fn [] [1 2]))
  (set-values! s 'NamedElement.name
               (fn [] {(r-elem 1) "Köln" (r-elem 2) "Frankfurt"}))
  (create-vertices! s 'junctions.Crossroad (fn [] ["a" "b"]))
  (create-edges! s 'localities.ContainsCrossroad
                 (fn [] [[1 (r-alpha 1) (r-omega "a")] [2 (r-alpha 2) (r-omega "b")]]))
  (create-edges! s 'connections.Street (fn [] [[1 (r-alpha "a") (r-omega "b")]])))

(deftest test-trans-1
  (let [g (transformation-1 (schema (rg)))]
    (is (= 4 (vcount g)))
    (is (= 3 (ecount g)))
    (is (= "Köln"      (value (vertex g 1) :name)))
    (is (= "Frankfurt" (value (vertex g 2) :name)))))

(deftransformation transformation-2
  "Creates a new schema and graph with 3 vertices and 3 edges."
  [s]
  (create-vertex-class! s {:qname 'Foo} (fn [] [1 2 3]))
  (create-attribute! s {:qname 'Foo.name :domain 'String}
                     (fn [] {(r-elem 1) "Hugo", (r-elem 2) "Klaus", (r-elem 3) "Dieter"}))
  (create-edge-class! s {:qname 'Foo2Foo
                         :from 'Foo
                         :to 'Foo}
                      (fn [] [[1 (r-alpha 1) (r-omega 2)]
                             [2 (r-alpha 2) (r-omega 3)]
                             [3 (r-alpha 3) (r-omega 1)]])))

(deftest test-trans-2
  (let [g (transformation-2 (create-schema 'foo.FooSchema 'FooGraph))]
    (is (= 3 (vcount g) (vcount g 'Foo)))
    (is (= 3 (ecount g) (ecount g 'Foo2Foo)))
    (loop [vs (vseq g) vals ["Hugo" "Klaus" "Dieter"]]
      (when (seq vs)
        (is (= (first vals) (value (first vs) :name)))
        (recur (next vs) (next vals))))))

(deftransformation transformation-3
  "Creates a new schema and graph with 2 vertices."
  []
  (create-vertex-class! {:qname 'Foo :abstract true})
  (create-vertex-class! {:qname 'Bar} (fn [] [1]))
  (create-vertex-class! {:qname 'Baz} (fn [] [2]))
  (create-attribute! {:qname 'Foo.name :domain 'String}
                     (fn [] {(r-elem 1) "Hugo", (r-elem 2) "Klaus"}))
  (add-sub-classes! 'Foo 'Bar 'Baz))

(deftest test-trans-3
  (let [g (transformation-3 ['foo3.FooSchema 'FooGraph])]
    (is (= 2 (vcount g) (vcount g 'Foo)))
    (is (= 0 (vcount g 'Foo!)))
    (is (= 1 (vcount g 'Bar)))
    (is (= 1 (vcount g 'Baz)))
    (is (= 0 (ecount g)))
    (loop [vs (vseq g) vals ["Hugo" "Klaus"]]
      (when (seq vs)
        (is (= (first vals) (value (first vs) :name)))
        (recur (next vs) (next vals))))))

(deftransformation transformation-4
  "Creates a Foo vertex for any Town in city-graph plus a Baz loop edge at
  every Foo."
  [city-graph]
  (create-vertex-class! {:qname 'Foo}
                        #(vseq city-graph 'localities.Town))
  (create-edge-class! {:qname 'Baz :from 'Foo :to 'Foo}
                      #(map (fn [t]
                              [t (r-alpha t) (r-omega t)])
                           (keys (img 'Foo)))))

(deftest test-trans-4
  (let [g (transformation-4 (rg) ['foo4.FooSchema 'FooGraph])]
    (is (= (vcount (rg) 'localities.Town)
           (vcount g    'Foo)))
    (is (= (vcount (rg) 'localities.Town)
           (ecount g    'Baz)))))

(deftransformation transformation-5
  "Tests record and enum domains."
  []
  (create-record-domain! 'Point {:x 'Long :y 'Long})
  (create-enum-domain! 'Cooliness ['YAY 'NAY])
  (create-vertex-class! {:qname 'Location} (fn [] [1 2 3]))
  (create-attribute! {:qname 'Location.coords :domain 'Point}
                     #(map (fn [a] [(r-elem a) (record (r-elem a)
                                                       'Point
                                                       {:x a :y a})])
                           (keys (img 'Location))))
  (create-attribute! {:qname 'Location.cool :domain 'Cooliness}
                     #(map (fn [a] [(r-elem a) (enum-constant
                                               (r-elem a)
                                               (rand-nth ['Cooliness.YAY
                                                          'Cooliness.NAY]))])
                           (keys (img 'Location)))))

(deftest test-trans-5
  (let [g (transformation-5 ['foo5.LocSchema2 'LocGraph])]
    ;;(show-graph g)
    (is (== 3 (vcount g)))
    (is (forall? #(== (id %)
                      (-> % (value :coords) (value :x))
                      (-> % (value :coords) (value :y)))
                 (vseq g)))
    (is (forall? #(or (= "YAY" (str (value % :cool)))
                      (= "NAY" (str (value % :cool))))
                 (vseq g)))))

(deftransformation families-to-genealogy
  "Transforms the family graph fg into a genealogy graph."
  [fg]
  (create-vertex-class! {:qname 'Person :abstract true})

  (create-vertex-class!
   {:qname 'Female}
   #(filter (fn [m] (seq (iseq m '[HasMother HasDaughter])))
            (vseq fg 'Member)))

  (create-vertex-class!
   {:qname 'Male}
   #(clojure.set/difference
     (set (vseq fg 'Member))
     (keys (img 'Person))))

  (add-sub-classes! 'Person 'Female 'Male)

  (create-vertex-class!
   {:qname 'Address}
   #(set (map (fn [f] [(value f :street) (value f :town)])
              (vseq fg 'Family))))

  (create-edge-class!
   {:qname 'HasRelative :abstract true
    :from  'Person      :to       'Person})

  (create-edge-class!
   {:qname 'HasSpouse
    :from  'Male   :from-multies [0,1] :from-role 'husband
    :to    'Female :to-multis    [0,1] :to-role   'wife}
   #(map (fn [f]
           [f,
            (r-alpha (the (reachables f [<>-- 'HasFather])))
            (r-omega (the (reachables f [<>-- 'HasMother])))])
         (vseq fg 'Family)))

  (create-edge-class!
   {:qname 'HasChild
    :from 'Person :from-multies [0,2] :from-role 'parents
    :to   'Person                     :to-role   'children}
   #(for [e (eseq fg '[HasSon, HasDaughter])
          par (reachables (alpha e) [<>-- '[HasFather, HasMother]])]
      (let [child (omega e)]
        [[child par] (r-alpha par) (r-omega child)])))

  (add-sub-classes! 'HasRelative 'HasSpouse 'HasChild)

  ;; Define some local helper fns
  (let [main-family (fn [m]
                      (if (seq (iseq m '[HasFather HasMother]))
                        (that (first (iseq m '[HasFather HasMother])))
                        (the (reachables m --<>))))
        address-tuple (fn [m]
                        (let [mf (main-family m)]
                          [(value mf :street) (value mf :town)]))]
    (create-edge-class!
     {:qname 'LivesAt
      :from  'Person
      :to    'Address :to-multis [1,1]}
     #(map (fn [m]
             [m (r-alpha m) (r-omega (address-tuple m))])
           (keys (img 'Person))))

    (create-attribute!
     {:qname 'Person.fullName :domain 'String}
     #(apply hash-map
             (mapcat
              (fn [m]
                [(r-elem m)
                 (str (value m :firstName)
                      " "
                      (value (main-family m)
                             :lastName))])
              (keys (img 'Person)))))

    (create-attribute!
     {:qname 'Address.street :domain 'String}
     #(apply hash-map
             (mapcat
              (fn [f] [(r-elem f) (first f)])
              (keys (img 'Address)))))

    (create-attribute!
     {:qname 'Address.town :domain 'String}
     #(apply hash-map
             (mapcat
              (fn [f] [(r-elem f) (second f)])
              (keys (img 'Address)))))))

(deftest test-families-to-genealogy
  (let [tg (families-to-genealogy
            (load-graph "test/input/familygraph.tg")
            '[de.genealogy.GenealogySchema Genealogy])]
    (is (== 13 (count (vseq tg 'Person))))
    (is (==  6 (count (vseq tg 'Male))))
    (is (==  7 (count (vseq tg 'Female))))
    (is (==  3 (count (vseq tg 'Address))))))

