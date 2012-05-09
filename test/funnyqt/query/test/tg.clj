(ns funnyqt.query.test.tg
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:use funnyqt.utils)
  (:use funnyqt.tg)
  (:use funnyqt.query.tg)
  (:use funnyqt.test.tg)
  (:use clojure.test)
  (:import
   (de.uni_koblenz.jgralab.schema AttributedElementClass)))

;; Don't print that much on failing assertions of large collections.
(use-fixtures :once (fn [f]
                      (binding [*print-length* 5]
                        (f))))

;;* The tests themselves

(deftest test-vcount
  (is (= 155 (vcount (rg)) (count (vseq (rg)))))
  (is (= 3 (vcount (rg) 'localities.City)))
  (is (= 6808 (vcount (jg)) (count (vseq (jg))))))

(deftest test-ecount
  (is (= 355 (ecount (rg)) (count (eseq (rg)))))
  ;; Edge has no direct instances
  (is (= 3 (ecount (rg) 'connections.AirRoute!)))
  (is (= 6801 (ecount (jg)) (count (eseq (jg))))))

(deftest test-query-1
  (let [r (for [l (vseq (rg) 'localities.Locality)
		:when (re-matches #".*e.*" (value l 'name))]
	    [l (value l 'name)])]
    ;; r must have 8 elems
    (is (= 8 (count r)))
    ;; the elems are...
    (is (= (map #(let [v (vertex (rg) %1)] [v (value v 'name)])
		[1 2 3 4 7 9 10 11])
	   r))))

(deftest test-incidences
  (is (= 1 (count (iseq (vertex (rg) 1)))))
  (is (= 10 (count (iseq (vertex (rg) 12)))))
  (is (= 9 (count (iseq (vertex (rg) 12)
			'localities.ContainsLocality!))))
  (is (= 1 (count (iseq (vertex (rg) 12)
			'localities.HasCapital!))))
  (is (= 2 (count (iseq (vertex (rg) 6)))))
  (is (= 4 (count (iseq (vertex (rg) 11)))))
  (is (= 0 (count (iseq (vertex (rg) 6) nil :out)))))

(deftest test-adjs
  (is (member?(vertex (rg) 6)
	      (adjs (vertex (rg) 12)
                    'localities)))
  (is (= 131 (count (adjs (vertex (rg) 12)
                          'localities 'crossroads))))
  (is (member? (vertex (rg) 39)
	       (adjs (vertex (rg) 12)
                     'localities 'crossroads))))

(deftest test-average-inhabitants
  (let [locs (vseq (rg) 'localities.Locality)]
    (is (< 0.00000000000000000001 ;; epsilon
	   (- 91079.63636363637   ;; the GReQL computed val
	      (/ (reduce + (map #(value %1 :inhabitants)
				locs))))))))
(deftest test-this
  (doseq [v (vseq (rg))
	  e (iseq v)]
    (is (= v (this e)))))

(deftest test-avg-founding-year
  (is (== 13480/11
          ;; straight-forward
          (let [years (map #(value (value %1 :foundingDate) :year)
                           (vseq (rg) 'localities.Locality))]
            (/ (reduce + years)
               (count years)))
          ;; reduce-values
          (/ (reduce-values + 0 (vseq (rg) 'localities.Locality)
                            :foundingDate #(value % :year))
             (count (vseq (rg) 'localities.Locality)))
          ;; reduce-values even more convenient
          (let [locs (vseq (rg) 'localities.Locality)]
            (/ (reduce-values + 0 locs :foundingDate :year)
               (count locs))))))

(deftest test-all-localities-with-o
  (is (= 4 (count (filter #(re-matches #".*o.*" (value % :name))
			  (vseq (rg) 'localities.Locality))))))

(deftest test-all-capitals
  (is (= 2 (count (map omega
		       (eseq (rg) 'localities.HasCapital))))))

(deftest test-type-matchers
  (is (= (vseq (rg) ['junctions.Airport 'localities.City])
         (vseq (rg) [:or 'junctions.Airport 'localities.City]))))


;;** Traversal Context

(deftest test-vsubgraph-tc
  (let [vcnt (vcount (rg))
        ecnt (ecount (rg))]
    (testing "vertex induced TraversalContext by set"
      (on-subgraph [(rg) (vsubgraph (rg) (set (map #(vertex (rg) %)
                                                   [1 12 7])))]
        (is (== 3 (vcount (rg))))
        (is (== 2 (ecount (rg))))
        (testing "on-graph 1"
          (on-graph [(rg)]
            (is (== vcnt) (vcount (rg)))
            (is (== ecnt) (ecount (rg)))))
        (testing "vertex/edge on subgraph"
          ;; These are all in
          (is (= [1 7 12] (map id (vseq (rg)))))
          (is (= [17 22]  (map id (eseq (rg))))))))
    (testing "vertex induced TraversalContext by type"
      (on-subgraph [(rg) (vsubgraph (rg) 'junctions.Airport)]
        (is (== 3 (vcount (rg))))
        (is (== 3 (ecount (rg))))
        (testing "on-graph 2"
          (on-graph [(rg)]
            (is (== vcnt) (vcount (rg)))
            (is (== ecnt) (ecount (rg)))))))
    (testing "vertex induced TraversalContext by predicate"
      ;; Subgraph of all Locality vertices with more than 10 inhabitants.
      (let [locality? (type-matcher (rg) 'localities.Locality)]
        (on-subgraph [(rg) (vsubgraph (rg)
                                      #(and (locality? %)
                                            (> (value % :inhabitants) 10)))]
          (is (== 9 (vcount (rg))))
          (testing "on-graph 3"
            (on-graph [(rg) nil]
              (is (== vcnt) (vcount (rg)))
              (is (== ecnt) (ecount (rg)))))
          (is (== 0 (ecount (rg)))))))))


(deftest test-esubgraph-tc
  (let [vcnt (vcount (rg))
        ecnt (ecount (rg))]
    (testing "edge induced TraversalContext by set"
      (on-subgraph [(rg) (esubgraph (rg) (set (map #(edge (rg) %)
                                                   [17 22])))]
        (is (== 3 (vcount (rg))))
        (is (== 2 (ecount (rg))))
        (testing "on-graph 4"
          (on-graph [(rg)]
            (is (== vcnt) (vcount (rg)))
            (is (== ecnt) (ecount (rg)))))))
    (testing "edge induced TraversalContext by type"
      (on-subgraph [(rg) (esubgraph (rg) 'connections.AirRoute)]
        (is (== 3 (vcount (rg))))
        (is (== 3 (ecount (rg))))
        (testing "on-graph 5"
          (on-graph [(rg)]
            (is (== vcnt) (vcount (rg)))
            (is (== ecnt) (ecount (rg)))))))
    (testing "edge induced TraversalContext by predicate"
      (let [airroute? (type-matcher (rg) 'connections.AirRoute)]
        (on-subgraph [(rg) (esubgraph (rg) #(and (airroute? %)
                                                 (== (value (alpha  %) :inhabitants) 0)))]
          (testing "on-graph 5"
            (on-graph [(rg)]
              (is (== vcnt) (vcount (rg)))
              (is (== ecnt) (ecount (rg)))))
          (is (== 2 (vcount (rg))))
          (is (== 1 (ecount (rg)))))))))

(deftest test-subgraph-intersection-tcs
  (on-subgraph [(rg) (vsubgraph (rg) (set (map #(vertex (rg) %)
                                                          [1 12 7])))]
    (on-subgraph-intersection [(rg) (esubgraph (rg) (set (map #(edge (rg) %)
                                                                   [22 17])))]
      (is (== 3 (vcount (rg))))
      (is (== 2 (ecount (rg))))
      (on-subgraph-intersection [(rg) (esubgraph (rg) #{(edge (rg) 22)})]
        (is (== 2 (vcount (rg))))
        (is (== 1 (ecount (rg))))))))

;;** Path expression tests

(deftest test--->
  (doall (map #(is (= %1 %2))
	      (let [m (map id (reachables (vertex (rg) 12) -->))]
		;; There are 9 reachable unique vertices
		(is (= 9 (count m)))
		m)
	      ;; and that's the order (by ids)
	      [7 6 3 4 1 2 10 11 5])))

(deftest test-<--
  (is (= 0 (count (reachables (vertex (rg) 12) <--)))))

(deftest test-<->
  (doall (map #(is (= %1 %2))
	      (let [m (map id (reachables (vertex (rg) 12) <->))]
		;; There are 9 reachable unique vertices
		(is (= 9 (count m)))
		m)
	      ;; and that's the order (by ids)
	      [7 6 3 4 1 2 10 11 5])))

(deftest test-reachable-vertices
  (is (= 2 (count (reachables (vertex (rg) 1)
			   [p-seq --<> [p-* [--> 'localities.HasCapital]]]))))
  (is (= 4272 (count (reachables (vertex (jg) 12) [p-* -->]))
	      (count (reachables (vertex (jg) 12) [p-* -->]))))
  (is (= 4272 (count (reachables (vertex (jg) 12) [p-+ -->]))))
  (is (= 6117 (count (reachables (vertex (jg) 12) [p-* <->]))))
  (is (= 6117 (count (reachables (vertex (jg) 12) [p-+ <->]))))
  (is (= 19 (count (reachables (vertex (jg) 12) [p-+ <>--]))))
  (is (= 20 (count (reachables (vertex (jg) 12) [p-* <>--]))))
  (is (= 22 (count (reachables (vertex (jg) 12) [p-seq [p-* <>--] -->]))
	    (count (reachables (vertex (jg) 12) [p-seq [p-* <>--] -->]))))
  (is (= 4272 (count (reachables (vertex (jg) 12) [p-seq [p-* <>--] [p-+ -->]]))))
  (is (= 2337 (count (reachables (vertex (jg) 12) [p-+ [p-seq <>-- -->]]))))
  (is (= 6 (count (reachables (vertex (jg) 12)
                              [p-seq
                               [p-+ [p-seq <>-- -->]]
                               [p-restr  'annotations.Annotable]]))))
  (is (= 3280 (count (reachables (vertex (jg) 12)
                                 [p-seq [p-opt --<>]
                                  [p-+ [p-seq <>-- -->]]
                                  [p-opt <--]]))))
  (is (= 6 (count (reachables (vertex (jg) 12) [p-alt <>-- --<>])))))

(deftest test-p-exp
  (is (= (reachables (vertex (jg) 12) [p-seq --> --> -->])
	 (reachables (vertex (jg) 12) [p-exp 3 -->])))
  (is (= (reachables (vertex (jg) 12) -->)
	 (reachables (vertex (jg) 12) [p-exp 1 -->])))
  (is (= (to-oset (vertex (jg) 12))
	 (reachables (vertex (jg) 12) [p-exp 0 -->])))
  (is (= (reachables (vertex (jg) 12) [p-seq --> --> --> [p-opt -->] [p-opt -->] [p-opt -->]])
         (reachables (vertex (jg) 12) [p-exp 3 6 -->])))
  (is (= (reachables (vertex (jg) 12) [p-seq [p-opt -->] [p-opt -->] [p-opt -->]])
         (reachables (vertex (jg) 12) [p-exp 0 3 -->]))))

(deftest test-derived-from-state
  (let [start (the (filter #(= (value %1 :name) "State")
			   (vseq (jg) 'classifiers.Class)))]
    ;; test with only restrictions on the edge class types
    (is (= 11
	   (count
	    (reachables
	     start
	     [p-seq
	      [p-+
	       [p-seq
		[<-- 'types.ClassifierReferenceLinksToTarget]
		[--<> 'types.NamespaceClassifierReferenceContainsClassifierReferences]
		[--<> 'classifiers.ClassContainsExtends]]]
	      [p-restr 'classifiers.Class
	       (fn [v]
		 (empty? (filter
			  #(type-of? %1 'modifiers.Abstract)
			  (adjs v :annotationsAndModifiers))))]]))))))

(defn coupled-classes
  "Given a Class `c`, calculates all coupled classes."
  [c]
  (reachables c
    [p-seq [<>-- 'IsClassBlockOf] [<>-- 'IsMemberOf]
     [<-- ['IsBodyOfMethod 'IsFieldCreationOf]]
     [p-* [<-- 'IsStatementOf]]
     [p-alt
      ;; Classes whose methods are called by c
      [<-- 'IsDeclarationOfInvokedMethod]
      ;; Classes whose Fields are accessed by c
      [p-seq [<-- 'IsDeclarationOfAccessedField] [--> 'IsFieldCreationOf]]]
     [--<> 'IsMemberOf] [--<> 'IsClassBlockOf]
     [p-restr nil #(not (= c %1))]]))

(defn coupling-between-objects [c]
  (count (coupled-classes c)))

;;** Evaluate a simple binary tree

(def bin-tree
  (memoize
   #(let [g (create-graph
             (load-schema
              "test/input/binop-tree-schema.tg" :standard)
             "ExampleBinaryGraph" :standard)
          v1 (create-vertex! g 'Div)
          v2 (create-vertex! g 'Add)
          v3 (create-vertex! g 'Sub)
          v4 (create-vertex! g 'Mul)
          v5 (doto (create-vertex! g 'Const) (set-value! :value 3.0))
          v6 (doto (create-vertex! g 'Const) (set-value! :value 42.0))
          v7 (doto (create-vertex! g 'Const) (set-value! :value 2.0))
          v8 (doto (create-vertex! g 'Const) (set-value! :value 7.0))
          v9 (doto (create-vertex! g 'Const) (set-value! :value 9.0))]
      (doseq [[a o] [[v1 v2] [v1 v3] [v2 v4] [v2 v5] [v3 v6] [v3 v7]
                     [v4 v8] [v4 v9]]]
        (create-edge! g 'HasArg a o))
      g)))

(defn eval-bin-tree [v]
  (let [eval-args #(map eval-bin-tree
                        (--> % 'HasArg))]
    (cond
     (type-of? v 'Const) (value v :value)
     (type-of? v 'Add)   (reduce + (eval-args v))
     (type-of? v 'Sub)   (reduce - (eval-args v))
     (type-of? v 'Mul)   (reduce * (eval-args v))
     (type-of? v 'Div)   (reduce / (eval-args v)))))

(defprotocol BinTreeEval (eval-exp [this]))

(defn- schema-class [g qn]
  (.getSchemaClass
   ^AttributedElementClass (attributed-element-class g qn)))

(let [g (bin-tree)
      eval-args #(map eval-exp (--> % 'HasArg))]
  (extend-type (schema-class g 'Const) BinTreeEval
    (eval-exp [c] (value c :value)))
  (extend-type (schema-class g 'Add)   BinTreeEval
    (eval-exp [b] (reduce + (eval-args b))))
  (extend-type (schema-class g 'Sub)   BinTreeEval
    (eval-exp [b] (reduce - (eval-args b))))
  (extend-type (schema-class g 'Mul)   BinTreeEval
    (eval-exp [b] (reduce * (eval-args b))))
  (extend-type (schema-class g 'Div)   BinTreeEval
    (eval-exp [b] (reduce / (eval-args b)))))

(deftest test-bin-tree-eval
  (is (== 1.65
          (eval-bin-tree (vertex (bin-tree) 1))
          (eval-exp (vertex (bin-tree) 1)))))
