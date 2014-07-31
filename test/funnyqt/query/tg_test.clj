(ns funnyqt.query.tg-test
  (:use funnyqt.tg
        funnyqt.query.tg)
  (:require [funnyqt.generic :as g]
            [funnyqt.query   :as q]
            [funnyqt.utils   :as u]
            [funnyqt.tg-test :refer [rg jg]])
  (:use [clojure.test]))

(deftest test--->
  (mapv #(is (= %1 %2 %3))
        ;; TG -->
        (let [m (map id (q/reachables (vertex rg 12) -->))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; Generic -->
        (let [m (map id (q/reachables (vertex rg 12) q/-->))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; and that's the order (by ids)
        [7 6 3 4 1 2 10 11 5]))

(deftest test-<--
  (is (= 0
         (count (q/reachables (vertex rg 12) <--))
         (count (q/reachables (vertex rg 12) q/<--)))))

(deftest test-<->
  (mapv #(is (= %1 %2 %3))
        (let [m (map id (q/reachables (vertex rg 12) <->))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        (let [m (map id (q/reachables (vertex rg 12) q/<->))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; and that's the order (by ids)
        [7 6 3 4 1 2 10 11 5]))

(deftest test-reachable-vertices
  (is (= 2
         (count (q/reachables (vertex rg 1)
                              [q/p-seq --<> [q/p-* [--> 'localities.HasCapital]]]))
         (count (q/reachables (vertex rg 1)
                              [q/p-seq --<> [q/p-* [q/--> 'localities.HasCapital]]]))))
  (is (= 4272
         (count (q/reachables (vertex jg 12) [q/p-* -->]))
         (count (q/reachables (vertex jg 12) [q/p-* q/-->]))))
  (is (= 4272
         (count (q/reachables (vertex jg 12) [q/p-+ -->]))
         (count (q/reachables (vertex jg 12) [q/p-+ q/-->]))))
  (is (= 6117
         (count (q/reachables (vertex jg 12) [q/p-* <->]))
         (count (q/reachables (vertex jg 12) [q/p-* q/<->]))))
  (is (= 6117
         (count (q/reachables (vertex jg 12) [q/p-+ <->]))
         (count (q/reachables (vertex jg 12) [q/p-+ q/<->]))))
  (is (= 19
         (count (q/reachables (vertex jg 12) [q/p-+ <*>--]))
         (count (q/reachables (vertex jg 12) [q/p-+ q/<>--]))))
  (is (= 20
         (count (q/reachables (vertex jg 12) [q/p-* <*>--]))
         (count (q/reachables (vertex jg 12) [q/p-* q/<>--]))))
  (is (= 22
         (count (q/reachables (vertex jg 12) [q/p-seq [q/p-* <*>--] -->]))
         (count (q/reachables (vertex jg 12) [q/p-seq [q/p-* q/<>--] q/-->]))))
  (is (= 4272
         (count (q/reachables (vertex jg 12) [q/p-seq [q/p-* <*>--] [q/p-+ -->]]))
         (count (q/reachables (vertex jg 12) [q/p-seq [q/p-* q/<>--] [q/p-+ q/-->]]))))
  (let [tg (q/reachables (vertex jg 12) [q/p-+ [q/p-seq <*>-- -->]])
        ge (q/reachables (vertex jg 12) [q/p-+ [q/p-seq q/<>-- q/-->]])]
    (is (= 2337 (count tg) (count ge)))
    (is (= tg ge)))
  (is (= 6
         (count (q/reachables (vertex jg 12)
                              [q/p-seq
                               [q/p-+ [q/p-seq <*>-- -->]]
                               [q/p-restr  'annotations.Annotable]]))
         (count (q/reachables (vertex jg 12)
                                [q/p-seq
                                 [q/p-+ [q/p-seq q/<>-- q/-->]]
                                 [q/p-restr  'annotations.Annotable]]))))
  (let [tg (q/reachables (vertex jg 12)
                                [q/p-seq [q/p-opt --<*>]
                                 [q/p-+ [q/p-seq <*>-- -->]]
                                 [q/p-opt <--]])
        ge (q/reachables (vertex jg 12)
                              [q/p-seq [q/p-opt q/--<>]
                               [q/p-+ [q/p-seq q/<>-- q/-->]]
                               [q/p-opt q/<--]])]
    (is (= 3280 (count tg) (count ge)))
    (is (= tg ge)))
  (is (= 6
         (count (q/reachables (vertex jg 12) [q/p-alt <*>-- --<*>]))
         (count (q/reachables (vertex jg 12) [q/p-alt q/<>-- q/--<>])))))

(deftest test-p-exp
  (is (= (q/reachables (vertex jg 12) [q/p-seq --> --> -->])
         (q/reachables (vertex jg 12) [q/p-seq q/--> q/--> q/-->])
	 (q/reachables (vertex jg 12) [q/p-exp 3 -->])
         (q/reachables (vertex jg 12) [q/p-exp 3 q/-->])))
  (is (= (q/reachables (vertex jg 12) -->)
         (q/reachables (vertex jg 12) q/-->)
	 (q/reachables (vertex jg 12) [q/p-exp 1 -->])
         (q/reachables (vertex jg 12) [q/p-exp 1 q/-->])))
  (is (= (u/oset (vertex jg 12))
	 (q/reachables (vertex jg 12) [q/p-exp 0 -->])))
  (is (= (q/reachables (vertex jg 12) [q/p-seq --> --> -->
                                       [q/p-opt -->] [q/p-opt -->] [q/p-opt -->]])
         (q/reachables (vertex jg 12) [q/p-seq q/--> q/--> q/-->
                                       [q/p-opt q/-->] [q/p-opt q/-->] [q/p-opt q/-->]])
         (q/reachables (vertex jg 12) [q/p-exp 3 6 -->])))
  (is (= (q/reachables (vertex jg 12) [q/p-seq [q/p-opt -->] [q/p-opt -->] [q/p-opt -->]])
         (q/reachables (vertex jg 12) [q/p-exp 0 3 -->]))))

(deftest test-p-+*
  (is (= (q/reachables (vertex jg 1) [q/p-+ <->])
         (q/reachables (vertex jg 1) [q/p-seq <-> [q/p-* <->]])))
  (is (contains? (q/reachables (vertex jg 1) [q/p-* <*>--])
                 (vertex jg 1)))
  (is (not (contains? (q/reachables (vertex jg 1) [q/p-+ <*>--])
                      (vertex jg 1)))))

(deftest test-p-+*2
  (doseq [p [[q/p-seq <-> <->]
             <>--
             <*>--
             <_>--
             [q/p-alt [q/p-seq --> -->]
                    [q/p-seq <-- <--]]]]
    (doseq [vid [1 20 117 3038]]
      (is (= (q/reachables (vertex jg vid) [q/p-+ p])
             (q/reachables (vertex jg vid) [q/p-seq p [q/p-* p]])))
      (is (= (q/reachables (vertex jg vid) [q/p-* p])
             (q/reachables (vertex jg vid) [q/p-opt [q/p-+ p]]))))))

(deftest test-derived-from-state
  (let [start (q/the (filter #(= (value %1 :name) "State")
                             (vseq jg 'classifiers.Class)))]
    (let [tg (q/reachables
              start
              [q/p-seq
               [q/p-+
                [q/p-seq
                 [<-- 'types.ClassifierReferenceLinksToTarget]
                 [--<*> 'types.NamespaceClassifierReferenceContainsClassifierReferences]
                 [--<*> 'classifiers.ClassContainsExtends]]]
               [q/p-restr 'classifiers.Class
                (fn [v]
                  (empty? (filter
                           #(g/has-type? %1 'modifiers.Abstract)
                           (g/adjs v :annotationsAndModifiers))))]])
          ge (q/reachables
              start
              [q/p-seq
               [q/p-+
                [q/p-seq
                 [q/<-- 'types.ClassifierReferenceLinksToTarget]
                 [q/--<> 'types.NamespaceClassifierReferenceContainsClassifierReferences]
                 [q/--<> 'classifiers.ClassContainsExtends]]]
               [q/p-restr 'classifiers.Class
                (fn [v]
                  (empty? (filter
                           #(g/has-type? %1 'modifiers.Abstract)
                           (g/adjs v :annotationsAndModifiers))))]])]
      (is (= 11 (count tg) (count ge)))
      (is (= tg ge)))))

(defn coupled-classes
  "Given a Class `c`, calculates all coupled classes."
  [c]
  (q/reachables c
    [q/p-seq [<>-- 'IsClassBlockOf] [<>-- 'IsMemberOf]
     [<-- ['IsBodyOfMethod 'IsFieldCreationOf]]
     [q/p-* [<-- 'IsStatementOf]]
     [q/p-alt
      ;; Classes whose methods are called by c
      [<-- 'IsDeclarationOfInvokedMethod]
      ;; Classes whose Fields are accessed by c
      [q/p-seq [<-- 'IsDeclarationOfAccessedField] [--> 'IsFieldCreationOf]]]
     [--<> 'IsMemberOf] [--<> 'IsClassBlockOf]
     [q/p-restr nil #(not (= c %1))]]))

(defn coupling-between-objects [c]
  (count (coupled-classes c)))
