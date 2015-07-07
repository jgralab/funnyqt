(ns funnyqt.query.tg-test
  (:use funnyqt.tg)
  (:use funnyqt.query.tg)
  (:require [funnyqt.generic :as g]
            [funnyqt.query   :as q]
            [funnyqt.utils   :as u]
            [funnyqt.tg-test :refer [rg jg]])
  (:use clojure.test))

(deftest test--->
  (mapv #(is (= %1 %2 %3))
        ;; TG -->
        (let [m (map id (--> (vertex rg 12)))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; Generic -->
        (let [m (map id (q/--> (vertex rg 12)))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; and that's the order (by ids)
        [7 6 3 4 1 2 10 11 5]))

(deftest test-<--
  (is (= 0
         (count (<-- (vertex rg 12)))
         (count (q/<-- (vertex rg 12))))))

(deftest test-<->
  (mapv #(is (= %1 %2 %3))
        (let [m (map id (<-> (vertex rg 12)))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        (let [m (map id (q/<-> (vertex rg 12)))]
          ;; There are 9 reachable unique vertices
          (is (= 9 (count m)))
          m)
        ;; and that's the order (by ids)
        [7 6 3 4 1 2 10 11 5]))

(deftest test-reachable-vertices
  (is (= 2
         (count (q/p-seq (vertex rg 1) --<> [q/p-* [--> 'localities.HasCapital]]))
         (count (q/p-seq (vertex rg 1) --<> [q/p-* [q/--> 'localities.HasCapital]]))))
  (is (= 4272
         (count (q/p-* (vertex jg 12) -->))
         (count (q/p-* (vertex jg 12) q/-->))))
  (is (= 4272
         (count (q/p-+ (vertex jg 12) -->))
         (count (q/p-+ (vertex jg 12) q/-->))))
  (is (= 6117
         (count (q/p-* (vertex jg 12) <->))
         (count (q/p-* (vertex jg 12) q/<->))))
  (is (= 6117
         (count (q/p-+ (vertex jg 12) <->))
         (count (q/p-+ (vertex jg 12) q/<->))))
  (is (= 19
         (count (q/p-+ (vertex jg 12) <>--))
         (count (q/p-+ (vertex jg 12) q/<>--))))
  (is (= 20
         (count (q/p-* (vertex jg 12) <>--))
         (count (q/p-* (vertex jg 12) q/<>--))))
  (is (= 22
         (count (q/p-seq (vertex jg 12) [q/p-* <>--] -->))
         (count (q/p-seq (vertex jg 12) [q/p-* q/<>--] q/-->))))
  (is (= 4272
         (count (q/p-seq (vertex jg 12) [q/p-* <>--] [q/p-+ -->]))
         (count (q/p-seq (vertex jg 12) [q/p-* q/<>--] [q/p-+ q/-->]))))
  (let [tg (q/p-+ (vertex jg 12) [q/p-seq <>-- -->])
        ge (q/p-+ (vertex jg 12) [q/p-seq q/<>-- q/-->])]
    (is (= 2337 (count tg) (count ge)))
    (is (= tg ge)))
  (is (= 6
         (count (q/p-seq (vertex jg 12)
                         [q/p-+ [q/p-seq <>-- -->]]
                         [q/p-restr  'annotations.Annotable]))
         (count (q/p-seq (vertex jg 12)
                         [q/p-+ [q/p-seq q/<>-- q/-->]]
                         [q/p-restr  'annotations.Annotable]))))
  (let [tg (q/p-seq (vertex jg 12)
                    [q/p-opt --<>]
                    [q/p-+ [q/p-seq <>-- -->]]
                    [q/p-opt <--])
        ge (q/p-seq (vertex jg 12)
                    [q/p-opt q/--<>]
                    [q/p-+ [q/p-seq q/<>-- q/-->]]
                    [q/p-opt q/<--])]
    (is (= 3280 (count tg) (count ge)))
    (is (= tg ge)))
  (is (= 6
         (count (q/p-alt (vertex jg 12) <>-- --<>))
         (count (q/p-alt (vertex jg 12) q/<>-- q/--<>)))))

(deftest test-p-exp
  (is (= (q/p-seq (vertex jg 12) --> --> -->)
         (q/p-seq (vertex jg 12) q/--> q/--> q/-->)
	 (q/p-exp (vertex jg 12) 3 -->)
         (q/p-exp (vertex jg 12) 3 q/-->)))
  (is (= (--> (vertex jg 12))
         (q/--> (vertex jg 12))
	 (q/p-exp (vertex jg 12) 1 -->)
         (q/p-exp (vertex jg 12) 1 q/-->)))
  (is (= (u/oset (vertex jg 12))
	 (q/p-exp (vertex jg 12) 0 -->)))
  (is (= (q/p-seq (vertex jg 12)
                  --> --> -->
                  [q/p-opt -->] [q/p-opt -->] [q/p-opt -->])
         (q/p-seq (vertex jg 12)
                  q/--> q/--> q/-->
                  [q/p-opt q/-->] [q/p-opt q/-->] [q/p-opt q/-->])
         (q/p-exp (vertex jg 12) 3 6 -->)))
  (is (= (q/p-seq (vertex jg 12) [q/p-opt -->] [q/p-opt -->] [q/p-opt -->])
         (q/p-exp (vertex jg 12) 0 3 -->))))

(deftest test-p-+*
  (is (= (q/p-+ (vertex jg 1) <->)
         (q/p-seq (vertex jg 1) <-> [q/p-* <->])))
  (is (contains? (q/p-* (vertex jg 1) <>--)
                 (vertex jg 1)))
  (is (not (contains? (q/p-+ (vertex jg 1) <>--)
                      (vertex jg 1)))))

(deftest test-p-+*2
  (doseq [p [[q/p-seq <-> <->]
             <>--
             <>--
             <->--
             [q/p-alt [q/p-seq --> -->]
                    [q/p-seq <-- <--]]]]
    (doseq [vid [1 20 117 3038]]
      (is (= (q/p-+ (vertex jg vid) p)
             (q/p-seq (vertex jg vid) p [q/p-* p])))
      (is (= (q/p-* (vertex jg vid) p)
             (q/p-opt (vertex jg vid) [q/p-+ p]))))))

(deftest test-derived-from-state
  (let [start (q/the (filter #(= (value %1 :name) "State")
                             (vseq jg 'classifiers.Class)))]
    (let [tg (q/p-seq start
                      [q/p-+
                       [q/p-seq
                        [<-- 'types.ClassifierReferenceLinksToTarget]
                        [--<> 'types.NamespaceClassifierReferenceContainsClassifierReferences]
                        [--<> 'classifiers.ClassContainsExtends]]]
                      [q/p-restr 'classifiers.Class
                       (fn [v]
                         (empty? (filter
                                  #(g/has-type? %1 'modifiers.Abstract)
                                  (g/adjs v :annotationsAndModifiers))))])
          ge (q/p-seq start
                      [q/p-+
                       [q/p-seq
                        [q/<-- 'types.ClassifierReferenceLinksToTarget]
                        [q/--<> 'types.NamespaceClassifierReferenceContainsClassifierReferences]
                        [q/--<> 'classifiers.ClassContainsExtends]]]
                      [q/p-restr 'classifiers.Class
                       (fn [v]
                         (empty? (filter
                                  #(g/has-type? %1 'modifiers.Abstract)
                                  (g/adjs v :annotationsAndModifiers))))])]
      (is (= 11 (count tg) (count ge)))
      (is (= tg ge)))))

