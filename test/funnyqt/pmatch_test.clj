(ns funnyqt.pmatch-test
  (:use funnyqt.pmatch)
  (:use funnyqt.generic)
  (:require [funnyqt.tg        :as tg]
            [funnyqt.emf       :as emf]
            [funnyqt.generic   :as g])
  (:use funnyqt.query)
  (:use clojure.test))

;;# TG

(def fg (tg/load-graph "test/input/familygraph.tg"))

(defpattern families-with-fathers-simple-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> -hf<HasFather>-> m<Member>])

(deftest test-families-with-fathers-simple-tg
  (is (= 3 (count (families-with-fathers-simple-tg fg)))))

(defpattern families-with-fathers-tg
  {:pattern-expansion-context :tg}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(defpattern families-with-fathers-forall-tg
  {:pattern-expansion-context :tg
   :forall true}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(defpattern families-with-fathers-forall-distinct-tg
  {:pattern-expansion-context :tg
   :forall true
   :distinct true}
  ([g]
     [f<Family> -hf<HasFather>-> m<Member>])
  ([g famconst]
     [f<Family> -hf<HasFather>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-tg
  (is (= 3 (count (families-with-fathers-tg fg))))
  (is (= 3 (count (families-with-fathers-forall-tg fg))))
  (is (= 3 (count (families-with-fathers-forall-distinct-tg fg))))
  (is (= 3 (count (families-with-fathers-tg fg (constantly true)))))
  (is (= 2 (count (families-with-fathers-tg fg #(= "Smith" (tg/value % :lastName)))))))

(defpattern same-family-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-tg
  {:pattern-expansion-context :tg}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-tg
  (is (= 62
         (* 2 (count (same-family-distinct-tg fg)))
         (count (same-family-tg fg)))))

(defpattern given-fam-with-all-members-tg
  {:pattern-expansion-context :tg}
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-tg
  (let [fsmith (tg/vertex fg 12)
        r (given-fam-with-all-members-tg fg fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (first %)) r))))

(defpattern long-anon-pattern-tg
  {:pattern-expansion-context :tg}
  [g fam]
  [fam --> <Member> <-- <> -<HasSon>-> <> <-- x<Family>
   :when (not= fam x)])

(deftest test-long-anon-pattern-tg
  (let [fsmith (tg/vertex fg 12)
        r (long-anon-pattern-tg fg fsmith)]
    (is (= 1 (count r)))
    (is (= (first r) [(tg/vertex fg 12) (tg/vertex fg 1)]))))

(deftest test-letpattern-tg
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -hf<HasFather>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -hf<HasFather>-> m<Member>])
                 ([g famconst]
                    [f<Family> -hf<HasFather>-> m<Member>
                     :when (famconst f)
                     :as [f hf m]]))]
    {:pattern-expansion-context :tg}
    (is (= 3 (count (families-with-fathers-simple fg))))
    (is (= 3 (count (families-with-fathers fg))))
    (is (= 3 (count (families-with-fathers fg (constantly true)))))
    (is (= 2 (count (families-with-fathers fg #(= "Smith" (tg/value % :lastName))))))))

(deftest test-pattern-tg
  (let [families-with-fathers-simple (pattern {:pattern-expansion-context :tg} [g]
                                              [f<Family> -hf<HasFather>-> m<Member>])
        families-with-fathers (pattern {:pattern-expansion-context :tg}
                                       ([g]
                                          [f<Family> -hf<HasFather>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -hf<HasFather>-> m<Member>
                                           :when (famconst f)
                                           :as [f hf m]]))]
    (is (= 3 (count (families-with-fathers-simple fg))))
    (is (= 3 (count (families-with-fathers fg))))
    (is (= 3 (count (families-with-fathers fg (constantly true)))))
    (is (= 2 (count (families-with-fathers fg #(= "Smith" (tg/value % :lastName))))))))

(deftest test-eager-pattern-tg
  (let [lazy-pattern1 (pattern {:pattern-expansion-context :tg} [g]
                              [f<Family> -hf<HasFather>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :tg, :eager true} [g]
                                [f<Family> -hf<HasFather>-> m<Member>])
        lazy-pattern2 (pattern {:pattern-expansion-context :tg} [g]
                               [m1<Member> <-- <> --> m2<Member>
                                :when (distinct? m1 m2)
                                :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:pattern-expansion-context :tg, :eager true} [g]
                                [m1<Member> <-- <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fg) (eager-pattern1 fg)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fg)) (set (eager-pattern2 fg))))))

;;# EMF

(emf/load-ecore-resource "test/input/Families.ecore")
(def fm (emf/load-resource "test/input/example.families"))

(defpattern families-with-fathers-simple-emf
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> -<father>-> m<Member>])

(deftest test-families-with-fathers-simple-emf
  (is (= 3 (count (families-with-fathers-simple-emf fm)))))

(defpattern families-with-fathers-generic
  {:pattern-expansion-context :emf}
  ([g]
     [f<Family> -<father>-> m<Member>])
  ([g famconst]
     [f<Family> -<father>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-generic
  (is (= 3 (count (families-with-fathers-generic fm))))
  (is (= 3 (count (families-with-fathers-generic fm (constantly true)))))
  (is (= 2 (count (families-with-fathers-generic fm #(= "Smith" (emf/eget % :lastName)))))))

(defpattern same-family-generic
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-generic
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-generic
  (is (= 62
         (* 2 (count (same-family-distinct-generic fm)))
         (count (same-family-generic fm)))))

(defpattern given-fam-with-all-members-generic
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-generic
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                        (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        r (given-fam-with-all-members-generic fm fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (first %)) r))))

(defpattern long-anon-pattern-generic
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> <Member> --> <Family> -<sons>-> <> --> x<Family>
   :when (not= fam x)
   :as [(emf/eget x :lastName) (emf/eget x :street)]])

(deftest test-long-anon-pattern
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        ofsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                          (= "Smith Avenue 4" (emf/eget f :street))))
                             (emf/eallcontents fm 'Family)))
        r (long-anon-pattern-generic fm fsmith)]
    (is (= 1 (count r)))
    (is (= [fsmith ofsmith]))))

(deftest test-letpattern-generic
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -<father>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -<father>-> m<Member>])
                 ([g famconst]
                    [f<Family> -<father>-> m<Member>
                     :when (famconst f)
                     :as [f m]]))]
    {:pattern-expansion-context :emf}
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-pattern-generic
  (let [families-with-fathers-simple (pattern {:pattern-expansion-context :emf}
                                              [g]
                                               [f<Family> -<father>-> m<Member>])
        families-with-fathers (pattern {:pattern-expansion-context :emf}
                                       ([g]
                                          [f<Family> -<father>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -<father>-> m<Member>
                                           :when (famconst f)
                                           :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-eager-pattern-generic
  (let [lazy-pattern1 (pattern {:pattern-expansion-context :emf} [g]
                              [f<Family> -<father>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :emf, :eager true} [g]
                                [f<Family> -<father>-> m<Member>])
        lazy-pattern2 (pattern {:pattern-expansion-context :emf} [g]
                               [m1<Member> --> <> --> m2<Member>
                                :when (distinct? m1 m2)
                                :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:pattern-expansion-context :emf, :eager true} [g]
                                [m1<Member> --> <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fm) (eager-pattern1 fm)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fm)) (set (eager-pattern2 fm))))))

;;# Generic

(defpattern families-with-fathers-simple-generic
  [g]
  [f<Family> -<father>-> m<Member>])

(deftest test-families-with-fathers-simple-generic
  (is (= 3 (count (families-with-fathers-simple-generic fm)))))

(defpattern families-with-fathers-generic
  ([g]
     [f<Family> -<father>-> m<Member>])
  ([g famconst]
     [f<Family> -<father>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-generic
  (is (= 3 (count (families-with-fathers-generic fm))))
  (is (= 3 (count (families-with-fathers-generic fm (constantly true)))))
  (is (= 2 (count (families-with-fathers-generic fm #(= "Smith" (emf/eget % :lastName)))))))

(defpattern same-family-generic
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)])

(defpattern same-family-distinct-generic
  [g]
  [f<Family> --> m1<Member>
   f --> m2<Member>
   :when (not= m1 m2)
   :as #{m1 m2}
   :distinct])

(deftest test-same-family-generic
  (is (= 62
         (* 2 (count (same-family-distinct-generic fm)))
         (count (same-family-generic fm)))))

(defpattern given-fam-with-all-members-generic
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-generic
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        r (given-fam-with-all-members-generic fm fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (first %)) r))))

(defpattern long-anon-pattern-generic
  [g fam]
  [fam --> <Member> --> <Family> -<sons>-> <> --> x<Family>
   :when (not= fam x)
   :as [(g/aval x :lastName) (g/aval x :street)]])

(deftest test-long-anon-pattern
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallcontents fm 'Family)))
        ofsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                          (= "Smith Avenue 4" (emf/eget f :street))))
                             (emf/eallcontents fm 'Family)))
        r (long-anon-pattern-generic fm fsmith)]
    (is (= 1 (count r)))
    (is (= [fsmith ofsmith]))))

(deftest test-letpattern-generic
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -<father>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -<father>-> m<Member>])
                 ([g famconst]
                    [f<Family> -<father>-> m<Member>
                     :when (famconst f)
                     :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (g/aval % :lastName))))))))

(deftest test-pattern-generic
  (let [families-with-fathers-simple (pattern [g] [f<Family> -<father>-> m<Member>])
        families-with-fathers (pattern ([g]
                                          [f<Family> -<father>-> m<Member>])
                                       ([g famconst]
                                          [f<Family> -<father>-> m<Member>
                                           :when (famconst f)
                                           :as [f m]]))]
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

(deftest test-eager-pattern-generic
  (let [lazy-pattern1 (pattern [g]
                               [f<Family> -<father>-> m<Member>])
        eager-pattern1 (pattern {:pattern-expansion-context :generic, :eager true} [g]
                                [f<Family> -<father>-> m<Member>])
        lazy-pattern2 (pattern [g] [m1<Member> --> <> --> m2<Member>
                                    :when (distinct? m1 m2)
                                    :as #{m1 m2} :distinct])
        eager-pattern2 (pattern {:eager true} [g]
                                [m1<Member> --> <> --> m2<Member>
                                 :when (distinct? m1 m2)
                                 :as #{m1 m2} :distinct])]
    (is (= (lazy-pattern1 fm) (eager-pattern1 fm)))
    ;; With :distinct patterns, the order is undefined in the eager case.
    (is (= (set (lazy-pattern2 fm)) (set (eager-pattern2 fm))))))
