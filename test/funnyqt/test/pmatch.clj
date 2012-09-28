(ns funnyqt.test.pmatch
  (:use funnyqt.pmatch)
  (:use funnyqt.protocols)
  (:require [funnyqt.tg        :as tg]
            [funnyqt.query.tg  :as tgq]
            [funnyqt.emf       :as emf]
            [funnyqt.query.emf :as emfq])
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

(deftest test-families-with-fathers-tg
  (is (= 3 (count (families-with-fathers-tg fg))))
  (is (= 3 (count (families-with-fathers-tg fg (constantly true)))))
  (is (= 2 (count (families-with-fathers-tg fg #(= "Smith" (tg/value % :lastName)))))))

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
    (is (= [(tg/vertex fg 12) (tg/vertex fg 1)]))))

(deftest test-letpattern-tg
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -hf<HasFather>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -hf<HasFather>-> m<Member>])
                 ([g famconst]
                    [f<Family> -hf<HasFather>-> m<Member>
                     :when (famconst f)]
                    [f hf m]))]
    {:pattern-expansion-context :tg}
    (is (= 3 (count (families-with-fathers-simple fg))))
    (is (= 3 (count (families-with-fathers fg))))
    (is (= 3 (count (families-with-fathers fg (constantly true)))))
    (is (= 2 (count (families-with-fathers fg #(= "Smith" (tg/value % :lastName))))))))

;;# EMF

(emf/load-metamodel "test/input/Families.ecore")
(def fm (emf/load-model "test/input/example.families"))

(defpattern families-with-fathers-simple-emf
  {:pattern-expansion-context :emf}
  [g]
  [f<Family> -<father>-> m<Member>])

(deftest test-families-with-fathers-simple-emf
  (is (= 3 (count (families-with-fathers-simple-emf fm)))))

(defpattern families-with-fathers-emf
  {:pattern-expansion-context :emf}
  ([g]
     [f<Family> -<father>-> m<Member>])
  ([g famconst]
     [f<Family> -<father>-> m<Member>
      :when (famconst f)]))

(deftest test-families-with-fathers-emf
  (is (= 3 (count (families-with-fathers-emf fm))))
  (is (= 3 (count (families-with-fathers-emf fm (constantly true)))))
  (is (= 2 (count (families-with-fathers-emf fm #(= "Smith" (emf/eget % :lastName)))))))

(defpattern given-fam-with-all-members-emf
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> mem<Member>])

(deftest test-given-fam-with-all-members-emf
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                        (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallobjects fm 'Family)))
        r (given-fam-with-all-members-emf fm fsmith)]
    (is (= 4 (count r)))
    (is (forall? #(= fsmith (first %)) r))))

(defpattern long-anon-pattern-emf
  {:pattern-expansion-context :emf}
  [g fam]
  [fam --> <Member> --> <> -<sons>-> <> --> x<Family>
   :when (not= fam x)]
  [(emf/eget x :lastName) (emf/eget x :street)])

(deftest test-long-anon-pattern-emf
  (let [fsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                        (= "Smithway 17" (emf/eget f :street))))
                            (emf/eallobjects fm 'Family)))
        ofsmith (the (filter (fn [f] (and (= "Smith" (emf/eget f :lastName))
                                         (= "Smith Avenue 4" (emf/eget f :street))))
                             (emf/eallobjects fm 'Family)))
        r (long-anon-pattern-emf fm fsmith)]
    (is (= 1 (count r)))
    (is (= [fsmith ofsmith]))))

(deftest test-letpattern-emf
  (letpattern [(families-with-fathers-simple [g]
                 [f<Family> -<father>-> m<Member>])
               (families-with-fathers
                 ([g]
                    [f<Family> -<father>-> m<Member>])
                 ([g famconst]
                    [f<Family> -<father>-> m<Member>
                     :when (famconst f)]
                    [f m]))]
    {:pattern-expansion-context :emf}
    (is (= 3 (count (families-with-fathers-simple fm))))
    (is (= 3 (count (families-with-fathers fm))))
    (is (= 3 (count (families-with-fathers fm (constantly true)))))
    (is (= 2 (count (families-with-fathers fm #(= "Smith" (emf/eget % :lastName))))))))

