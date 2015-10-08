(ns funnyqt.model2model-test
  (:require [clojure.test :refer :all]
            [funnyqt
             [emf :as emf]
             [generic :as g]
             [model2model :refer :all]
             [query :as q]
             [tg :refer :all]]))

(emf/load-ecore-resource "test/input/Families.ecore")

(defn family
  "Returns the main family of member m."
  [m]
  (or (g/adj m :familyFather) (g/adj m :familyMother)
      (g/adj m :familySon)    (g/adj m :familyDaughter)))

(defn male?
  "Returns true, iff member m is male."
  [m]
  (or (g/adj m :familyFather) (g/adj m :familySon)))

(defn parents-of
  "Returns the set of parent members of m."
  [m]
  (q/p-seq m
           [q/p-alt :familySon :familyDaughter]
           [q/p-alt :father :mother]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (g/adj* m :familyFather :mother))

(generate-schema-functions "test/input/genealogy-schema.tg"
                           test.functional.genealogy.tg
                           gen-tg)

(deftransformation families2genealogy
  "Transforms a family model to a genealogy model."
  [^:in in ^:out out]
  (first-name
   [m]
   (emf/eget m :firstName))
  (make-address
   :from [street town]
   :to [adr 'Address {:street street
                      :town town}]
   (is (= adr (resolve-in :make-address [street town]))))
  (^:top member2person
         :from [m]
         :disjuncts [member2male member2female :as p]
         (gen-tg/set-ageGroup! p (enum-constant p (if (< (emf/eget m :age) 18)
                                                    'AgeGroup.CHILD
                                                    'AgeGroup.ADULT)))
         (gen-tg/->set-address! p (make-address (emf/eget (family m) :street)
                                                (emf/eget (family m) :town)))
         (when-let [ps (seq (parents-of m))]
           (gen-tg/->set-parents! p (map member2person ps)))
         (is (= p (or (resolve-in :member2male m)
                      (resolve-in :member2female m)))))
  (member2male
   :from [m 'Member]
   ;; Just for testing purposes, use the full name as identity rather than the
   ;; Member element itself.
   :id   [id (str (emf/eget m :firstName) " "
                  (emf/eget (family m) :lastName))]
   ;;:dup-id-eval true
   :when (male? m)
   ;; This nonsense is just here to test that there may be multiple
   ;; :let/:when/:when-let clauses.
   :let [x m]
   :when (male? x)
   :let [y x]
   :when [y]
   :when-let [z y
              foo z]
   :to   [p 'Male :in out {:wife (when-let [w (wife z)] (member2female w))
                           :fullName id}]
   (is (= p (resolve-in :member2male m))))
  (member2female
   :from [m 'Member]
   :when (not (male? m))
   :to   [p 'Female {:fullName (str (emf/eget m :firstName) " "
                                    (emf/eget (family m) :lastName))}]
   (is (= p (resolve-in :member2female m)))))

(deftest test-families2genealogy
  (let [in (emf/load-resource "test/input/example.families")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (new-graph out-schema)
        _ (print "families2genealogy (EMF -> TG):               ")
        trace (time (families2genealogy in ng))]
    #_(viz/print-model ng :gtk)
    (is (== 13 (vcount ng 'Person)))
    (is (==  7 (vcount ng 'Female)))
    (is (==  6 (vcount ng 'Male)))
    (is (==  3 (ecount ng 'HasSpouse)))
    (is (== 18 (ecount ng 'HasChild)))
    (is (== 3  (count (vseq ng 'Address))))
    #_(clojure.pprint/pprint trace)))

(deftransformation families2genealogy-ext
  "Like families2genealogy, but prepends Mr./Mrs. to first names."
  [^:in in ^:out out]
  :extends families2genealogy
  (first-name [m]
   (str (if (male? m) "Mr. " "Mrs. ")
        (emf/eget m :firstName))))

(deftest test-families2genealogy-ext
  (let [in (emf/load-resource "test/input/example.families")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (new-graph out-schema)
        _ (print "families2genealogy-ext (EMF -> TG):           ")
        trace (time (families2genealogy-ext in ng))]
    #_(viz/print-model ng :gtk)
    (is (== 13 (vcount ng 'Person)))
    (is (==  7 (vcount ng 'Female)))
    (is (==  6 (vcount ng 'Male)))
    (is (==  3 (ecount ng 'HasSpouse)))
    (is (== 18 (ecount ng 'HasChild)))
    (is (== 3  (count (vseq ng 'Address))))
    #_(clojure.pprint/pprint trace)))

(deftransformation families2genealogy-explicit-main
  "Transforms a family model to a genealogy model."
  [^:in in ^:out out]
  (first-name [m]
   (emf/eget m :firstName))
  (make-address
   :from [street town]
   :to [adr 'Address {:street street, :town town}])
  (member2person
   :from [m]
   :disjuncts [member2male member2female :as p]
   (set-value! p :fullName
               (str (first-name m) " "
                    (emf/eget (family m) :lastName)))
   (set-value! p :ageGroup (enum-constant p (if (< (emf/eget m :age) 18)
                                              'AgeGroup.CHILD
                                              'AgeGroup.ADULT)))
   (g/set-adj! p :address (make-address (emf/eget (family m) :street)
                                        (emf/eget (family m) :town)))
   (when-let [ps (seq (parents-of m))]
     (g/set-adjs! p :parents (map member2person ps))))
  (member2male
   :from [m 'Member]
   :when (male? m)
   :let  [w (wife m)]
   :to   [p 'Male :in out]
   (when w
     (g/set-adj! p :wife (member2female w))))
  (member2female
   :from [m 'Member]
   :when (not (male? m))
   :to   [p 'Female])
  ;; Try a main function instead of automatic application of ^:top rules.
  (main []
   (doseq [m (emf/eallcontents in 'Member)]
     (member2person m))))

(deftest test-families2genealogy-explicit-main
  (let [in (emf/load-resource "test/input/example.families")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (new-graph out-schema)
        _ (print "families2genealogy-explicit-main (EMF -> TG): ")
        trace (time (families2genealogy-explicit-main in ng))]
    #_(viz/print-model ng :gtk)
    (is (== 13 (vcount ng 'Person)))
    (is (==  7 (vcount ng 'Female)))
    (is (==  6 (vcount ng 'Male)))
    (is (==  3 (ecount ng 'HasSpouse)))
    (is (== 18 (ecount ng 'HasChild)))
    (is (== 3  (count (vseq ng 'Address))))
    #_(clojure.pprint/pprint trace)))

(deftransformation families2genealogy-generic
  "Transforms a family model to a genealogy model."
  [^:in in ^:out out]
  (^:top member2person
         :from [m]
         :disjuncts [member2male member2female :as p]
         (g/set-aval! p :fullName
                      (str (g/aval m :firstName) " "
                           (g/aval (family m) :lastName)))
         (g/set-adj! p :address (family2address (family m)))
         (g/set-adjs! p :parents (map member2person (parents-of m))))
  (member2male
   :from [m 'Member]
   :when (male? m)
   :let  [w (wife m)]
   :to   [p 'Male {:wife (member2female (wife m))}])
  (member2female
   :from [m 'Member]
   :when (not (male? m))
   :to   [p 'Female])
  (family2address
   :from [f 'Family]
   :id   [id [(g/aval f :street) (g/aval f :town)]]
   :to   [adr 'Address {:street (first id), :town (second id)}]))


(deftest test-families2genealogy-generic
  (let [in (emf/load-resource "test/input/example.families")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (new-graph out-schema)
        _ (print "families2genealogy-generic (EMF -> TG): ")
        trace (time (families2genealogy-generic in ng))]
    #_(viz/print-model ng :gtk)
    (is (== 13 (vcount ng 'Person)))
    (is (==  7 (vcount ng 'Female)))
    (is (==  6 (vcount ng 'Male)))
    (is (==  3 (ecount ng 'HasSpouse)))
    (is (== 18 (ecount ng 'HasChild)))
    (is (== 13 (ecount ng 'LivesAt)))
    (is (== 3  (vcount ng 'Address)))
    #_(clojure.pprint/pprint trace)))


(deftest test-valid-to-bindings
  (is (var? (eval '(funnyqt.model2model/deftransformation
                     complex-to-bindings [^:in in ^:out out1 ^:out out2]
                     (^:top rule1
                            :from [x 'X]
                            :to   [a 'A
                                   b 'B :in out2
                                   c 'C {:name "Test"}
                                   d 'D :in out1 {:name "Test2"}
                                   e 'E
                                   f 'F {:name "Test3"}
                                   g 'G {:name "Test4"} :in out2
                                   h 'H
                                   i 'I
                                   j 'J :in out2 {:foo "Bar"}
                                   k (symbol "SomeType")
                                   l (symbol "SomeType") {:a 1 :b 2} :in out1
                                   m 'A :in out2
                                   n 'B {:a 1 :b 2}
                                   o (symbol "C")
                                   p (symbol "C")
                                   q (symbol "C") {:a 1 :b 2}]))))))


(deftransformation copy-transformation-1 [^:in old ^:out new]
  (^:top element2element
   :from [oel]
   :let  [cls (g/mm-class oel)]
   :to   [nel cls]
   (doseq [attr (g/mm-all-attributes cls)]
     (g/set-aval! nel attr (g/aval oel attr)))
   (doseq [ref  (g/mm-all-references cls)]
     (if (g/mm-multi-valued-property? cls ref)
       (when-let [oadjs (seq (g/adjs oel ref))]
         (g/set-adjs! nel ref (map element2element oadjs)))
       (when-let [oadj (g/adj oel ref)]
         (g/set-adj! nel ref (element2element oadj)))))))

(deftest test-copy-transformation-1
  (let [in  (emf/load-resource "test/input/example.families")
        out (emf/new-resource)]
    (copy-transformation-1 in out)
    (is (g/equal-models? in out))))
