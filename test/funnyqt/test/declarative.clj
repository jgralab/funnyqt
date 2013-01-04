(ns funnyqt.test.declarative
  (:require [funnyqt.emf :as emf])
  (:require [funnyqt.query.emf :as emfq])
  (:use funnyqt.tg)
  (:use funnyqt.query)
  (:use funnyqt.declarative)
  (:use clojure.test))

(defn family
  "Returns the main family of member m."
  [m]
  (or (emf/eget m :familyFather) (emf/eget m :familyMother)
      (emf/eget m :familySon)    (emf/eget m :familyDaughter)))

(defn male?
  "Returns true, iff member m is male."
  [m]
  (or (emf/eget m :familyFather)
      (emf/eget m :familySon)))

(defn parents-of
  "Returns the set of parent members of m."
  [m]
  (emfq/reachables
   m [p-seq
      [p-alt :familySon :familyDaughter]
      [p-alt :father :mother]]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (when-let [w (seq (emfq/reachables
                     m [p-seq :familyFather :mother]))]
    (the w)))


(deftransformation families2genealogy [[in :emf]
                                       [out :tg]]
  (member2person [m]
                 :generalizes [member2male member2female])
  (member2person-setter [m p]
                        (set-value! p :fullName
                                    (str (emf/eget m :firstName)
                                         " "
                                         (emf/eget (family m) :lastName)))
                        (when-let [ps (seq (parents-of m))]
                          (set-adjs! p :parents (map member2person ps))))
  (member2male [m]
               :from 'Member
               :when (male? m)
               :to   [p 'Male :model out]
               (member2person-setter m p)
               (when-let [w (wife m)]
                 (add-adj! p :wife (member2female w))))
  (member2female [m]
                 :from 'Member
                 :when (not (male? m))
                 :to   [p 'Female]
                 (member2person-setter m p)))

(deftest test-transformation
  (let [_ (emf/load-metamodel "test/input/Families.ecore")
        in (emf/load-model "test/input/example.families")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (create-graph out-schema)
        gen (time (families2genealogy in ng))]
    #_(show-graph gen)
    (is gen)
    (is (== 13 (vcount gen 'Person)))
    (is (==  7 (vcount gen 'Female)))
    (is (==  6 (vcount gen 'Male)))
    (is (==  3 (ecount gen 'HasSpouse)))
    (is (== 18 (ecount gen 'HasChild)))
    ;(is (== 3  (count (vseq gen 'Address))))
    ))

