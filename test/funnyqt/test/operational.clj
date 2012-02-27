(ns funnyqt.test.operational
  (:use funnyqt.operational)
  (:use funnyqt.generic)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:use funnyqt.emf.core)
  (:use funnyqt.emf.query)
  (:import [funnyqt.emf.core EMFModel]))

(deftransformation Families2GenealogyEMF [in out]
  (defhelper family
    "Returns the main family of member m."
    [m]
    (or (eget m :familyFather) (eget m :familyMother)
        (eget m :familySon)    (eget m :familyDaughter)))

  (defhelper male?
    "Returns true, iff member m is male."
    [m]
    (or (eget m :familyFather)
        (eget m :familySon)))

  (defhelper parents-of
    "Returns the set of parent members of m."
    [m]
    (reachables
     m [p-seq
        [p-alt :familySon :familyDaughter]
        [p-alt :father :mother]]))

  (defhelper set-person-props
    "Sets the person p's attributes according to its source member m."
    [p m]
    (eset! p :fullName
           (str (eget m :firstName) " "
                (eget (family m) :lastName)))
    (eset! p :ageGroup
           (eenum-literal (if (>= (eget m :age) 18)
                            'AgeGroup.ADULT
                            'AgeGroup.CHILD)))
    (deferred
      (eset! p :parents
             (resolve-all-in member2person (parents-of m)))))

  (defhelper wife
    "Returns the wife member of member m."
    [m]
    (when-let [w (seq (reachables
                       m [p-seq :familyFather :mother]))]
      (the w)))

  (defmapping member2male [m]
    (let [male (ecreate 'Male)]
      (set-person-props male m)
      (deferred
        (eset! male :wife
               (resolve-in member2person (wife m))))
      male))

  (defmapping member2female [m]
    (doto (ecreate 'Female)
      (set-person-props m)))

  (defmapping member2person [m]
    (if (male? m)
      (member2male m)
      (member2female m)))

  (defmapping familymodel2genealogy [fm]
    (doto (ecreate out 'Genealogy)
      (eset! :persons (map member2person
                           (eallobjects in 'Member)))))

  (familymodel2genealogy in))

(load-metamodel "test/Families.ecore")
(load-metamodel "test/Genealogy.ecore")

;; Run it!

(deftest test-transformation
  (let [gen (Families2GenealogyEMF (load-model "test/example.families")
                                   (new-model))]
    (pdf-print-model gen "genealogy.pdf")
    (is gen)
    (is (== 13 (count (eallobjects gen 'Person))))))
