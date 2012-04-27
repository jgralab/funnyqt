(ns funnyqt.test.operational-f2m-emf
  (:use funnyqt.operational)
  (:use funnyqt.query)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:use funnyqt.emf.core)
  (:use funnyqt.emf.query)
  (:import [funnyqt.emf.core EMFModel]))

;; Just for the fun of it, declare some helpers and mapping outside of the
;; transformation, so that we see if the expansion context works.

(defhelper family
  "Returns the main family of member m."
  [m]
  (or (eget m :familyFather) (eget m :familyMother)
      (eget m :familySon)    (eget m :familyDaughter)))

(defmapping family2address [f out]
  (doto (ecreate! out 'Address)
    (eset! :street (eget f :street))
    (eset! :town (eget f :town))))

;; Here comes the transformation that uses those external helpers and mappings.
(deftransformation families2genealogy-emf [in out]
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

  (defhelper wife
    "Returns the wife member of member m."
    [m]
    (when-let [w (seq (reachables
                       m [p-seq :familyFather :mother]))]
      (the w)))

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
    (eset! p :address
           (resolve-in family2address (family m)))
    (deferred
      (eset! p :parents
             (resolve-all-in member2person (parents-of m)))))

  (defmapping member2male
    "Transforms a Member to a Male."
    [m]
    (let [male (ecreate! 'Male)]
      (set-person-props male m)
      (deferred
        (when-let [wife (resolve-in member2person (wife m))]
          (eset! male :wife wife)))
      male))

  (defmapping member2female
    "Transforms a Member to a Female."
    [m]
    (doto (ecreate! 'Female)
      (set-person-props m)))

  (defmapping member2person [m]
    (if (male? m)
      (member2male m)
      (member2female m)))

  (defmapping familymodel2genealogy []
    (doseq [f (eallobjects in 'Family)]
      (family2address f out))
    (doto (ecreate! out 'Genealogy)
      (eset! :persons (map member2person
                           (eallobjects in 'Member)))))

  ;; The main form
  (do
    (familymodel2genealogy)
    out))

(load-metamodel "test/input/Families.ecore")
(load-metamodel "test/input/Genealogy.ecore")

;; Run it!

(deftest test-transformation
  (let [gen (families2genealogy-emf (load-model "test/input/example.families")
                                   (new-model))]
    (print-model gen "test/output/families2genealogy-emf.pdf")
    (save-model gen "test/output/families2genealogy-emf.xmi")
    (is gen)
    (is (== 13 (count (eallobjects gen 'Person))))
    (is (== 7  (count (eallobjects gen 'Female))))
    (is (== 6  (count (eallobjects gen 'Male))))
    (is (== 3  (count (eallobjects gen 'Address))))))
