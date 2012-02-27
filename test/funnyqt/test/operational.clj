(ns funnyqt.test.operational
  (:use funnyqt.operational)
  (:use funnyqt.generic)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:require [funnyqt.emf.core :as emf])
  (:require [funnyqt.emf.query :as emfq])
  (:import [funnyqt.emf.core EMFModel]))

(deftransformation Families2GenealogyEMF [in out]
  (defn male?
    "A member m is male if he's a father or a son of some family."
    [m]
    (or (emf/eget m :familyFather)
        (emf/eget m :familySon)))

  (defn family
    "Gets the main family of m."
    [m]
    (or (emf/eget m :familyFather) (emf/eget m :familyMother)
        (emf/eget m :familySon)    (emf/eget m :familyDaughter)))

  (defn parents-of
    "Returns a set of m's parents."
    [m]
    (emfq/reachables
     m [emfq/p-seq
        [emfq/p-alt :familySon :familyDaughter]
        [emfq/p-alt :father :mother]]))

  (declare member2person)
  (defn set-person-props [p m]
    (emf/eset! p :fullName
               (str (emf/eget m :firstName) " "
                    (emf/eget (family m) :lastName)))
    (emf/eset! p :ageGroup
               (emf/eenum-literal (if (>= (emf/eget m :age) 18)
                                    'AgeGroup.ADULT
                                    'AgeGroup.CHILD)))
    (deferred
      (emf/eset! p :parents
                 (resolve-all-in member2person (parents-of m)))))

  (defn wife
    "Returns the wife member of m."
    [m]
    (when-let [w (seq (emfq/reachables
                       m [emfq/p-seq :familyFather :mother]))]
      (the w)))

  (defmapping member2male [m]
    (let [male (emf/ecreate 'Male)]
      (set-person-props male m)
      (deferred
        (emf/eset! male :wife
                   (resolve-in member2person (wife m))))
      male))

  (defmapping member2female [m]
    (doto (emf/ecreate 'Female)
      (set-person-props m)))

  (defmapping member2person [m]
    (if (male? m)
      (member2male m)
      (member2female m)))

  (defmapping familymodel2genealogy [fm]
    (doto (emf/ecreate out 'Genealogy)
      (emf/eset! :persons (map member2person
                               (emf/eallobjects in 'Member)))))

  (familymodel2genealogy in))

(emf/load-metamodel "test/Families.ecore")
(emf/load-metamodel "test/Genealogy.ecore")

;; Run it!

(deftest test-transformation
  (let [gen (Families2GenealogyEMF (emf/load-model "test/example.families")
                                   (emf/new-model))]
    (emf/pdf-print-model gen "genealogy.pdf")
    (is gen)))

