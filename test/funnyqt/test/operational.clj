(ns funnyqt.test.operational
  (:use funnyqt.operational)
  (:use funnyqt.generic)
  (:use funnyqt.utils)
  (:require [funnyqt.emf.core :as emf])
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

  (defn set-person-props [p m]
    (emf/eset! p :fullName (str (emf/eget m :firstName) " "
                                (emf/eget (family m) :lastName)))
    (emf/eset! p :ageGroup (emf/eenum-literal (if (>= (emf/eget m :age) 18)
                                                'AgeGroup.ADULT
                                                'AgeGroup.CHILD))))

  (defmapping member2male [m]
    (doto (emf/ecreate 'Male)
      (set-person-props m)))

  (defmapping member2female [m]
    (doto (emf/ecreate 'Female)
      (set-person-props m)))

  (defmapping member2person [m]
    (pr-identity (if (male? m)
                   (member2male m)
                   (member2female m))))

  (defmapping family-model2-genealogy [fm]
    (doto (emf/ecreate out 'Genealogy)
      (emf/eset! :persons (map member2person
                               (emf/eallobjects in 'Member)))))

  (do
    (family-model2-genealogy in)
    (emf/save-model out "genealogy.xmi")
    (emf/pdf-print-model out "genealogy.pdf")))

(emf/load-metamodel "test/Families.ecore")
(emf/load-metamodel "test/Genealogy.ecore")

;(Families2GenealogyEMF (emf/load-model "test/example.families")
;                       (EMFModel. (org.eclipse.emf.ecore.resource.impl.ResourceImpl.)))
