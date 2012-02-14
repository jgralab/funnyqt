(ns funnyqt.emf.test.core
  (:use [funnyqt.emf.core])
  (:use [funnyqt.generic])
  (:use [clojure.test])
  (:import
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.common.util URI]
   [org.eclipse.emf.ecore EPackage EObject EModelElement]))

(deftest test-load-metamodel
  (let [mm (load-metamodel "test/Families.ecore")]
    (is (== 1 (count mm)))
    (is (instance? EPackage (first mm)))
    (is (== 1 (count (epackages (first mm)))))
    ;; We know 3: the EMF-XML stuff pkg, the ECore package, and the families
    ;; package.
    (is (== 3 (count (epackages))))
    ;; Restricting to our custom one by its nsURI...
    (with-ns-uris ["http://families/1.0"]
      (is (== 1 (count (epackages)))))))

(def family-mm (load-metamodel "test/Families.ecore"))
(def family-model (load-model "test/example.families"))

(deftest test-eclassifiers
  (with-ns-uris ["http://families/1.0"]
    (is (== 3 (count (eclasses))))
    (is (== 3 (count (eclassifiers))))))

(deftest test-eclassifier
  (let [fmodel (eclassifier 'FamilyModel)
        family (eclassifier 'Family)
        person (eclassifier 'Member)]
    (is fmodel)
    (is family)
    (is person)))

(deftest test-econtents-eallcontents
  (let [all   (eallcontents family-model)
        mems  (eallcontents family-model 'Member)
        fams  (eallcontents family-model 'Family)
        fmods (eallcontents family-model 'FamilyModel)]
    (is (== 17 (count all)))
    (is (== 1  (count fmods)))
    (is (== 3  (count fams)))
    (is (== 13 (count mems)))
    ;; The FamilyModel is the container of all Members and Families.
    (doseq [x (concat mems fams)]
      (is (the fmods) (econtainer x)))
    ;; In this concrete case, econtents and eallcontents equal
    (is (= (eallcontents family-model) (econtents family-model)))
    (is (= (eallcontents family-model 'FamilyModel)
           (econtents family-model    'FamilyModel)))
    (is (= (eallcontents family-model 'Member)
           (econtents family-model    'Member)))
    (is (= (eallcontents family-model 'Family)
           (econtents family-model    'Family)))))

(deftest test-ecrossrefs
  (let [fsmith (first (econtents family-model 'Family))]
    (is (= (ecrossrefs fsmith)
           (ecrossrefs fsmith [:father :mother :sons :daughters])))
    (is (== 1
            (count (ecrossrefs fsmith :father))
            (count (ecrossrefs fsmith :mother))
            (count (ecrossrefs fsmith :daughters))))
    (is (== 3 (count (ecrossrefs fsmith :sons))))))

(deftest test-eget
  (let [fm (the family-model)
        fsmith (first (econtents fm 'Family))]
    (is (= (next (econtents fm))
           (concat (eget fm :families)
                   (eget fm :members))))
    (is (= (econtents fm 'Family)
           (eget fm :families)))
    (is (= (econtents fm 'Member)
           (eget fm :members)))))
