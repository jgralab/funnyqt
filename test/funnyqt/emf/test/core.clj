(ns funnyqt.emf.test.core
  (:use [funnyqt.emf.core])
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
  (is (== 3 (with-ns-uris ["http://families/1.0"] (eclasses))))
  (is (== 3 (with-ns-uris ["http://families/1.0"] (eclassifiers)))))

(deftest test-eclassifier
  (let [fmodel (eclassifier 'FamilyModel)
        family (eclassifier 'Family)
        person (eclassifier 'Member)]
    (is fmodel)
    (is family)
    (is person)))
