(ns funnyqt.emf.test.core
  (:use [funnyqt.emf.core])
  (:use [funnyqt.generic])
  (:use [ordered.set])
  (:use [ordered.map])
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

(defn- make-uniqueelist
  []
  (let [ul (org.eclipse.emf.common.util.UniqueEList.)]
    (doseq [i [0 1 2 3 4 1 5 6 7 7 3 2 8 1 0 0 9 0]]
      (.add ul i))
    ul))

(defn- make-elist
  []
  (let [el (org.eclipse.emf.common.util.BasicEList.)]
      (doseq [item [0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]]
        (.add el item))
      el))

(defn- make-emap
  []
  (let [em (org.eclipse.emf.common.util.BasicEMap.)]
    (doseq [[k v] [[:a "a"] [:b "b"] [:c "c"] [:d "d"]]]
      (.put em k v))
    em))

(deftest test-emf2clj-conversion
  ;; UniqueEList -> OrderedSet
  (let [uel (make-uniqueelist)
        clj-uel (emf2clj uel)]
    (is (instance? ordered.set.OrderedSet clj-uel))
    (is (== (count uel) (count clj-uel)))
    (is (= (seq uel) (seq clj-uel))))
  ;; EList -> ISeq
  (let [el (make-elist)
        clj-el (emf2clj el)]
    (is (seq? clj-el))
    (is (== (count el) (count clj-el)))
    (is (= (seq el) clj-el)))
  ;; EMap -> IPersistentMap
  (let [^org.eclipse.emf.common.util.EMap em (make-emap)
        clj-em (emf2clj em)]
    (is (map? clj-em))
    (is (== (count em) (count clj-em)))
    (doseq [k (keys clj-em)]
      (is (.containsKey em k))
      (is (= (.get em k) (clj-em k))))))

(deftest test-emf2clj-and-back
  (let [m  (make-emap)
        l  (make-elist)
        ul (make-uniqueelist)]
    ;; Conversion back and fourth
    (are [x y] (= x (-> y emf2clj clj2emf))
         m  m
         l  l
         ul ul)
    ;; Conversion from clojure
    (are [x y] (= x (clj2emf y))
         m  (ordered-map :a "a", :b "b", :c "c", :d "d")
         l  [0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]
         ul (ordered-set 0 1 2 3 4 1 5 6 7 7 3 2 8 1 0 0 9 0))
    ;; Conversion to clojure
    (are [x y] (= x (emf2clj y))
         (ordered-map :a "a", :b "b", :c "c", :d "d")      m
         [0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]         l
         (ordered-set 0 1 2 3 4 1 5 6 7 7 3 2 8 1 0 0 9 0) ul)))

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