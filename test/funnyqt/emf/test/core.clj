(ns funnyqt.emf.test.core
  (:use [funnyqt.emf.core])
  (:use [funnyqt.generic])
  (:use [ordered.set])
  (:use [ordered.map])
  (:use [clojure.test])
  (:import
   [org.eclipse.emf.ecore.xmi.impl XMIResourceImpl]
   [org.eclipse.emf.common.util URI EList]
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

(deftest test-inv-erefs
  (let [[f1 f2 f3] (econtents family-model 'Family)]
    (are [x y z cnt] (and (== cnt (count x) (count y) (count z))
                        (= (apply hash-set x)
                           (apply hash-set y)
                           (apply hash-set z)))
         ;; 7, cause the FamilyModel is also included
         (erefs f1)
         (inv-erefs f1)
         (inv-erefs f1 nil family-model)
         7
         ;; Here, it's not included (cross-refs only)
         (ecrossrefs f1)
         (inv-ecrossrefs f1)
         (inv-ecrossrefs f1 nil family-model)
         6
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (erefs f1 :father)
         (inv-erefs f1 :familyFather)
         (inv-erefs f1 :familyFather family-model)
         1
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ecrossrefs f1 :father)
         (inv-ecrossrefs f1 :familyFather)
         (inv-ecrossrefs f1 :familyFather family-model)
         1
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (erefs f1 [:mother :father])
         (inv-erefs f1 [:familyMother :familyFather])
         (inv-erefs f1 [:familyMother :familyFather] family-model)
         2
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ecrossrefs f1 [:mother :father])
         (inv-ecrossrefs f1 [:familyMother :familyFather])
         (inv-ecrossrefs f1 [:familyMother :familyFather] family-model)
         2
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (erefs f2 :father)
         (inv-erefs f2 :familyFather)
         (inv-erefs f2 :familyFather family-model)
         1
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ecrossrefs f2 :father)
         (inv-ecrossrefs f2 :familyFather)
         (inv-ecrossrefs f2 :familyFather family-model)
         1
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (erefs f3 :sons)
         (inv-erefs f3 :familySon)
         (inv-erefs f3 :familySon family-model)
         0
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ecrossrefs f3 :sons)
         (inv-ecrossrefs f3 :familySon)
         (inv-ecrossrefs f3 :familySon family-model)
         0
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (erefs f3 [:daughters])
         (inv-erefs f3 :familyDaughter)
         (inv-erefs f3 :familyDaughter family-model)
         3
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (ecrossrefs f3 [:daughters])
         (inv-ecrossrefs f3 :familyDaughter)
         (inv-ecrossrefs f3 :familyDaughter family-model)
         3)))

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

(deftest test-erefs-and-ecrossrefs
  (let [fm (the family-model)
        fsmith (first (econtents fm 'Family))]
    (are [x] (= (eget fm x) (erefs fm x))
         :families
         :members)
    (are [x] (= (let [r (eget fsmith x)]
                  (if (coll? r) r [r]))
                (erefs fsmith x)
                (ecrossrefs fsmith x))
         :father
         :mother
         :sons
         :daughters)
    ;; Those are all crossrefs, so erefs and ecrossrefs should equal
    (are [x] (= (erefs fsmith x) (ecrossrefs fsmith x))
         :father
         :mother
         :sons
         :daughters
         [:father :mother]
         [:sons :daughters]
         [:father :sons]
         [:mother :daughters])))

(defn- make-test-familymodel
  "Creates a more or less random FamilyModel with `fnum' families and `mnum'
  members.  The references (father, mother, sons, daughters) are set randomly."
  [fnum mnum]
  (let [fm (ecreate 'FamilyModel)
        make-family (fn [i]
                      (doto (ecreate 'Family)
                        (eset! :lastName (str "Family" i))
                        (eset! :street   (str "Some Street " i))
                        (eset! :town     (str i " Sometown"))))
        make-member (fn [i]
                      (doto (ecreate 'Member)
                        (eset! :firstName (str "Member" i))
                        (eset! :age       (Integer/valueOf ^Long (mod i 80)))))
        random-free-member (fn [mems ref]
                             (loop [m (rand-nth mems)]
                               (if (eget m ref)
                                 (recur (rand-nth mems))
                                 m)))
        random-members (fn [mems]
                         (loop [r #{}, i (rand-int 7)]
                           (if (pos? i)
                             (recur (conj r (rand-nth mems)) (dec i))
                             r)))]
    (eset! fm :families
           (loop [fams [], i 1]
             (if (<= i fnum)
               (recur (conj fams (make-family i)) (inc i))
               fams)))
    (eset! fm :members
           (loop [mems [], i 1]
             (if (<= i mnum)
               (recur (conj mems (make-member i)) (inc i))
               mems)))
    (let [mems (vec (eget fm :members))]
      (loop [fams (eget fm :families), r []]
        (when (seq fams)
          (let [fam (first fams)]
            (eset! fam :father    (random-free-member mems :familyFather))
            (eset! fam :mother    (random-free-member mems :familyMother))
            (eset! fam :sons      (random-members mems))
            (eset! fam :daughters (random-members mems))
            (recur (rest fams) (conj r fam))))))
    fm))

(deftest test-ecreate
  (let [fm (make-test-familymodel 100 1000)]
    (are [c s] (== c
                   (count (econtents fm s))
                   (count (eallcontents fm s)))
         1101 nil
         1    'FamilyModel
         100  'Family
         1000 'Member
         1100 '[Family Member])
    ;; Every family has its father/mother refs set
    (is (forall? (fn [f]
                   (and (eget f :father)
                        (eget f :mother)))
                 (econtents fm 'Family)))))

(deftest test-eget-raw
  (let [i 1000
        fm (ecreate 'FamilyModel)
        ^EList ms (eget-raw fm :members)]
    (time (dotimes [_ i]
            (.add ms (ecreate 'Member))))
    (is (== i (count (econtents fm 'Member))))
    (time (eset! fm :members (loop [ims (eget fm :members), x i]
                               (if (pos? x)
                                 (recur (conj ims (ecreate 'Member)) (dec x))
                                 ims))))
    (is (== (* 2 i) (count (econtents fm 'Member))))))
