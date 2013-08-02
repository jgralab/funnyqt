(ns funnyqt.extensional.emf-test
  (:use funnyqt.emf)
  (:use funnyqt.query)
  (:use funnyqt.query.emf)
  (:use funnyqt.extensional)
  (:use funnyqt.extensional.emf)
  (:use funnyqt.protocols)
  (:use clojure.test))

;; The Family2Genealogy transformation from EMF to EMF

(defn family
  "Returns the main family of member m."
  [m]
  (or (adj m :familyFather) (adj m :familyMother)
      (adj m :familySon)    (adj m :familyDaughter)))

(defn male?
  "Returns true, iff member m is male."
  [m]
  (or (adj m :familyFather)
      (adj m :familySon)))

(defn parents-of
  "Returns the set of parent members of m."
  [m]
  (reachables
   m [p-seq
      [p-alt :familySon :familyDaughter]
      [p-alt :father :mother]]))

(defn wife
  "Returns the wife member of member m."
  [m]
  (adj m :familyFather :mother))

(deftransformation families2genealogy [m tm]
  (create-eobjects! tm 'Male
                    (fn []
                      (filter male?
                              (eallobjects m 'Member))))
  (create-eobjects! tm 'Female
                    (fn []
                      (filter (complement male?)
                              (eallobjects m 'Member))))
  (set-features! tm 'Person.fullName
                 (fn []
                   (for [mem (eallobjects m 'Member)]
                     [(resolve-eobject mem)
                      (str (eget mem :firstName) " "
                           (eget (family mem) :lastName))])))
  (set-features! tm 'Male.wife
                 (fn []
                   (for [mem (filter wife (eallobjects m 'Member))]
                     [(resolve-eobject mem) (resolve-target (wife mem))])))
  (add-features! tm 'Person.parents
                 (fn []
                   (for [child (eallobjects m 'Member)
                         :let [parents (parents-of child)]
                         :when parents]
                     [(resolve-eobject child) (resolve-all-targets parents)]))))

(deftest test-families2genealogy-extensional
  (let [tmm (load-metamodel "test/input/Genealogy.ecore")
        tm (new-model)
        _ (load-metamodel "test/input/Families.ecore")
        m (load-model "test/input/example.families")]
    #_(clojure.pprint/pprint (families2genealogy m tm))
    (families2genealogy m tm)
    (is (= 13 (count (eallobjects tm 'Person))))
    (is (=  7 (count (eallobjects tm 'Female))))
    (is (=  6 (count (eallobjects tm 'Male))))
    (is (= 18 (count (eallpairs tm :parents :children))))
    (is (=  3 (count (eallpairs tm :husband :wife))))
    #_(print-model tm ".gtk")))

