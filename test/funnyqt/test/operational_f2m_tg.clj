(ns funnyqt.test.operational-f2m-tg
  (:use funnyqt.operational)
  (:use funnyqt.generic)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query))

;; Define some helpers and mappings outside of the transformation to see if
;; that internal/external stuff works.

(defhelper family
  "Returns the main family of member m."
  [m]
  (or (adj m :familyFather) (adj m :familyMother)
      (adj m :familySon)    (adj m :familyDaughter)))

(defhelper male?
  "Returns true, iff member m is male."
  [m]
  (or (adj m :familyFather)
      (adj m :familySon)))

(defhelper parents-of
  "Returns the set of parent members of m."
  [m]
  (reachables
   m [p-seq
      [p-alt :familySon :familyDaughter]
      [p-alt :father :mother]]))

(defmapping family2address [f out]
  (doto (create-vertex! out 'Address)
    (set-value! :street (value f :street))
    (set-value! :town   (value f :town))))

;; Here goes the transformation using those external helpers/mappings...
(deftransformation families2genealogy-tg [in out]
  (defhelper set-person-props
    "Sets the person p's attributes according to its source member m."
    [p m]
    (set-value! p :fullName
                (str (value m :firstName) " "
                     (value (family m) :lastName)))
    (set-value! p :ageGroup
                (enum-constant p (if (>= (value m :age) 18)
                                   'AgeGroup.ADULT
                                   'AgeGroup.CHILD)))
    (add-adj! p :address
              (resolve-in family2address (family m)))
    (deferred
      (set-adjs! p :parents
                 (resolve-all-in member2person (parents-of m)))))

  (defhelper wife
    "Returns the wife member of member m."
    [m]
    (when-let [w (seq (reachables
                       m [p-seq :familyFather :mother]))]
      (the w)))

  (defmapping member2male [m]
    (let [male (create-vertex! out 'Male)]
      (set-person-props male m)
      (deferred
        (when-let [w (resolve-in member2person (wife m))]
          (add-adj! male :wife w)))
      male))

  (defmapping member2female [m]
    (doto (create-vertex! out 'Female)
      (set-person-props m)))

  (defmapping member2person [m]
    (if (male? m)
      (member2male m)
      (member2female m)))

  (defmapping familymodel2genealogy []
    (doseq [f (vseq in 'Family)]
      (family2address f out))
    (doseq [p (vseq in 'Member)]
      (member2person p)))

  ;; The main form
  (do
    (familymodel2genealogy)
    out))

;; Run it!

(deftest test-transformation
  (let [in (load-graph "test/input/familygraph.tg")
        out-schema (load-schema "test/input/genealogy-schema.tg")
        ng (create-graph out-schema)
        gen (time (families2genealogy-tg in ng))]
    (save-graph gen "test/output/families2genealogy-tg.tg")
    (print-graph gen "test/output/families2genealogy-tg.pdf" false)
    (is gen)
    (is (== 13 (count (vseq gen 'Person))))
    (is (== 7  (count (vseq gen 'Female))))
    (is (== 6  (count (vseq gen 'Male))))
    (is (== 3  (count (vseq gen 'Address))))))
