(ns funnyqt.test.operational-f2m-tg
  (:use funnyqt.operational)
  (:use funnyqt.generic)
  (:use funnyqt.utils)
  (:use clojure.test)
  (:use funnyqt.tg.core)
  (:use funnyqt.tg.query))

(deftransformation families2genealogy-tg [in out]
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

  (defhelper set-person-props
    "Sets the person p's attributes according to its source member m."
    [p m]
    (set-value! p :fullName
                (str (value m :firstName) " "
                     (value (family m) :lastName)))
    (set-value! p :ageGroup
                (enum p 'AgeGroup (if (>= (value m :age) 18)
                                    'ADULT
                                    'CHILD)))
    (add-adj! p :address
              (resolve-in family2address (family m)))
    (deferred
      (add-adjs! p :parents
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

  (defmapping family2address [f]
    (doto (create-vertex! out 'Address)
      (set-value! :street (value f :street))
      (set-value! :town   (value f :town))))

  (defmapping familymodel2genealogy []
    (doseq [f (vseq in 'Family)]
      (family2address f))
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
        gen (families2genealogy-tg in (create-graph out-schema))]
    (save-graph gen "test/output/families2genealogy-tg.tg")
    (print-graph gen "test/output/families2genealogy-tg.pdf" false)))
