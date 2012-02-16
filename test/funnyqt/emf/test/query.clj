(ns funnyqt.emf.test.query
  (:refer-clojure :exclude [parents])
  (:use funnyqt.utils)
  (:use funnyqt.generic)
  (:use ordered.set)
  (:use ordered.map)
  (:use funnyqt.emf.core)
  (:use funnyqt.emf.query)
  (:use [funnyqt.emf.test.core :only [family-model]])
  (:use clojure.test))

(deftest test-basic
  (let [fm family-model]
    (are [x y n] (let [ox (into-oset x)]
                   (and (= ox y)
                        (== n (count ox))))
         (erefs fm) (ereachables fm -->>) 16
         (ecrossrefs fm) (ereachables fm -->) 0
         (erefs fm :members) (ereachables fm :members) 13
         (erefs fm :families) (ereachables fm :families) 3
         (erefs fm [:members :families]) (ereachables fm [ep-alt :members :families]) 16)))

(defn- parents
  [m]
  (ereachables m [ep-seq
                  [ep-alt :familySon :familyDaughter]
                  [ep-alt :father :mother]]))

(defn- aunts
  [m]
  (let [ps (parents m)]
    (ereachables ps [ep-seq
                     [ep-alt :familySon :familyDaughter]
                     :daughters
                     [ep-restr nil #(not (member? % ps))]])))
