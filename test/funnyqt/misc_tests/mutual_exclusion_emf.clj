(ns ^{:pattern-expansion-context :emf}
  funnyqt.misc-tests.mutual-exclusion-emf
  (:require [clojure.test :refer :all]
            [funnyqt
             [emf :refer :all]
             [generic :refer :all]
             [in-place :refer :all]
             [query :as q]
             [utils :refer :all]]))

(load-ecore-resource "test/input/MutualExclusion.ecore")

;;* Rules

;;** Short Transformation Sequence

(def counter (atom 1))

(defrule new-rule
  "Matches 2 connected processes and injects a new process in between."
  [model] [p1<Process> -<:next>-> p2
           :isomorphic]
  (let [p (ecreate! model 'Process)]
    (eset! p :name (str "np" @counter))
    (swap! counter inc)
    (eset! p1 :next p)
    (eset! p :next p2)))

(defrule kill-rule
  "Matches a sequence of 3 connected processes and deletes the middle one."
  [model] [p1<Process> -<:next>-> p -<:next>-> p2
           :isomorphic]
  (delete! p)
  (eset! p1 :next p2))

(defrule mount-rule
  "Matches a process and creates and assigns a resource to it."
  [model] [p<Process>]
  (let [r (ecreate! model 'Resource)]
    (eset! r :name (str "nr" @counter))
    (swap! counter inc)
    (eset! r :taker p)))

(defrule unmount-rule
  "Matches a resource assigned to a process and deletes it."
  [model] [r<Resource> -<:taker>-> p]
  (delete! r))

(defrule pass-rule
  "Passes the token to the next process if the current doesn't request it."
  [model] [r<Resource> -<:taker>-> p1 -!<:requested>-> r
           p1 -<:next>-> p2]
  (eset! r :taker p2))

(defrule request-rule
  "Matches a process that doesn't request any resource and a resource not held
  by that process and makes the process request that resource."
  [model] [r<Resource> -!<:holder>-> p<Process> -!<:requested>-> <>]
  (eadd! p :requested r))

(defrule take-rule
  "Matches a process that requests a resource that in turn tokens the process
  and makes the process hold that resource."
  ([model] [p<Process> -<:requested>-> r -<:taker>-> p]
   (take-rule model r p))
  ([model r p]
   (eunset! r :taker)
   (eremove! r :requester p)
   (eset! r :holder p)
   [r p]))

(defrule release-rule
  "Matches a resource held by a process and not requesting more resources, and
  releases that resource."
  ([model] [r<Resource> -<:holder>-> p -!<:requested>-> <>]
   (release-rule model r p))
  ([model r p]
   (eunset! r :holder)
   (eset! r :releaser p)
   [r p]))

(defrule give-rule
  "Matches a process releasing a resource, and gives the token to that resource
  to the next process."
  ([model] [r<Resource> -<:releaser>-> p1 -<:next>-> p2]
   (give-rule model r p1))
  ([model r p1]
   (let [p2 (eget p1 :next)]
     (eunset! r :releaser)
     (eset! r :taker p2)
     [r p2])))

(defrule blocked-rule
  "Matches a process requesting a resource held by some other process, and
  creates a blocked edge."
  [model] [p1<Process> -<:requested>-> r -<:holder>-> p2
           :isomorphic]
  (eadd! r :blocked p1))

(defrule waiting-rule
  "Moves the blocked state."
  ([model] [p2<Process> -<:requested>-> r1 -<:holder>-> p1 -<:blocked_by>-> r2
            :isomorphic]
   (waiting-rule model r1 r2 p1 p2))
  ([model r1] [r1 -<:requester>-> p2
               r1 -<:holder>-> p1 -<:blocked_by>-> r2
               :isomorphic]
   (waiting-rule model r1 r2 p1 p2))
  ([model r1 r2 p1 p2]
   (eremove! r2 :blocked p1)
   (eadd! r2 :blocked p2)
   [model r1]))

(defrule ignore-rule
  "Removes the blocked state if nothing is held anymore."
  [model] [r<Resource> -<:blocked>-> p -!<:held>-> <>]
  (eremove! r :blocked p))

(defrule unlock-rule
  "Matches a process holding and blocking a resource and releases it."
  [model] [r<Resource> -<:holder>-> p -<:blocked_by>-> r]
  (eunset! r :holder)
  (eremove! r :blocked p)
  (eset! r :releaser p))

(defn apply-mutual-exclusion-sts
  "Does the STS on `m`."
  [m n param-pass]
  ;; n-2 times new-rule ==> n processes in a ring
  (dotimes [_ (- n 2)]
    (new-rule m))
  ;; mount a resource and give token to one process
  (mount-rule m)
  ;; Let all processe issue a request to the single resource
  (dotimes [_ n]
    (request-rule m))
  ;; Handle the requests...
  (if param-pass
    ((iterated-rule #(apply give-rule m (apply release-rule m (take-rule m)))))
    ((iterated-rule #(do
                       (take-rule m)
                       (release-rule m)
                       (give-rule m))))))

(defn g-sts
  "Returns an initial graph for the STS.
  Two Processes connected in a ring by two Next edges."
  []
  (let [m (new-resource)
        p1 (ecreate! m 'Process)
        p2 (ecreate! m 'Process)]
    (eset! p1 :name "p1")
    (eset! p2 :name "p2")
    (eset! p1 :next p2)
    (eset! p2 :next p1)
    m))


;;** Long Transformation Sequence

(defrule request-star-rule
  "Matches a process and its successor that hold two different resources, and
  makes the successor request its predecessor's resource."
  [model] [r1<Resource> -<:holder>-> p1 -<:prev>-> p2 -<:held>-> r2
           p1 -!<:requested>-> r2
           :isomorphic]
  (eadd! p1 :requested r2))

(defrule release-star-rule
  "Matches a process holding 2 resources where one is requested by another
  process, and releases the requested one."
  ([model] [p1<Process> -<:requested>-> r1 -<:holder>-> p2 -<:held>-> r2
            :isomorphic]
   (eunset! r1 :holder)
   (eset! r1 :releaser p2))
  ([model r2 p2] [p2 -<:held>-> r1 -<:requester>-> p1 :isomorphic]
   (eunset! r1 :holder)
   (eset! r1 :releaser p2)))


(defn apply-mutual-exclusion-lts
  "Performs the LTS transformation."
  [model n param-pass]
  ;; The main entry point
  (let [cnt (atom 0)]
    (dotimes [_ n]
      (request-star-rule model))
    (blocked-rule model)
    (dotimes [_ (dec n)]
      (waiting-rule model))
    (unlock-rule model)
    (blocked-rule model)
    (if param-pass
      ((iterated-rule #(or ((iterated-rule* waiting-rule) model)
                           (waiting-rule model))))
      ((iterated-rule waiting-rule) model))
    (ignore-rule model)
    (if param-pass
      ((iterated-rule #(apply release-star-rule model
                              (apply take-rule model
                                     (give-rule model)))))
      ((iterated-rule (sequential-rule
                       give-rule take-rule release-star-rule))
       model))
    (give-rule model)
    (take-rule model)))

(defn g-lts
  "Returns an initial graph for the LTS.
  n processes and n resources.
  n Next edges organize the processes in a token ring.
  n HeldBy edges assign to each process a resource."
  [n]
  (let [m (new-resource)]
    (loop [i n, lp nil]
      (if (pos? i)
        (let [r (ecreate! m 'Resource)
              p (ecreate! m 'Process)]
          (when lp
            (eset! lp :next p))
          (eset! p :name (str "p" (- (inc n) i)))
          (eset! r :name (str "r" (- (inc n) i)))
          (eset! r :holder p)
          (recur (dec i) p))
        (eset! lp :next (first (econtents m 'Process)))))
    m))


;;* Tests

(deftest mutual-exclusion-sts
  (println)
  (println "Mutual Exclusion STS")
  (println "====================")
  (doseq [n [5, 100, 500]]
    (let [g1 (g-sts)
          g2 (g-sts)]
      (println "N =" n)
      (print "  without parameter passing:\t")
      (time (apply-mutual-exclusion-sts g1 n false))
      (is (= (inc n) (count (eallcontents g1))))
      (is (= (inc n) (count (ecrosspairs g1))))
      ;;(print-model g1 ".gtk")

      (print "  with parameter passing:\t")
      (time (apply-mutual-exclusion-sts g2 n true))
      (is (= (inc n) (count (eallcontents g2))))
      (is (= (inc n) (count (ecrosspairs g2))))
      ;;(print-model g2 ".gtk")
      )))

(deftest mutual-exclusion-lts
  (println)
  (println "Mutual Exclusion LTS")
  (println "====================")
  (doseq [[n r vc ec] [[4 100 8 8]
                       [1000 1 2000 2000]]]
    (let [g1 (g-lts n)
          g2 (g-lts n)]
      (println "N =" n ", R =" r)

      (print "  without parameter passing:\t")
      (time (dotimes [_ r] (apply-mutual-exclusion-lts g1 n false)))
      #_(print-model g1 ".gtk")
      (is (= vc (count (eallcontents g1))))
      (is (= ec (count (ecrosspairs g1))))


      (print "  with parameter passing:\t")
      (time (dotimes [_ r] (apply-mutual-exclusion-lts g2 n true)))
      (is (= vc (count (eallcontents g2))))
      (is (= ec (count (ecrosspairs g2)))))))

