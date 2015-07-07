(ns ^{:pattern-expansion-context :tg}
  funnyqt.misc-tests.mutual-exclusion-tg
  (:require [clojure.test :refer :all]
            [funnyqt
             [generic :as g]
             [in-place :refer :all]
             [tg :refer :all]
             [utils :refer :all]]))

;;* Rules

;;** Short Transformation Sequence

(defrule new-rule
  "Matches 2 connected processes and injects a new process in between."
  [g] [p1<Process> -n<Next>-> p2
       :isomorphic]
  (let [p (create-vertex! g 'Process)]
    (set-omega! n p)
    (create-edge! g 'Next p p2)))

(defrule kill-rule
  "Matches a sequence of 3 connected processes and deletes the middle one."
  [g] [p1<Process> -n1<Next>-> p -n2<Next>-> p2
       :isomorphic]
  (set-omega! n1 p2)
  (g/delete! p))

(defrule mount-rule
  "Matches a process and creates and assigns a resource to it."
  [g] [p<Process>]
  (create-edge! g 'Token (create-vertex! g 'Resource) p))

(defrule unmount-rule
  "Matches a resource assigned to a process and deletes it."
  [g] [r<Resource> -t<Token>-> p]
  (g/delete! r))

(defrule pass-rule
  "Passes the token to the next process if the current doesn't request it."
  [g] [r<Resource> -t<Token>-> p1 -!<Request>-> r
       p1 -n<Next>-> p2]
  (set-omega! t p2))

(defrule request-rule
  "Matches a process that doesn't request any resource and a resource not held
  by that process and makes the process request that resource."
  [g] [r<Resource> -!<HeldBy>-> p<Process> -!<Request>-> <>]
  (create-edge! g 'Request p r))

(defrule take-rule
  "Matches a process that requests a resource that in turn tokens the process
  and makes the process hold that resource."
  ([g] [p -rq<Request>-> r -t<Token>-> p]
   (take-rule g r t p rq))
  ([g r t p] [p -rq<Request>-> r]
   (take-rule g r t p rq))
  ([g r t p rq]
   (g/delete! [t rq])
   ;; Return a vec of the resource, HeldBy and process for param passing
   [r (create-edge! g 'HeldBy r p) p]))

(defrule release-rule
  "Matches a resource holding a resource and not requesting more resources, and
  releases that resource."
  ([g] [r<Resource> -hb<HeldBy>-> p -!<Request>-> <>]
   (release-rule g r hb p))
  ([g r hb p]
   (g/delete! hb)
   [r (create-edge! g 'Release r p) p]))

(defrule give-rule
  "Matches a process releasing a resource, and gives the token to that resource
  to the next process."
  ([g] [r<Resource> -rel<Release>-> p1 -n<Next>-> p2]
   (give-rule g r rel p1 n p2))
  ([g r rel p1] [p1 -n<Next>-> p2]
   (give-rule g r rel p1 n p2))
  ([g r rel p1 n p2]
   (g/delete! rel)
   [r (create-edge! g 'Token r p2) p2]))

(defrule blocked-rule
  "Matches a process requesting a resource held by some other process, and
  creates a blocked edge."
  [g] [p1<Process> -req<Request>-> r -hb<HeldBy>-> p2
       :isomorphic]
  (create-edge! g 'Blocked r p1))

(defrule waiting-rule
  "Moves the blocked state."
  ([g] [p2<Process> -req<Request>-> r1 -hb<HeldBy>->
        p1 <-b<Blocked>- r2
        :isomorphic]
   (waiting-rule g r1 b p2))
  ([g r1] [r1 <-req<Request>- p2
           r1 -hb<HeldBy>-> p1 <-b<Blocked>- r2
           :isomorphic]
   (waiting-rule g r1 b p2))
  ([g r1 b p2]
   (set-omega! b p2)
   [g r1]))


(defrule ignore-rule
  "Removes the blocked state if nothing is held anymore."
  [g] [r<Resource> -b<Blocked>-> p
       :when (empty? (iseq p 'HeldBy))]
  (g/delete! b))

(defrule unlock-rule
  "Matches a process holding and blocking a resource and releases it."
  [g] [r<Resource> -hb<HeldBy>-> p <-b<Blocked>- r]
  (g/delete! [hb b])
  (create-edge! g 'Release r p))

(defn apply-mutual-exclusion-sts
  [g n param-pass]
  ;; n-2 times new-rule ==> n processes in a ring
  (dotimes [_ (- n 2)]
    (new-rule g))
  ;; mount a resource and give token to one process
  (mount-rule g)
  ;; Let all processe issue a request to the single resource
  (dotimes [_ n]
    (request-rule g))
  ;; Handle the requests...
  (if param-pass
    ((iterated-rule #(apply give-rule g (apply release-rule g (take-rule g)))))
    ((iterated-rule #(do
                       (take-rule g)
                       (release-rule g)
                       (give-rule g))))))

(defn g-sts
  "Returns an initial graph for the STS.
  Two Processes connected in a ring by two Next edges."
  []
  (let [g (new-graph (load-schema "test/input/mutual-exclusion-schema.tg")
                     "Short transformation sequence.")
        p1 (create-vertex! g 'Process)
        p2 (create-vertex! g 'Process)]
    (create-edge! g 'Next p1 p2)
    (create-edge! g 'Next p2 p1)
    g))

;;** Long Transformation Sequence

(defrule request-star-rule
  "Matches a process and its successor that hold two different resources, and
  makes the successor request its predecessor's resource."
  [g] [r1<Resource> -h1<HeldBy>-> p1 <-<Next>- p2 <-h2<HeldBy>- r2
       p1 -!<Request>-> r2
       :isomorphic]
  (create-edge! g 'Request p1 r2))

(defrule release-star-rule
  "Matches a process holding 2 resources where one is requested by another
  process, and releases the requested one."
  ([g] [p1<Process> -rq<Request>-> r1 -h1<HeldBy>-> p2 <-h2<HeldBy>- r2
        :isomorphic]
   (release-star-rule g r2 h2 p2 h1 r1 rq p1))
  ([g r2 h2 p2] [p2 <-h1<_>- r1 <-rq<Request>- p1 :isomorphic]
   (release-star-rule g r2 h2 p2 h1 r1 rq p1))
  ([g r2 h2 p2 h1 r1 rq p1]
   (g/delete! h1)
   (create-edge! g 'Release r1 p2)))

(defn apply-mutual-exclusion-lts
  [g n param-pass]
  (dotimes [_ n]
    (request-star-rule g))
  (blocked-rule g)
  (dotimes [_ (dec n)]
    (waiting-rule g))
  (unlock-rule g)
  (blocked-rule g)
  (if param-pass
    ((iterated-rule #(or ((iterated-rule* waiting-rule) g)
                         (waiting-rule g))))
    ((iterated-rule #(waiting-rule g))))
  (ignore-rule g)
  (if param-pass
    ((iterated-rule #(apply release-star-rule % (apply take-rule % (give-rule %)))) g)
    ((iterated-rule (sequential-rule give-rule take-rule release-star-rule)) g))
  (give-rule g)
  (take-rule g))

(defn g-lts
  "Returns an initial graph for the LTS.
  n processes and n resources.
  n Next edges organize the processes in a token ring.
  n HeldBy edges assign to each process a resource."
  [n]
  (let [g (new-graph (load-schema "test/input/mutual-exclusion-schema.tg")
                     (str "Long transformation sequence, N =" n))]
    (loop [i n, lp nil]
      (if (pos? i)
        (let [r (create-vertex! g 'Resource)
              p (create-vertex! g 'Process)]
          (when lp
            (create-edge! g 'Next lp p))
          (create-edge! g 'HeldBy r p)
          (recur (dec i) p))
        (create-edge! g 'Next lp (first (vseq g 'Process)))))
    g))


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
      (is (= (inc n) (vcount g1)))
      (is (= (inc n) (ecount g1)))

      (print "  with parameter passing:\t")
      (time (apply-mutual-exclusion-sts g2 n true))
      (is (= (inc n) (vcount g2)))
      (is (= (inc n) (ecount g2))))))

(deftest mutual-exclusion-lts
  (println)
  (println "Mutual Exclusion LTS")
  (println "====================")
  ;; vc and ec are the expected values
  (doseq [[n r vc ec] [[4 100 8 8]
                       [1000 1 2000 2000]]]
    (let [g1 (g-lts n)
          g2 (g-lts n)]
      (println "N =" n ", R =" r)
      (print "  without parameter passing:\t")
      (time (dotimes [_ r] (apply-mutual-exclusion-lts g1 n false)))
      (is (= vc (vcount g1)))
      (is (= ec (ecount g1)))

      (print "  with parameter passing:\t")
      (time (dotimes [_ r] (apply-mutual-exclusion-lts g2 n true)))
      (is (= vc (vcount g2)))
      (is (= ec (ecount g2))))))

(defn test-interactive-rule []
  ((interactive-rule
    new-rule kill-rule mount-rule unmount-rule pass-rule request-rule
    take-rule release-rule give-rule blocked-rule waiting-rule
    ignore-rule unlock-rule request-star-rule release-star-rule)
   (g-lts 4)))
