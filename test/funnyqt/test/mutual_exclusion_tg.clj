(ns funnyqt.test.mutual-exclusion-tg
  (:use funnyqt.tg.core)
  (:use funnyqt.utils)
  (:use funnyqt.protocols)
  (:use funnyqt.in-place)
  (:use funnyqt.tg.query)
  (:use clojure.test))

;;* Rules

;;** Short Transformation Sequence

(defrule new-rule
  "Matches 2 connected processes and injects a new process in between."
  [g] [p1 (vseq g 'Process)
       n  (iseq p1 'Next :out)
       :let [p2 (omega n)]]
  (let [p (create-vertex! g 'Process)]
    (set-omega! n p)
    (create-edge! g 'Next p p2)))

(defrule kill-rule
  "Matches a sequence of 3 connected processes and deletes the middle one."
  [g] [p1 (vseq g 'Process)
       n1 (iseq p1 'Next :out)
       :let [p (omega n1)]
       n2 (iseq p 'Next :out)
       :let [p2 (omega n2)]]
  (set-omega! n1 p2)
  (delete! p))

(defrule mount-rule
  "Matches a process and creates and assigns a resource to it."
  [g] [p (vseq g 'Process)]
  (create-edge! g 'Token (create-vertex! g 'Resource) p))

(defrule unmount-rule
  "Matches a resource assigned to a process and deletes it."
  [g] [r (vseq g 'Resource)
       t (iseq r 'Token :out)
       :let [p (omega r)]]
  (delete! r))

(defrule pass-rule
  "Passes the token to the next process if the current doesn't request it."
  [g] [r (vseq g 'Resource)
       t (iseq r 'Token :out)
       :let [p1 (omega t)]
       :when (empty? (filter #(= (omega %) r)
                             (iseq p1 'Request :out)))
       n (iseq p1 'Next :out)
       :let [p2 (omega n)]]
  (set-omega! t p2))

(defrule request-rule
  "Matches a process that doesn't request any resource and a resource not held
  by that process and makes the process request that resource."
  [g] [p (vseq g 'Process)
       :when (empty? (iseq p 'Request :out))
       r (vseq g 'Resource)
       :when (empty? (filter #(= (omega %) p)
                             (iseq r 'HeldBy :out)))]
  (create-edge! g 'Request p r))

(defrule take-rule
  "Matches a process that requests a resource that in turn tokens the process
  and makes the process hold that resource."
  ([g] [r  (vseq g 'Resource)
        t  (iseq r 'Token :out)
        :let [p (omega t)]
        rq (iseq r 'Request :in)
        :when (= p (alpha rq))]
     (take-rule g r t p rq))
  ([g r t p] [rq (iseq r 'Request :in)
              :when (= p (alpha rq))]
     (take-rule g r t p rq))
  ([g r t p rq]
     (delete! [t rq])
     ;; Return a vec of the resource, HeldBy and process for param passing
     [r (create-edge! g 'HeldBy r p) p]))

(defrule release-rule
  "Matches a resource holding a resource and not requesting more resources, and
  releases that resource."
  ([g] [r  (vseq g 'Resource)
        hb (iseq r 'HeldBy :out)
        :let [p (omega hb)]]
     (release-rule g r hb p))
  ([g r hb p]
     (when (empty? (iseq p 'Request :out))
       (delete! hb)
       [r (create-edge! g 'Release r p) p])))

(defrule give-rule
  "Matches a process releasing a resource, and gives the token to that resource
  to the next process."
  ([g] [p1  (vseq g 'Process)
        rel (iseq p1 'Release :in)
        :let [r (alpha rel)]
        n (iseq p1 'Next :out)
        :let [p2 (omega n)]]
     (give-rule g r rel p1 n p2))
  ([g r rel p1] [n (iseq p1 'Next :out)
                 :let [p2 (omega n)]]
     (give-rule g r rel p1 n p2))
  ([g r rel p1 n p2]
     (delete! rel)
     [r (create-edge! g 'Token r p2) p2]))

(defrule blocked-rule
  "Matches a process requesting a resource held by some other process, and
  creates a blocked edge."
  [g] [r   (vseq g 'Resource)
       req (iseq r 'Request :in)
       :let [p1 (alpha req)]
       hb  (iseq r 'HeldBy :out)
       :let [p2 (omega hb)]]
  (create-edge! g 'Blocked r p1))

(defrule waiting-rule
  "Moves the blocked state."
  ([g] [hb (eseq g 'HeldBy)
        :let [r1 (alpha hb)
              p1 (omega hb)]
        b (iseq p1 'Blocked :in)
        :let [r2 (alpha b)]
        p2 (adjs r1 :requester)]
     (waiting-rule g r1 b p2))
  ([g r1] [req (iseq r1 'Request :in)
           :let [p2 (alpha req)]
           hb  (iseq r1 'HeldBy :out)
           :let [p1 (omega hb)]
           b   (iseq p1 'Blocked :in)
           :let [r2 (alpha b)]
           :when (not= r1 r2)]
     (waiting-rule g r1 b p2))
  ([g r1 b p2]
     (set-omega! b p2)
     [g r1]))


(defrule ignore-rule
  "Removes the blocked state if nothing is held anymore."
  [g] [p (vseq g 'Process)
       :when (empty? (iseq p 'HeldBy :in))
       b (iseq p 'Blocked :in)]
  (delete! b))

(defrule unlock-rule
  "Matches a process holding and blocking a resource and releases it."
  [g] [r  (vseq g 'Resource)
       hb (iseq r 'HeldBy :out)
       :let [p (omega hb)]
       b  (iseq r 'Blocked :out)
       :when (= p (omega b))]
  (delete! [hb b])
  (create-edge! g 'Release r p))

(deftransformation apply-mutual-exclusion-sts
  [g n param-pass]
  (do
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
      (iteratively #(apply give-rule g (apply release-rule g (take-rule g))))
      (iteratively #(do
                      (take-rule g)
                      (release-rule g)
                      (give-rule g))))))

(defn g-sts
  "Returns an initial graph for the STS.
  Two Processes connected in a ring by two Next edges."
  []
  (let [g (create-graph (load-schema "test/input/mutual-exclusion-schema.tg")
                        "Short transformation sequence.")
        p1 (create-vertex! g 'Process)
        p2 (create-vertex! g 'Process)]
    (create-edge! g 'Next p1 p2)
    (create-edge! g 'Next p2 p1)
    g))

;;** Long Transformation Sequence

(deftransformation apply-mutual-exclusion-lts
  [g n param-pass]

  (defrule request-star-rule
    "Matches a process and its successor that hold two different resources, and
  makes the successor request its predecessor's resource."
    [g] [r1 (vseq g 'Resource)
         h1 (iseq r1 'HeldBy :out)
         :let [p1 (omega h1)]
         n  (iseq p1 'Next :in)
         :let [p2 (alpha n)]
         h2 (iseq p2 'HeldBy :in)
         :let [r2 (alpha h2)]
         :when (not= r1 r2)
         :when (empty? (filter #(= p1 (alpha %))
                               (iseq r2 'Request :in)))]
    (create-edge! g 'Request p1 r2))

  (defpattern release-star-pattern
    "Given a resource and a process, matches another process and resource where
    the resource is held by the given process and another process requests it."
    [g r2 p2] [h1 (iseq p2 'HeldBy :in)
               :let [r1 (alpha h1)]
               rq (iseq r1 'Request :in)
               :let [p1 (alpha rq)]
               :when (and (not= r1 r2) (not= p1 p2))])

  (defrule release-star-rule
    "Matches a process holding 2 resources where one is requested by another
  process, and releases the requested one."
    ([g] [p2 (vseq g 'Process)
          h2 (iseq p2 'HeldBy :in)
          :let [r2 (alpha h2)]
          [h1 r1 rq p1] (release-star-pattern g r2 p2)]
       (release-star-rule g r2 h2 p2 h1 r1 rq p1))
    ([g r2 h2 p2] [[h1 r1 rq p1] (release-star-pattern g r2 p2)]
       (release-star-rule g r2 h2 p2 h1 r1 rq p1))
    ([g r2 h2 p2 h1 r1 rq p1]
       (delete! h1)
       (create-edge! g 'Release r1 p2)))

  (do
    (dotimes [_ n]
      (request-star-rule g))
    (blocked-rule g)
    (dotimes [_ (dec n)]
      (waiting-rule g))
    (unlock-rule g)
    (blocked-rule g)
    (if param-pass
      (iteratively #(or (iteratively* waiting-rule g)
                        (waiting-rule g)))
      (iteratively #(waiting-rule g)))
    (ignore-rule g)
    (if param-pass
      (iteratively #(apply release-star-rule % (apply take-rule % (give-rule %))) g)
      (iteratively #(do (give-rule g) (take-rule g) (release-star-rule g))))
    (give-rule g)
    (take-rule g)))

(defn g-lts
  "Returns an initial graph for the LTS.
  n processes and n resources.
  n Next edges organize the processes in a token ring.
  n HeldBy edges assign to each process a resource."
  [n]
  (let [g (create-graph (load-schema "test/input/mutual-exclusion-schema.tg")
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
  (doseq [n [5, 100, 1000]]
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
  (doseq [[n r] [[4, 100] [30, 27] [500, 1]]]
    (let [g1 (g-lts n)
          vc (* 2 n)
          ec (ecount g1)
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
