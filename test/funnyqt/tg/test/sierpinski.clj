(ns funnyqt.tg.test.sierpinski
  (:use funnyqt.tg.core)
  (:use funnyqt.match-replace)
  (:use funnyqt.tg.query)
  (:use clojure.test)
  (:import [java.util.concurrent Executors ExecutorService Future]
           [de.uni_koblenz.jgralab.codegenerator CodeGeneratorConfiguration]
           [de.uni_koblenz.jgralab ImplementationType]))


(defn sierpinski-init
  "Returns a sierpienki triangle."
  [vc ec variant]
  (let [g (create-graph (load-schema "test/sierpinski.tg" variant)
                        "Sierpinski" variant)
        t (create-vertex! g 'V)
        l (create-vertex! g 'V)
        r (create-vertex! g 'V)]
    (create-edge! 'L t l)
    (create-edge! 'R t r)
    (create-edge! 'B l r)
    g))

(defrule triangulate
  "triangulate one trinangle"
  [g lv b rv] [l (iseq lv 'L :in)
               :let [tv (alpha l)]
               r (iseq rv 'R :in)]
  (let [nl (create-vertex! g 'V)
        nr (create-vertex! g 'V)
        nb (create-vertex! g 'V)]
    (set-omega! l nl)
    (set-omega! r nr)
    (set-omega! b nb)

    (create-edge! 'L nl lv)
    (create-edge! 'R nr rv)
    (create-edge! 'B nb rv)

    (create-edge! 'L nr nb)
    (create-edge! 'R nl nb)
    (create-edge! 'B nl nr)))

(defn triangulate-sequential
  [g]
  (doseq [b (doall (eseq g 'B))]
    (triangulate g (alpha b) b (omega b))))

(defrule triangulate-recursively
  "triangulate all recursively"
  ([g] [tv (filter #(empty? (iseq % 'B)) (vseq g))]
     (triangulate-recursively g tv))
  ([g tv] [l (iseq tv 'L :out)
           :let [lv (omega l)]
           r (iseq tv 'R :out)
           :let [rv (omega r)]
           b (iseq lv 'B :out)]
     (let [nlv (create-vertex! g 'V)
           nrv (create-vertex! g 'V)
           nbv (create-vertex! g 'V)]
       (set-omega! l nlv)
       (set-omega! r nrv)
       (set-omega! b nbv)

       (create-edge! 'L nlv lv)
       (create-edge! 'R nrv rv)
       (create-edge! 'B nbv rv)

       (create-edge! 'L nrv nbv)
       (create-edge! 'R nlv nbv)
       (create-edge! 'B nlv nrv)
       (triangulate-recursively g lv)
       (triangulate-recursively g rv))))

(defrule triangulate-trampolined
  "triangulate all recursively"
  ([g] [tv (filter #(empty? (iseq % 'B)) (vseq g))]
     (triangulate-trampolined g tv))
  ([g tv] [l (iseq tv 'L :out)
           :let [lv (omega l)]
           r (iseq tv 'R :out)
           :let [rv (omega r)]
           b (iseq lv 'B :out)]
     (let [nlv (create-vertex! g 'V)
           nrv (create-vertex! g 'V)
           nbv (create-vertex! g 'V)]
       (set-omega! l nlv)
       (set-omega! r nrv)
       (set-omega! b nbv)

       (create-edge! 'L nlv lv)
       (create-edge! 'R nrv rv)
       (create-edge! 'B nbv rv)

       (create-edge! 'L nrv nbv)
       (create-edge! 'R nlv nbv)
       (create-edge! 'B nlv nrv)
       (fn []
         (triangulate-recursively g lv)
         (triangulate-recursively g rv)))))

(defn impl-label [g]
  (if (instance? de.uni_koblenz.jgralab.impl.generic.GenericGraphImpl g)
    "gen"
    "std"))

(defn run
  [g f title n correct-vc correct-ec]
  (print (format "  ==> %s (%s): " title (impl-label g)))
  (time (dotimes [_ n] (f g)))
  (let [vc (vcount g), ec (ecount g)]
    (is (== correct-vc vc))
    (is (== correct-ec ec))))

(deftest sierpinski
  (println "Sierpinski Triangles")
  (println "====================")
  (doseq [variant [:generic :standard]]
    (doseq [n (range 8 13)]
      (let [correct-vc (int (* 3/2 (inc (Math/pow 3 n))))
            correct-ec (int (Math/pow 3 (inc n)))]
        (System/gc)
        (println (format "No. of generations: %s (%s vertices, %s edges)"
                         n correct-vc correct-ec))
        (run (sierpinski-init correct-vc correct-ec variant)
            #(triangulate-sequential %)
          "SEQUEN" n correct-vc correct-ec)
        (run (sierpinski-init correct-vc correct-ec variant)
            #(triangulate-recursively %)
          "RECURS" n correct-vc correct-ec)
        (run (sierpinski-init correct-vc correct-ec variant)
            #(trampoline (triangulate-trampolined %))
          "TRAMPO" n correct-vc correct-ec)))))

