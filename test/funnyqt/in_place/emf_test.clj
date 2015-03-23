(ns ^{:pattern-expansion-context :emf}
  funnyqt.in-place.emf-test
  (:require [funnyqt.generic :as g]
            [funnyqt.query   :as q]
            [funnyqt.emf     :as emf]
            [funnyqt.tg      :as tg]
            [funnyqt.pmatch  :as pmatch])
  (:use funnyqt.in-place)
  (:use clojure.test))

(emf/load-ecore-resource "test/input/clock.ecore")

(defn hour [g h]
  (q/the #(= h (emf/eget % :hour))
         (emf/eallcontents g 'Hour)))

(defn ^:private clock-model []
  (let [g (emf/new-resource)]
    (dotimes [i 12]
      (let [h (emf/ecreate! g 'Hour {:hour (inc i)})]
        (when-not (zero? i)
          (emf/eset! h :prev (hour g i)))))
    (emf/eset! (hour g 12) :next (hour g 1))
    (let [c (emf/ecreate! g 'Clock)]
      (emf/eset! c :current (hour g 12)))
    g))

(defrule tick-forward [g]
  [c<Clock> -<:current>-> <> -<:next>-> nh]
  (emf/eset! c :current nh))

(defrule tick-backward [g]
  [c<Clock> -<:current>-> <> -<:prev>-> ph]
  (emf/eset! c :current ph))

(defrule reset-clock [g]
  [c<Clock> -<:current>-> h
   :when (not= 12 (emf/eget h :hour))]
  (emf/eset! c :current (hour g 12)))

(deftest test-state-space-1
  (let [g (clock-model)
        [ssg s2g] (create-state-space
                   g
                   g/equal-models?
                   [tick-forward tick-backward reset-clock])]
    ;;(./print-model ssg :gtk)
    (is (= 12 (tg/vcount ssg)))
    ;; From every Hour we can tick forward and backward, and from every state
    ;; except for the first, we can reset to 12 o'clock.
    (is (= 35 (tg/ecount ssg)))))

(deftest test-state-space-2
  (let [g (clock-model)
        [ssg s2g] (create-state-space
                   g
                   #(g/equal-models? %1 %2 true)
                   [tick-forward tick-backward reset-clock])]
    ;;(./print-model ssg :gtk)
    (is (= 12 (tg/vcount ssg)))
    ;; From every Hour we can tick forward and backward, and from every state
    ;; except for the first, we can reset to 12 o'clock.
    (is (= 35 (tg/ecount ssg)))))

(defrule erase-clock-hand [g]
  [c<Clock> -<:current>-> <>]
  (emf/eset! c :current nil))

(pmatch/defpattern current-hour-exists? [g]
  [c<Clock> -<:current>-> <>])

(defn test-explore-state-space []
  (let [g (clock-model)]
    (explore-state-space
     g
     #(g/equal-models? %1 %2 false)
     [tick-forward tick-backward reset-clock erase-clock-hand]
     [#(seq (current-hour-exists? %))]
     {}
     [#(<= (tg/vcount %) 12)])))
