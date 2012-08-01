(ns funnyqt.test.pmatch
  (:use funnyqt.pmatch)
  (:use funnyqt.protocols)
  (:use funnyqt.tg)
  (:use funnyqt.query.tg)
  (:use clojure.test))

(def fg (load-graph "test/input/familygraph.tg"))

(defpattern families-with-fathers [g]
  [f<Family> -hf<HasFather>-> m<Member>])

(deftest test-families-with-fathers
  (is (= 3 (count (families-with-fathers fg)))))

