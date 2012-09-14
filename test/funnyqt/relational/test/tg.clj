(ns funnyqt.relational.test.tg
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic
        funnyqt.relational.tg
        funnyqt.relational
        [funnyqt.test.tg :only [rg]])
  (:require [funnyqt.tg :as tg]
            [clojure.test :as test]))

(test/deftest test-vertexo
  (test/is (= (tg/vseq rg)
              (run*-on-model rg [q]
                (vertexo q)))))

(test/deftest test-edgeo
  (test/is (= (tg/eseq rg)
              (run*-on-model rg [q]
                (with-fresh
                  (edgeo q _ _)))))
  (test/is (= (map (fn [e]
                     [e (tg/alpha e) (tg/omega e)])
                   (tg/eseq rg))
              (run*-on-model rg [q]
                (with-fresh
                  (edgeo ?e ?a ?o)
                  (== q [?e ?a ?o]))))))

(test/deftest test-typeo
  (test/is (= (tg/vseq rg 'Junction)
              (run*-on-model rg [q]
                (typeo q 'Junction))))
  (test/is (= (tg/eseq rg 'Connection)
              (run*-on-model rg [q]
                (typeo q 'Connection)))))
