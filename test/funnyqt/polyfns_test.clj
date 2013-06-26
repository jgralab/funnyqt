(ns funnyqt.polyfns-test
  (:require [funnyqt.tg :as tg])
  (:use funnyqt.polyfns)
  (:use [funnyqt.tg-test :only [jg]])
  (:use clojure.test)
  (:require [criterium.core :as c]))

;; TODO: Write tests!

(declare-polyfn aec-name [elem]
                "--undefined--")

(defpolyfn aec-name 'containers.CompilationUnit
  [elem]
  "containers.CompilationUnit")

(defpolyfn aec-name 'imports.ClassifierImport
  [elem]
  "imports.ClassifierImport")

(defpolyfn aec-name 'classifiers.ConcreteClassifier
  [elem]
  "classifiers.ConcreteClassifier")

(defpolyfn aec-name 'classifiers.Class
  [elem]
  "classifiers.Class")

(defpolyfn aec-name 'operators.Assignment
  [elem]
  "operators.Assignment")

(defpolyfn aec-name 'expressions.Expression
  [elem]
  "expressions.Expression")

(defpolyfn aec-name 'expressions.CastExpression
  [elem]
  "expressions.CastExpression")

(defpolyfn aec-name 'expressions.AdditiveExpression
  [elem]
  "expressions.AdditiveExpression")

(defpolyfn aec-name 'statements.Conditional
  [elem]
  "statements.Conditional")

(defpolyfn aec-name 'statements.Throw
  [elem]
  "statements.Throw")

(deftest time-polyfns
  ;; 1. Execution time mean : 208.373278 ms
  ;;    Execution time mean : 189.096473 ms
  ;;    Execution time mean : 196.353706 ms
  #_(c/bench (dorun (map aec-name (concat (tg/vseq jg) (tg/eseq jg)))))
  (dotimes [i 10]
    (print (format "%s. run: " i))
    (time (dorun (map aec-name (concat (tg/vseq jg) (tg/eseq jg)))))))

(deftest test-polyfn-aec-name
  (doseq [el (tg/vseq jg 'Class)]
    (is (= "classifiers.Class" (aec-name el))))
  (doseq [el (tg/vseq jg '[classifiers.Enumeration classifiers.Interface])]
    (is (= "classifiers.ConcreteClassifier" (aec-name el))))
  (doseq [el (tg/vseq jg '[:nor expressions.Expression operators.Assignment
                           classifiers.ConcreteClassifier imports.ClassifierImport
                           containers.CompilationUnit statements.Conditional
                           statements.Throw])]
    (is (= "--undefined--" (aec-name el)))))
