(ns funnyqt.utils-test
  (:use clojure.test)
  (:use funnyqt.utils))

(deftest test-qname?
  (is (qname? 'foo.bar.Baz))
  (is (qname? 'foo.bar.Baz!))
  (is (qname? 'Baz))
  (is (qname? '!Baz))
  (is (qname? 'B))
  (is (qname? 'Baz2))
  (is (qname? 'Baz2!))
  (is (qname? 'foo.b_a_r1.Baz))
  (is (not (qname? :foo.bar.Baz)))
  (is (not (qname? 'foo.bar.1Baz))))

(deftest test-prop-name?
  (is (prop-name? :foo))
  (is (prop-name? :x))
  (is (prop-name? :fooBar))
  (is (prop-name? :foo_Bar))
  (is (not (prop-name? :Baz)))
  (is (not (prop-name? 'foo))))
