(ns funnyqt.test.in-place
  (:use funnyqt.in-place))

(defn refer-private [nssym sym]
  (intern *ns* sym ((ns-interns (find-ns nssym)) sym)))

;; Make it public here for testing!
(refer-private 'funnyqt.in-place 'transform-match-vector)

;; TODO: Write tests!
