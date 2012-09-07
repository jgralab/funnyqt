(ns funnyqt.macro-utils
  "Macro utilities used by various namespaces."
  (:use funnyqt.utils))

;;# Comprehension bindings

(defn bindings-to-arglist
  "Rips out the symbols declared in `bindings`.
  `bindings` is a binding vector with the syntax of `for`."
  [bindings]
  (loop [p bindings l []]
    (if (seq p)
      (cond
       ;; Handle :let [x y, [u v] z]
       (or (= :let (first p))
           (= :when-let (first p)))
       (recur (rest (rest p))
              (vec (concat l
                           (loop [ls (first (rest p)) bs []]
                             (if (seq ls)
                               (recur (rest (rest ls))
                                      (let [v (first ls)]
                                        (if (coll? v)
                                          (into bs v)
                                          (conj bs v))))
                               bs)))))
       ;; Ignore :when (exp ...)
       (keyword? (first p)) (recur (rest (rest p)) l)
       ;; A vector destructuring form
       (vector? (first p)) (recur (rest (rest p)) (vec (concat l (first p))))
       ;; Anothen destructuring form
       (coll? (first p))
       (errorf "Only vector destructuring is permitted outside :let, got: %s"
               (first p))
       ;; That's a normal binding
       :default (recur (rest (rest p)) (conj l (first p))))
      (vec l))))
