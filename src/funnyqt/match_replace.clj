(ns funnyqt.match-replace
  "Match elements in a structure, and act on them."
  (:use [funnyqt.utils :only [error add-long-doc!]])
  (:require clojure.set)
  (:use [funnyqt.generic :only [member?]])
  (:require [clojure.tools.macro :as m]))

(add-long-doc!
 "TODO")

;;** Matching

(defn- bindings-to-arglist [bindings]
  (loop [p bindings l []]
    (if (seq p)
      (cond
       ;; Handle :let [x y, z a]
       (= :let (first p)) (recur (rest (rest p))
                                 (concat l
                                         (loop [ls (first (rest p)) bs []]
                                           (if (seq ls)
                                             (recur (rest (rest ls))
                                                    (conj bs (first ls)))
                                             bs))))
       ;; Ignore :when (exp ...)
       (keyword? (first p)) (recur (rest (rest p)) l)
       ;; Don't allow destructuring
       (coll? (first p)) (error "Destructuring not allowed outside :let")
       ;; That's a normal binding
       :default (recur (rest (rest p)) (conj l (first p))))
      (vec l))))


(defmacro with-match
  "Establish bindings as specified in `bindings', and execute `body'.
  `bindings' is a vector of bindings with the syntax of `for'.

  If a match could be found, that is, all symbols in `bindings' can be bound to
  non-nil values, `body' is executed with the established bindings and `body's
  value is the return value.  If no match is found, nil is returned."
  ;; Nicer arglist in doc
  {:arglists '([[bindings*] & body])}
  [bindings & body]
  (when (not= 0 (mod (count bindings) 2))
    (error "bindings has to be var-exp pairs"))
  (let [arglist (bindings-to-arglist bindings)
        r `r#]
    `(when-let [~r (first (for ~bindings ~arglist))]
       (when (every? (complement nil?) ~r)
         (let ~(loop [a arglist, i 0, res []]
                 (if (seq a)
                   (recur (rest a) (inc i) (concat res [(first a) `(~r ~i)]))
                   (vec res)))
           ~@body)))))

(defn- verify-match-vector
  "Ensure that the match vector `match' and the arg vector `args' are disjoint.
  Throws an exception if they overlap, else returns `match'."
  [match args]
  (if (seq (clojure.set/intersection
            (set (bindings-to-arglist match))
            (set args)))
    (error "Arglist and match vector overlap!")
    match))

(defmacro defpattern
  "Defines a pattern with `name', optional doc-string', optional `attr-map?',
  an `args' vector, and a `match' vector.  When invoked, it returns a lazy seq
  of all matches of `match'.

  Usually, you use this to specify a pattern that occurs in the match pattern
  of many rules.  So instead of writing a match vector like

    [a (vseq g), b (iseq a) :let [c (that b)], ...]

  in several rules, you do

    (defpattern abc [g] [a (vseq g), b (iseq a) :let [c (that b)]])

  and then

    [[a b c] (abc g), ...]

  in the rules."
  {:arglists '([name doc-string? attr-map? [args] [match]]
                 [name doc-string? attr-map? ([args] [match])+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        convert (fn [[a m]]
                  `(~a
                    (for ~(verify-match-vector m a)
                      ~(bindings-to-arglist m))))]
    `(defn ~name
       ~(meta name)
       ~@(if (seq? (first more))
           (map convert more)
           (convert more)))))

(defmacro defrule
  "Defines a rule with `name', optional doc-string', optional `attr-map?',
  an `args' vector, a `match' vector, and following `body' code.  Just like
  `defn', overloading is supported as well.  The `match' vector is actually
  optional.  If no version has a match, then you should use `defn' directly.

  `match' specifies what the rule matches (a vector with the syntax of `for').

  `body' is applied on the match.

  If the rule could be applied, then it returns the value of the last form in
  body, and any logical true value means the rule succeeded.  Thus, one should
  take care to always return logical true if the rule was applied.

  It it a compile error, if the `args' contain vars that occur also in
  `match'."
  {:arglists '([name doc-string? attr-map? [args] [match] & body]
                 [name doc-string? attr-map? ([args] [match] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        convert (fn [s]
                  (let [args  (first s)
                        more (next s)]
                    (if (vector? (first more))
                      ;; match vector given
                      (let [match (first more)
                            body (next more)]
                        `(~args
                          (with-match ~(verify-match-vector match args)
                            ~@body)))
                      ;; No match given
                      `(~args ~@more))))]
    `(defn ~name
       ~(meta name)
       ~@(if (seq? (first more))
           (map convert more)
           (convert more)))))

(defn iteratively
  "Applies the rule `r' with `args' as long as it returns logical true.
  Returns the number of successful applications or nil if it couldn't be
  applied at least once."
  [r & args]
  (loop [val (apply r args), i 0]
    (if val
      (recur (apply r args) (inc i))
      (if (zero? i) nil i))))

(defn iteratively*
  "Applies the rule `r' as long as it returns logical true.
  On the first application, `r' receives `args'.  The second till last
  application receive the value of the previous successful application.
  Returns the number of successful applications or nil if it couldn't be
  applied at least once."
  [r & args]
  (loop [val (apply r args), i 0]
    (if val
      (recur (apply r val) (inc i))
      (if (zero? i) nil i))))

(defn ntimes
  "Applies the rule `r' at most `n' times and returns the number of successfull
  applications.  Stops as soon as `r' fails."
  [n r & args]
  (loop [n n, succs 0]
    (if (and (pos? n) (apply r args))
      (recur (dec n) (inc succs))
      succs)))

(defn choose
  "Randomly chooses one of the given `rules' and applies it.
  Returns that fun's return value or nil, if no fun was applicable."
  [& rules]
  (let [r (rand-nth rules)
        v (r)]
    (or v (recur (remove #(= r %) rules)))))
