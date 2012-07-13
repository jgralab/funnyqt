(ns funnyqt.in-place
  "In-place transformation stuff."
  (:use [funnyqt.utils :only [errorf pr-identity]])
  (:use [funnyqt.query :only [the for*]])
  (:use [funnyqt.pmatch])
  (:use funnyqt.macro-utils)
  (:use funnyqt.protocols)
  (:use [funnyqt.query :only [member?]])
  (:require [clojure.tools.macro :as m]))


;;# Rules

(defn- shortcut-let-vector [lv]
  (mapcat (fn [[s v]]
            [:let [s v]
             :when s])
          (partition 2 lv)))

(defn- shortcut-bindings
  "Converts :let [x (foo), y (bar)] to :let [x (foo)] :when x :let [y (bar)] :when y."
  [bindings]
  (loop [p bindings, nb []]
    (if (seq p)
      (if (= :let (first p))
        (recur (rest (rest p))
               (vec (concat nb (shortcut-let-vector (fnext p)))))
        (recur (rest (rest p)) (conj (conj nb (first p)) (second p))))
      (vec nb))))

(defmacro with-match
  "Establish bindings as specified in `bindings`, and execute `body`.
  `bindings` is a vector of bindings with the syntax of `for*` or a pattern.

  If a match could be found, that is, all symbols in `bindings` can be bound to
  non-nil values, `body` is executed with the established bindings and `body`s
  value is the return value.  If no match is found, nil is returned."
  ;; Nicer arglist in doc
  {:arglists '([[bindings*] & body])}
  [bindings & body]
  (when (not= 0 (mod (count bindings) 2))
    (errorf "bindings has to be var-exp pairs"))
  (let [arglist (bindings-to-arglist bindings)
        sbindings (shortcut-bindings bindings)
        r `r#]
    `(when-let [~r (first (for* ~sbindings ~arglist))]
       ;; We want no matches with nil values
       (when (every? (complement nil?) ~r)
         ;; Rip out the individual values of the result and let-bind them.
         ;; Alternatively, one could create a (apply (fn [args] body) result),
         ;; but that consumes much more stack space in plain recursive rules
         ;; (i.e., when one cannot use recur because it is recursed more than
         ;; once).
         (let ~(loop [a arglist, i 0, res []]
                 (if (seq a)
                   (recur (rest a) (inc i) (concat res [(first a) `(~r ~i)]))
                   (vec res)))
           ~@body)))))

(def ^{:dynamic true
       :doc "A function that is invoked when a rule matches,
  mainly for debugging purposes.
  The function gets the following arguments: [r args match]
    - r is a symbol denoting the current matched rule
    - args is the vector of the rule's input arguments
    - match is the vector of the elements matched by the rule"}
  *on-matched-rule-fn* nil)

(defmacro defrule-internal
  [debug name more]
  (let [[name more] (m/name-with-attributes name more)
        convert (fn [s]
                  (let [args  (first s)
                        more (next s)]
                    (if (vector? (first more))
                      ;; match vector given
                      (let [match (first more)
                            body (next more)]
                        `(~args
                          (with-match ~(transform-match-vector match args)
                            ~@(when debug
                                `((when *on-matched-rule-fn*
                                    (*on-matched-rule-fn*
                                     '~name ~args ~(bindings-to-arglist match)))))
                            ~@body)))
                      ;; No match given
                      `(~args ~@more))))]
    `(defn ~name ~(meta name)
       ~@(if (seq? (first more))
           (map convert more)
           (convert more)))))

(defmacro defrule
  "Defines a rule with `name`, optional doc-string', optional `attr-map?',
  an `args` vector, an optional `match` vector, and following `body` code.
  Just like `defn`, overloading is supported as well.  The `match` vector is
  actually optional.  If no version has a match, then you should use `defn`
  directly.

  `match` specifies what the rule matches (a vector with the syntax of `for`).
  The match vector is optional.  The purpose of this optionality is mainly
  overloading, i.e., you can have a rule like this:

    (defrule foobar
      \"Matches a, b, and c, and performs actions on them.\"
      ([g] [a ..., b ..., c ...] (foobar g a b c))
      ([g a] [b ..., c ...]      (foobar g a b c))
      ([g a b] [c ...]           (foobar g a b c))
      ([g a b c]  ;; No match vector, just actions
        (action1 a)
        (action2 b)
        (action3 c)
        (action4 a c)))

  `body` is applied on the match.

  If the rule could be applied, then it returns the value of the last form in
  body, and any logical true value means the rule succeeded.  Thus, one should
  take care to always return logical true if the rule was applied.

  It it a compile error, if the `args` contain vars that occur also in
  `match`."
  {:arglists '([name doc-string? attr-map? [args] [match] & body]
                 [name doc-string? attr-map? ([args] [match] & body)+])}
  [name & more]
  `(defrule-internal false ~name ~more))

(defmacro defrule-debug
  "Exactly the same as `defrule`, but expands into a rule that calls
`*on-matched-rule-fn*', if that's bound."
  {:arglists '([name doc-string? attr-map? [args] [match] & body]
                 [name doc-string? attr-map? ([args] [match] & body)+])}
  [name & more]
  `(defrule-internal true ~name ~more))

;;# Transformations

(defmacro deftransformation
  {:doc "Defines a match-replace transformation named `name` with optional
  `doc-string?', optional `meta-map?, a mandatory `args` vector, and a `body`.
  The `body` must consist of arbitrary many `defpattern` and `defrule` forms,
  and exactly one other form, the main entry point of the transformation.  This
  form is evaluated when the transformation is called.

  All patterns, rules, and the main form of the transformation have access to
  the `args` of the transformation."
   :arglists '([name doc-string? meta-map? [args] & body])}
  [tname & more]
  (let [[tname more] (m/name-with-attributes tname more)
        args (first more)
        body (next more)]
    (when-not (vector? args)
      (errorf "No args vector specified for transformation %s." args))
    `(defn ~tname ~(meta tname)
       ~args
       ~@body)))

;;# Higher order rules

(defn iteratively
  "Applies the rule `r` with `args` as long as it returns logical true.
  Returns the number of successful applications or nil if it couldn't be
  applied at least once."
  [r & args]
  (loop [val (apply r args), i 0]
    (if val
      (recur (apply r args) (inc i))
      (when-not (zero? i) i))))

(defn iteratively*
  "Applies the rule `r` as long as it returns logical true.
  On the first application, `r` receives `args`.  The second till last
  application receive the value of the previous successful application.
  Returns the number of successful applications, or nil, if it couldn't be
  applied at least once."
  [r & args]
  (loop [val (apply r args), i 0]
    (if val
      (recur (apply r val) (inc i))
      (when-not (zero? i) i))))

(defn ntimes
  "Applies the rule `r` at most `n` times and returns the number of successfull
  applications.  Stops as soon as `r` fails."
  [n r & args]
  (loop [n n, succs 0]
    (if (and (pos? n) (apply r args))
      (recur (dec n) (inc succs))
      succs)))

(defn choose
  "Randomly chooses one of the given `rules` and applies it.
  Returns that fun's return value or nil, if no fun was applicable."
  [& rules]
  (let [r (rand-nth rules)
        v (r)]
    (or v (recur (remove #(= r %) rules)))))
