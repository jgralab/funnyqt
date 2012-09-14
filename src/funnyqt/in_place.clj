(ns funnyqt.in-place
  "In-place transformation stuff."
  (:use [funnyqt.utils :only [errorf pr-identity]])
  (:use [funnyqt.query :only [the]])
  (:use [funnyqt.pmatch])
  (:use funnyqt.protocols)
  (:use [funnyqt.query :only [member?]])
  (:require [clojure.tools.macro :as m]))


;;# Rules

(def ^{:dynamic true
       :doc "A function that is invoked when a rule matches,
  mainly for debugging purposes.
  The function gets the following arguments: [r args match]
    - r is a symbol denoting the current matched rule
    - args is the vector of the rule's input arguments
    - match is the vector of the elements matched by the rule"}
  *on-matched-rule-fn* nil)

(defn- convert-spec
  "spec is ([args] [pattern] & body) or ([args] & body)."
  [name debug spec]
  (let [args (first spec)
        more (next spec)]
    (if (vector? (first more))
      ;; match vector given
      (let [match (first more)
            match (transform-pattern-vector match args)
            matchsyms (bindings-to-arglist match)
            body (next more)]
        `(~args
          (when-let [r# (first (pattern-for ~match ~matchsyms))]
            (when (every? (complement nil?) r#)
              (let [~matchsyms r#]
                ~@(when debug
                    `((when *on-matched-rule-fn*
                        (*on-matched-rule-fn*
                         '~name ~args ~matchsyms))))
                ~@body)))))
      ;; No match given
      `(~args ~@more))))

(defmacro rule
  "Defines a rule.  Stands to defrule (which see) in the same way as fn stands
  to defn."
  {:arglists '([name? [args] [pattern] & body]
                 [name? ([args] [pattern] & body)+])}
  [& more]
  (let [[name more] (if (symbol? (first more))
                      [(first more) (next more)]
                      [nil more])
        [name more] (if name
                      (m/name-with-attributes name more)
                      [name more])]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              (:pattern-expansion-context (meta *ns*))
                                              *pattern-expansion-context*)]
      `(fn ~@(when name [name])
         ~@(if (seq? (first more))
             (doall (map (partial convert-spec name (:debug (meta name))) more))
             (convert-spec name (:debug (meta name)) more))))))

(defmacro letrule
  "Establishes local rules just like `letfn` establishes local fns.
  Also see `rule` and `defrule`."
  {:arglists '([[rspecs] & body])}
  [rspecs & body]
  (when-not (vector? rspecs)
    (errorf "No rspec vector in letmapping!"))
  (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                            (:pattern-expansion-context (meta *ns*))
                                            *pattern-expansion-context*)]
    `(letfn [~@(map (fn [[n & more]]
                      `(~n ~@(if (seq? (first more))
                               (doall (map (partial convert-spec name nil) more))
                               (convert-spec name false more))))
                 rspecs)]
       ~@body)))

(defmacro defrule
  "Defines a rule with `name`, optional doc-string', optional `attr-map?',
  an `args` vector, an optional `pattern` vector, and following `body` code.
  Just like `defn`, overloading is supported as well.  The `pattern` vector is
  actually optional.  If no version has it, then you should use `defn`
  directly.

  `pattern` is a vector with the syntax of funnyqt.pmatch/defpattern.  The
  pattern is optional.  The purpose of this optionality is mainly overloading,
  i.e., you can have a rule like this:

    (defrule foobar
      \"Matches a, b, and c, and performs actions on them.\"
      ([g] [a --> b --> c ...] (foobar g a b c))
      ([g a] [b --> c ...]     (foobar g a b c))
      ([g a b] [c ...]         (foobar g a b c))
      ([g a b c]  ;; No match vector, just actions
        (action1 a)
        (action2 b)
        (action3 c)
        (action4 a c)))

  `body` is applied on the match.

  If the rule could be applied, then it returns the value of the last form in
  body, and any logical true value means the rule succeeded.  Thus, one should
  take care to always return logical true if the rule was applied.

  If a defrule form has ^:debug metadata, on every invocation of that rule
  *on-matched-rule-fn* is invoked which you can use to inspect matches."
  {:arglists '([name doc-string? attr-map? [args] [pattern] & body]
                 [name doc-string? attr-map? ([args] [pattern] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)]
    (binding [*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                              (:pattern-expansion-context (meta *ns*))
                                              *pattern-expansion-context*)]
      `(defn ~name ~(meta name)
         ~@(if (seq? (first more))
             (doall (map (partial convert-spec name (:debug (meta name))) more))
             (convert-spec name (:debug (meta name)) more))))))

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
