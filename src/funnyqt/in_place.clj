(ns ^{:doc "In-place transformation stuff."}
  funnyqt.in-place
  (:require [clojure.string :as str]
            [clojure.tools.macro :as m]
            [funnyqt
             [generic :as g]
             [pmatch :as pm]
             [tg :as tg]
             [utils :as u]
             [visualization :as viz]])
  (:import [java.awt GridBagConstraints GridBagLayout GridLayout]
           [java.awt.event ItemEvent ItemListener]
           [javax.swing AbstractAction Action BorderFactory Box BoxLayout
            DefaultListCellRenderer ImageIcon JButton JCheckBox JComboBox
            JComponent JDialog JLabel JPanel JScrollPane WindowConstants]))

;;# Rules

(def ^{:dynamic true
       :doc "Only for internal use.  See `as-pattern' macro."}
  *as-pattern* false)

(defmacro as-pattern
  "Performs the given rule application `rule-app` as a pattern.
  That is, returns the lazy sequence of matches without applying the rule's
  actions."
  [rule-app]
  `(binding [*as-pattern* true]
     ~rule-app))

(def ^{:dynamic true
       :doc "Only for internal use.  See `as-test' macro."}
  *as-test* false)

(defmacro as-test
  "Performs the given rule application `rule-app` as a test.
  That is, lets the rule find a match and returns a thunk that can be applied
  later to perform the rule's actions.  If there's no match, returns nil.

  Note that if the actions of the rule `recur` to this rule, when called with
  `as-test` you get a plain, stack-consuming recursion instead because `recur`
  would recur to the thunk, not to the rule.

  Note further that when applying a ^:forall rule as test you get a vector of
  thunks, each thunk applying the actions to one match, instead."
  [rule-app]
  `(binding [*as-test* true]
     ~rule-app))

(defn ^:private unrecur
  "Replaces (recur ...) forms with (fnname ...) forms where *as-test* is bound to false.
  Existing (fnname ...) forms are also wrapped by bindings of *as-test* to
  false.  Doesn't replace in nested `loop` or `fn` forms."
  [fnname form]
  (u/prewalk (fn [el]
               (if (and (seq? el)
                        (or (= (first el) 'recur)
                            (= (first el) fnname)))
                 `(binding [*as-test* false]
                    (~fnname ~@(next el)))
                 el))
             (fn [el]
               (and (seq? el)
                    (let [x (first el)]
                      (or (= x `clojure.core/loop)
                          (= x `clojure.core/fn)))))
             form))

;; Refer the private var in pmatch...
(def ^:private create-recheck-pattern #'pm/create-recheck-pattern)

(defn ^:private convert-spec
  "spec is ([args] [pattern] & body) or ([args] & body)."
  [name spec]
  (let [args (first spec)
        more (next spec)]
    (if (vector? (first more))
      ;; pattern vector given
      (let [pattern-vector (first more)
            bf          (@#'pm/transform-pattern-spec name pattern-vector args)
            matchsyms   (@#'pm/bindings-to-argvec bf)
            body        (next more)
            pattern     (gensym "pattern")
            matches     (gensym "matches")
            match       (gensym "match")
            recheck-pattern (gensym "recheck-pattern")
            as-test-thunk-name (symbol (str name "-as-test-thunk"))
            emit-actions (fn emit-actions [match]
                           `(let [{:keys ~matchsyms} ~match]
                              (if *as-test*
                                (with-meta (fn ~as-test-thunk-name
                                             ([]
                                              ~(if (:recheck (meta name))
                                                 `(when (seq (~recheck-pattern ~(first args) ~@matchsyms))
                                                    ~@(unrecur name body))
                                                 `(do ~@(unrecur name body))))
                                             ;; One may also specify a
                                             ;; different match than what this
                                             ;; thunk was actually meant for
                                             ;; (for interactive-rule).
                                             ([match#]
                                              (let [{:keys ~matchsyms} match#]
                                                ~(if (:recheck (meta name))
                                                   `(when (seq (~recheck-pattern ~(first args) ~@matchsyms))
                                                      ~@(unrecur name body))
                                                   `(do ~@(unrecur name body))))))
                                  {:match ~match
                                   :args ~args
                                   :all-matches ~matches})
                                ~(if (and (:recheck (meta name))
                                          (:forall (meta name)))
                                   `(when (seq (~recheck-pattern ~(first args)
                                                                 ~@matchsyms))
                                      (do ~@body))
                                   `(do ~@body)))))]
        (when (:as (meta bf))
          (u/error "Rule patterns mustn't have :as clause."))
        `(~args
          (let [~pattern (pm/pattern ~(or name (gensym "anon-pattern"))
                                     ;; forall rules can benefit from parallel
                                     ;; pattern evaluation.
                                     ~(assoc (meta name) :eager (:forall (meta name)))
                                     ~args ~pattern-vector)
                ~@(when (:recheck (meta name))
                    [recheck-pattern `(pm/pattern ~(with-meta (gensym (str (or name "anon")
                                                                           "-recheck-pattern"))
                                                     (meta name))
                                                  ;; A recheck pattern is never eager
                                                  ~(assoc (meta name) :eager false)
                                                  [~(first args) ~@matchsyms]
                                                  ~(create-recheck-pattern pattern-vector))])
                ~matches (apply ~pattern ~args)]
            (if *as-pattern*
              ~matches
              (when (seq ~matches)
                ~(if (:forall (meta name))
                   (if (:no-result-vec (meta name))
                     `(loop [i# 0, ~matches ~matches]
                        (if (seq ~matches)
                          (do ~(emit-actions `(first ~matches))
                              (recur (inc i#) (rest ~matches)))
                          i#))
                     `(mapv (fn [~match] ~(emit-actions match)) ~matches))
                   (emit-actions `(first ~matches))))))))
      ;; No pattern given
      `(~args
        (cond
          *as-pattern* (u/errorf "Can't apply rule %s without pattern as pattern!" name)
          *as-test*    (fn [] ~@(unrecur name more))
          :else        (do ~@more))))))

(defmacro rule
  "Defines an anonymous rule.  Stands to `defrule` (which see) in the same way
  as `fn` stands to `defn`.  Also see `letrule`."
  {:arglists '([name? attr-map? [args] [pattern] & body]
               [name? attr-map? ([args] [pattern] & body)+])}
  [& more]
  (let [[name more] (if (symbol? (first more))
                      [(first more) (next more)]
                      [nil more])
        [name more] (m/name-with-attributes (or name (gensym "anon-rule")) more)
        name (vary-meta name assoc :funnyqt.pmatch/pattern-specs (@#'pm/extract-pattern-specs more))
        name (vary-meta name assoc :pattern-expansion-context (@#'pm/pattern-expansion-context name))]
    `(fn ~name
       ~@(if (vector? (first more))
           ;; starts with argvec, so just one def
           (convert-spec name more)
           (mapv (partial convert-spec name) more)))))

(defmacro letrule
  "Establishes local rules just like `letfn` establishes local fns.
  Also see `rule` and `defrule`."
  {:arglists '([[rspecs] attr-map? & body])}
  [rspecs & attr-map-body]
  (when-not (vector? rspecs)
    (u/errorf "No rspecs vector in letrule!"))
  (let [[attr-map body] (if (map? (first attr-map-body))
                          [(first attr-map-body) (next attr-map-body)]
                          [{} attr-map-body])
        names-with-specs (apply hash-map (mapcat (fn [[n & more]]
                                                   [n (@#'pm/extract-pattern-specs more)])
                                                 rspecs))
        attr-map (assoc attr-map :funnyqt.pmatch/letpattern-pattern-specs names-with-specs)
        attr-map (assoc attr-map :pattern-expansion-context (or (:pattern-expansion-context attr-map)
                                                                (@#'pm/pattern-expansion-context nil)))]
    `(letfn [~@(map (fn [[n & more]]
                      (let [n (vary-meta n merge attr-map (meta n))
                            [n more] (m/name-with-attributes n more)
                            n (vary-meta n assoc :funnyqt.pmatch/pattern-specs (get names-with-specs n))]
                        `(~n ~@(if (vector? (first more))
                                 (convert-spec n more)
                                 (mapv (partial convert-spec n) more)))))
                 rspecs)]
       ~@body)))

(defmacro defrule
  "Defines a rule with `name`, optional doc-string', optional `attr-map?',
  an `args` vector, an optional `pattern` vector, and following `body` code.
  Just like `defn`, overloading is supported as well.  The `pattern` vector is
  optional.  If no version has it, then you should use `defn` directly.

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

  The `pattern` may include every pattern feature of funnyqt.pmatch/defpattern
  with the exception of :as clauses.  It may also have :extends clauses
  extending the patterns of other rules (only the other rule's pattern is
  extended; not the other rule's actions), or of another arity of the extending
  rule.

  The `body` may contain arbitrary code acting upon `args` and the elements
  matched by `pattern`.

  Rules expand to plain Clojure functions.  When a rule gets applied, it tries
  to find a match.  If it can't find one, it returns logical false.  If it
  finds one, it applies its `body` on the match returning the value of the last
  form in `body`, which should be logical true by convention.

  In general, rules consider the same options as patterns.

  Rules may have ^:forall metadata attached to their name or the :forall option
  set in the attr-map.  Such a rule first finds all matches eagerly (i.e., the
  underlying pattern has the :eager option set automatically), and then applies
  the actions to each match in sequence.  The finding of matches is done in
  parallel (if some constraints hold, and there's no ^:sequential metadata).
  The result of a ^:forall rule is the vector of action results (one for each
  match).  If you're not interested in that and want to save some memory, you
  can also add ^:no-result-vec metadata.  In that case, applying the ^:forall
  rule returns only the number of matches.  In any case, if there were no
  matches at all, nil is returned.

  Note that in a :forall rule, applying the actions on some match might
  invalidate a later match.  If that is feasible, enable the :recheck option
  either in attr-map or as metadata.  Then the pattern is rechecked before
  applying the actions.

  The same holds for applying a rule using `as-test'.  Here, the matching is
  performed at the call but you might apply the returned action thunk some time
  in the future when the model has already changed.  If that's the case in your
  scenario, again :recheck will enable rechecking the match before applying the
  actions.

  Also see `as-pattern` and `as-test`."
  {:arglists '([name doc-string? attr-map? [args] [pattern] & body]
               [name doc-string? attr-map? ([args] [pattern] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)
        pspecs (@#'pm/extract-pattern-specs more)
        name (vary-meta name assoc :funnyqt.pmatch/pattern-specs pspecs)
        name (vary-meta name assoc :pattern-expansion-context (@#'pm/pattern-expansion-context name))]
    `(defn ~name ~{:funnyqt.pmatch/pattern-specs `'~pspecs}
       ~@(if (vector? (first more))
           (convert-spec name more)
           (mapv (partial convert-spec name) more)))))

;;# Higher order rule combinators

(defn disjunctive-rule
  "Returns a varargs function which applies the first matching rule in `rules`
  to its args and returns the result of this application.  If no rule matches,
  the function returns nil."
  [& rules]
  (fn  disjunctive-rule-fn [& args]
    (loop [rs rules]
      (when (seq rs)
        (or (apply (first rs) args)
            (recur (rest rs)))))))

(defn sequential-rule
  "Returns a varargs function which applies all `rules` to its args in sequence
  collecting the individual application results in a vector.  The function
  returns this vector if at least one rule could be applied.  Else, it returns
  false."
  [& rules]
  (fn all-rule-fn [& args]
    (loop [rs rules, rets [], one-was-applied false]
      (if (seq rs)
        (let [r (apply (first rs) args)]
          (recur (rest rs) (conj rets r) (or one-was-applied r)))
        (if one-was-applied rets false)))))

(defn conjunctive-rule
  "Returns a varargs function which applies `rules` in sequence to its args
  until one rule returns logical false.  The function returns the value of the
  last rule application iff all rules could be applied.  Else, it returns
  logical false.

  Thus,            ((conjunctive-rule r1 r2 r3 r4 r5) x y)
  is equivalent to (and (r1 x y) (r2 x y) (r3 x y) (r4 x y) (r5 x y))"
  [& rules]
  (fn conjunctive-rule-fn [& args]
    (loop [rs rules, ret true]
      (if (seq rs)
        (when-let [v (apply (first rs) args)]
          (recur (rest rs) v))
        ret))))

(defn conjunctive-rule*
  "Returns a varargs function which applies `rules` in sequence until one rule
  returns logical false.  The first rule is applied to the arguments given to
  the function, all others are applied with the result of the previous rule
  application.  The function returns the value of the last rule application iff
  all rules could be applied.  Else, it returns logical false.

  Thus,            ((conjunctive-rule* r1 r2 r3 r4 r5) x y)
  is equivalent to (when-let [r (r1 x y)]
                     (when-let [r (apply r2 r)]
                       (when-let [r (apply r3 r)]
                         (when-let [r (apply r4 r)]
                           (when-let [r (apply r5 r)]
                             r)))))."
  [& rules]
  (fn conjunctive-rule*-fn [ & args]
    (loop [rs rules, ret (or args true)]
      (if (seq rs)
        (when-let [v (apply (first rs) ret)]
          (recur (rest rs) v))
        ret))))

(defn iterated-rule
  "Returns a varargs function which applies the rule `r` to the function's
  arguments as long as it returns logical true.  The function returns the
  number of successful applications or nil if it couldn't be applied at least
  once."
  [r & args]
  (fn iterated-rule-fn [& args]
    (loop [val (apply r args), i 0]
      (if val
        (recur (apply r args) (inc i))
        (when-not (zero? i) i)))))

(defn iterated-rule*
  "Returns a varargs function which applies the rule `r` as long as it returns
  logical true.  On the first application, `r` receives the arguments given to
  the function.  The second till last application receive the value of the
  previous successful application.  Returns the number of successful
  applications, or nil if it couldn't be applied at least once."
  [r]
  (fn iterated-rule*-fn [& args]
    (loop [val (apply r args), i 0]
      (if val
        (recur (apply r val) (inc i))
        (when-not (zero? i) i)))))

(defn repeated-rule
  "Returns a varargs function which applies the rule `r` at most `n` times to
  the arguments given to the function and returns the number of successfull
  applications.  Stops as soon as `r` fails.

  The version of arity one returns a function (fn [n & args] ...), i.e., the
  maximum number of repetitions has to be provided as first argument when
  calling the returned functions.

  For example ((repeated-rule 10 r) m) and ((repeated-rule r) 10 m) are
  equivalent."
  ([r]
   (fn repeated-rule-fn1 [n & args]
     (loop [i n]
       (if (and (pos? i) (apply r args))
         (recur (dec i))
         (- n i)))))
  ([n r]
   (fn repeated-rule-fn2 [& args]
     (loop [i n]
       (if (and (pos? i) (apply r args))
         (recur (dec i))
         (- n i))))))

(defn repeated-rule*
  "Returns a function which applies the rule `r` at most `n` times and then
  returns the number of successfull applications.  Stops as soon as `r` fails.
  On the first application, `r` receives `args`.  The second to last
  application receive the result of the previous application.

  The version of arity one returns a function (fn [n & args] ...), i.e., the
  maximum number of repetitions has to be provided as first argument when
  calling the returned functions.

  For example ((repeated-rule* 10 r) m) and ((repeated-rule* r) 10 m) are
  equivalent."
  ([r]
   (fn repeated-rule*-fn1 [n & args]
     (loop [i n, val args]
       (if (pos? i)
         (if-let [val (apply r val)]
           (recur (dec i) val)
           (- n i))
         (- n i)))))
  ([n r]
   (fn repeated-rule*-fn2 [& args]
     (loop [i n, val args]
       (if (pos? i)
         (if-let [val (apply r val)]
           (recur (dec i) val)
           (- n i))
         (- n i))))))

(defn random-rule
  "Returns a varargs funtion which randomly chooses one applicable rule in
  `rules` and applies it to the arguments given to the function.  The function
  returns that rule's return value or nil if no rule was applicable."
  [& rules]
  (fn [& args]
    (loop [rs (set rules)]
      (when (seq rs)
        (let [r (rand-nth rs)
              v (apply r args)]
          (or v (recur (disj rs r))))))))

(defn ^:private action ^javax.swing.Action [name f]
  (proxy [AbstractAction] [name]
    (actionPerformed [ev]
      (try
        (f)
        (catch Exception e
          (.printStackTrace e))))))

(defn match-list-cell-renderer []
  (proxy [DefaultListCellRenderer] []
    (getListCellRendererComponent [list value index isSelected cellHasFocus]
      (let [^DefaultListCellRenderer this this]
        (proxy-super getListCellRendererComponent list value index isSelected cellHasFocus)
        ;; value is a match, i.e., a map from keywords to model objects
        (.setText this (str "<html><table>"
                            (str/join "<br> "
                                      (map (fn [[k v]]
                                             (str "<tr><td><u>"
                                                  (name k)
                                                  "</u></td>"
                                                  "<td>" (-> (pr-str v)
                                                             (str/replace #"&" "&amp;")
                                                             (str/replace #"<" "&lt;")
                                                             (str/replace #">" "&gt;"))
                                                  "</td></tr>"))
                                           value))
                            "</table></html>"))
        this))))

(defn ^:private select-rule-dialog [model rule-var-thunk-tups thunkp pos posp]
  (let [d  (javax.swing.JDialog.)
        content-pane (let [^JComponent cp(.getContentPane d)]
                       (doto cp (.setBorder (BorderFactory/createEmptyBorder 3 3 3 3))))
        rule-panel (doto (JPanel.)
                     (.setBorder (BorderFactory/createTitledBorder "Applicable rules")))
        sp (JScrollPane. rule-panel)
        button-box (Box. BoxLayout/X_AXIS)
        gridbag (GridBagLayout.)
        gridbagconsts (GridBagConstraints.)]
    (letfn [(deliver-action ^javax.swing.Action [name val]
              (proxy [AbstractAction] [name]
                (actionPerformed [ev]
                  (deliver thunkp val)
                  (.dispose d))))]
      (.setTitle d "Select a rule to apply")
      ;; Deliver nil if the window is closed.
      (.addWindowListener d (proxy [java.awt.event.WindowAdapter] []
                              (windowClosed [ev]
                                (when-not (instance? java.awt.Point posp)
                                  (deliver posp (.getLocation d)))
                                (deliver thunkp nil))))
      (.setLayout content-pane (BoxLayout. content-pane BoxLayout/Y_AXIS))
      (.setLayout rule-panel gridbag)
      (.add content-pane sp)
      (.add content-pane button-box)
      (.setDefaultCloseOperation d WindowConstants/DISPOSE_ON_CLOSE)
      ;; The rule panel
      (doto (javax.swing.ToolTipManager/sharedInstance)
        (.setEnabled true))
      (set! (.fill gridbagconsts) GridBagConstraints/BOTH)
      (doseq [[rule thunk] rule-var-thunk-tups
              :let [label (JLabel. (str (u/fn-name rule) ": "))
                    cb (doto (JComboBox. (to-array (:all-matches (meta thunk))))
                         (.setRenderer (match-list-cell-renderer)))
                    current-match (atom (:match (meta thunk)))
                    flatten-match (fn [m]
                                    (cond
                                      (map? m) (vals m)
                                      (not (instance? java.util.Collection m)) [m]
                                      :else m))
                    show-match-fn (fn smf
                                    ([] (smf :gtk))
                                    ([file]
                                     (let [els (concat (:args (meta thunk))
                                                       (flatten-match @current-match))]
                                       (viz/print-model
                                        model file :mark els
                                        :include (let [nodes (filter g/element? els)]
                                                   (concat nodes
                                                           (mapcat g/neighbors nodes)))))))
                    viewb (JButton. (action "Show match" show-match-fn))
                    applyb (JButton. ^Action (deliver-action "Apply rule"
                                                             (fn []
                                                               (thunk @current-match))))
                    tmpfile (java.io.File/createTempFile "funnyqt-match-tooltip" ".png")
                    tooltip! (fn []
                               (show-match-fn (.getPath tmpfile))
                               (.setToolTipText
                                cb
                                (str "<html><img src=\"file://"
                                     (.getPath tmpfile)
                                     "\"></html>")))]]
        (.addItemListener cb (reify ItemListener
                               (itemStateChanged [this ev]
                                 (when (== (.getStateChange ev) ItemEvent/SELECTED)
                                   (reset! current-match (.getItem ev))
                                   (tooltip!)))))
        (tooltip!)
        (.add rule-panel label)
        (set! (.gridwidth gridbagconsts) GridBagConstraints/LINE_START)
        (.setConstraints gridbag label gridbagconsts)
        (.add rule-panel cb)
        (set! (.gridwidth gridbagconsts) 1)
        (.setConstraints gridbag cb gridbagconsts)
        (.add rule-panel viewb)
        (.setConstraints gridbag viewb gridbagconsts)
        (.add rule-panel applyb)
        (set! (.gridwidth gridbagconsts) GridBagConstraints/REMAINDER)
        (.setConstraints gridbag applyb gridbagconsts))
      ;; The button-box
      (.add button-box (JButton. (action "View model"
                                         #(viz/print-model model :gtk))))
      (.add button-box (JButton. ^Action (deliver-action "Done" nil)))
      (.pack d)
      (if pos
        (.setLocation d pos)
        (.setLocationRelativeTo d nil))
      (.setVisible d true))))

(defn interactive-rule
  "Returns a function which interactively applies the given `rules` to the
  returned function's arguments."
  [& rules]
  (fn [& args]
    (let [model (first args)]
      (loop [pos nil, posp (promise), rules-applied 0]
        (let [rule-thunk-tups (mapcat
                               (fn [r]
                                 (when-let [thunk (as-test (apply r args))]
                                   [[r thunk]]))
                               rules)
              t (promise)]
          (if (seq rule-thunk-tups)
            (do
              (select-rule-dialog model rule-thunk-tups t pos posp)
              (if-let [thunk @t]
                (let [pos @posp]
                  (thunk)
                  (recur pos (promise) (inc rules-applied)))
                (if (zero? rules-applied) nil rules-applied)))
            (println "None of the rules is applicable.")))))))


;;# State Space Exploration/Generation


(def ^:private statespace-schema (tg/load-schema "state-space-schema.tg"))

(defn state-space-step-fn
  "Creates and returns a function for step-wise state space creation.
  The state space exploration uses `init-model` as initial model/state, applies
  `rules` in each state, and compares the models corresponding to states using
  `comparefn` (usually `funnyqt.generic/equal-models?` is a good `comparefn`).

  An optional map of further options may be given to `state-space-step-fn`.

    {:additional-args   <seq of additional args for rule application>
     :select-rules-fn   (fn [rules] ...)
     :select-state-fn   (fn [undone-states] ...)
     :state-preds       <seq of predicates on each state's model>
     :transition-preds  <map of rule postconditions>
     :state-space-preds <seq of predicates on the states space graph>}

  The returned step-function is overloaded on arity and accepts these
  arguments:

    []                                  ;; arity zero
    [select-rules-fn]                   ;; arity one
    [select-rules-fn select-state-fn]   ;; arity two

  The two arguments may override the values given in the map of options.

  During each step, first the rules to be applied in this step are computed
  using (select-rules-fn rules).  The default select-rules-fn is
  clojure.core/identity.  If rules should be applied in a random order, use
  clojure.core/shuffle as select-rules-fn.

  Then, the state to whose corresponding model the rules are going to be
  applied is selected by calling (select-state-fn undone-states) where
  undone-states is the sequence of states to which not all rules have been
  applied yet.  The default select-state-fn is clojure.core/first.

  Then, each selected rule is applied using

    (apply rule model-of-a-state additional-args)

  so the :additional-args entry is for specifying further arguments to the
  rules in addition to the model corresponding to a state.

  The other three entries in the map of options allow for specifying validation
  predicates.

  - The :state-preds value is a sequence of predicates that receive a state's
    model.  If any predicate returns logical false, the state of that model is
    an InvalidState.  Else, it is a ValidState.  Thus, use this entry for
    specifying invariants for the model under transformation.

  - The :transition-preds value is a map of the form {rule [pred1 pred2], ...}.
    When rule is executed to the model of a state, each predicate of this rule
    is called like

       (pred old-model match new-model).

    If that returns logical false, the transition from the state of old-model
    to the state of new-model is an InvalidTransition.  Else, it is a
    ValidTransition.  Thus, use this entry to specify postconditions of rules.

  - The :state-space-preds value is a sequence of predicates that receive the
    current state space graph.  Use this entry to specify invariants of the
    state space graph itself.  For example, the number of states might
    be bounded for some transformation, thus an invariant might want to check
    that (<= (vcount ssg) upper-bound).  If that invariant fails, you
    probably have a bug in your implementation or you use a too strict
    `comparefn` which considers models different although they differ only
    in properties which are not important for the case at hand, e.g., the
    models might differ only in the order of references.

  Applying the step-function returns false if no rule could be applied, i.e.,
  if the `select-state-fn` couldn't selected some state.  Else, it returns a
  vector of the form

    [seq-of-invalid-states
     seq-of-invalid-transitions
     seq-of-failed-state-space-preds]

  where seq-of-invalid-states is the sequence of InvalidState vertices in the
  state space graph, seq-of-invalid-transitions is the sequence of
  InvalidTransition edges, and seq-of-failed-state-space-preds is the sequence
  of `state-space-preds` which failed for the current state space graph.

  The returned step-function has the following metadata:

    {:state-space-graph the-state-space-graph
     :state2model-map volatile-state2model-map}

  So this metadata is where you extract the states space graph and the map from
  states to corresponding models.  The latter is a volatile so needs to be
  dereferenced (e.g., @volatile-state2model-map or using `clojure.core/deref`)
  in orded to obtain its current value."
  ([init-model comparefn rules]
   (state-space-step-fn init-model comparefn rules {}))
  ([init-model comparefn rules {:keys [additional-args
                                       select-state-fn
                                       select-rules-fn
                                       state-preds
                                       transition-preds
                                       state-space-preds]
                                :or {select-state-fn first
                                     select-rules-fn identity}
                                :as options}]
   (let [unknown-opts (dissoc options :additional-args :state-preds
                              :transition-preds :state-space-preds)]
     (when (seq unknown-opts)
       (u/errorf "Unknown options: %s" unknown-opts)))
   (when (and transition-preds (not (map? transition-preds)))
     (u/errorf "transition-preds must be a map but was %s" transition-preds))
   (let [ssg (tg/new-graph statespace-schema)
         failed-state-preds (fn [m]
                              (for [p state-preds
                                    :when (not (p m))]
                                p))
         failed-transition-preds (fn [r old-m match new-m]
                                   (when transition-preds
                                     (for [p (transition-preds r)
                                           :when (not (p old-m match new-m))]
                                       p)))
         failed-state-space-preds (fn []
                                    (doall
                                     (for [p state-space-preds
                                           :when (not (p ssg))]
                                       p)))
         create-state! (fn create-new-state! [m]
                         (let [failed (failed-state-preds m)]
                           (if (seq failed)
                             (tg/create-vertex! ssg 'InvalidState
                                                {:n (inc (tg/vcount ssg))
                                                 :failed (set (map u/fn-name failed))})
                             (tg/create-vertex! ssg 'ValidState
                                                {:n (inc (tg/vcount ssg))}))))
         state2model (volatile! {(create-state! init-model) init-model})
         find-equiv-state (fn [m]
                            (first (filter #(comparefn m (@state2model %))
                                           (tg/vseq ssg 'State))))
         invalid-state-tm (g/type-matcher ssg 'InvalidState)
         invalid-transition-tm (g/type-matcher ssg 'InvalidTransition)]
     (with-meta (fn do-step
                  ([]
                   (do-step select-rules-fn select-state-fn))
                  ([select-rules-fn]
                   (do-step select-rules-fn select-state-fn))
                  ([select-rules-fn select-state-fn]
                   (let [rules (select-rules-fn rules)]
                     (if-let [st (select-state-fn
                                  (remove #(.containsAll
                                            ^java.util.Set (tg/value % :done)
                                            (map u/fn-name rules))
                                          (tg/vseq ssg 'State)))]
                       (do
                         (doseq [r rules
                                 :let [m (g/copy-model (@state2model st))]]
                           (when-let [thunk (as-test (apply r m additional-args))]
                             (thunk)
                             (let [nst (or (find-equiv-state m)
                                           (create-state! m))]
                               (if-let [failed-posts (seq (failed-transition-preds
                                                           r
                                                           (@state2model st)
                                                           (:match (meta thunk))
                                                           m))]
                                 (tg/create-edge! ssg 'InvalidTransition st nst
                                                  {:rule (u/fn-name r)
                                                   :failed (set (map u/fn-name failed-posts))})
                                 (tg/create-edge! ssg 'ValidTransition st nst {:rule (u/fn-name r)}))
                               (when-not (contains? @state2model nst)
                                 (vswap! state2model assoc nst m))))
                           (tg/set-value! st :done (.plus
                                                    ^org.pcollections.PSet (tg/value st :done)
                                                    (u/fn-name r))))
                         [(tg/vseq ssg invalid-state-tm)
                          (tg/eseq ssg invalid-transition-tm)
                          (failed-state-space-preds)])
                       false))))
       {:state-space-graph ssg
        :state2model-map state2model}))))

(defn create-state-space
  "Takes the `model` as initial state and applies all `rules` to it generating
  a state space graph.

  An optional map of options may be given.  For a description of arguments and
  options except for :recur-pred, see `funnyqt.in-place/state-space-step-fn`.

  If the map of options contains a :recur-pred entry, its value must be a
  function of five arguments which determines if the state space generation
  should be resumed.  It's signature should be:

    (fn [ssg state2model-map
         invalid-states invalid-transitions failed-state-space-preds]
      ...)

  If the recur-pred returns logical true, the state space generation is
  resumed.  By default, i.e., when there is no :recur-pred entry in the map of
  options, `create-state-space` returns as soon as at least one predicate of
  `state-preds`, `transition-preds`, or `state-space-preds` fails.

  The return value has the form

    [state-space-graph state2model-map step-fn-retval]

  where state-space-graph is the final state space graph, state2model-map is
  the final map from SSG states to corresponding models, and step-fn-retval
  is the return value of the underlying `state-space-step-fn`.

  This vector also has :state-space-step-fn metadata which is the state space
  step function used internally by create-state-space.  This allows for driving
  the state space generation further after create-state-space has returned
  because of a failed predicate or a user-defined recur-pred."
  ([model comparefn rules]
   (create-state-space model comparefn rules {}))
  ([model comparefn rules options]
   (let [sss-fn (state-space-step-fn model comparefn rules (dissoc options :recur-pred))
         recur-pred (or (:recur-pred options)
                        (fn default-recur-pred [ssg s2m-map inv-states inv-transitions failed-ssg-preds]
                          ;; Apply as long as sss-fn returns [() () #{}], i.e.,
                          ;; stop if no new states can be created, or an
                          ;; invalid state or transition has been created, or
                          ;; the state space has become invalid.
                          (and (empty? inv-states)
                               (empty? inv-transitions)
                               (empty? failed-ssg-preds))))
         ssg (:state-space-graph (meta sss-fn))
         s2m-map-v (:state2model-map (meta sss-fn))]
     (loop [ret (sss-fn)]
       (if (and ret (apply recur-pred ssg @s2m-map-v ret))
         (recur (sss-fn))
         (with-meta [ssg @s2m-map-v ret]
           {:state-space-step-fn sss-fn}))))))

(defn ^:private explore-state-space-dialog [sss-fn rules]
  (let [ssg (:state-space-graph (meta sss-fn))
        s2m (:state2model-map (meta sss-fn))
        CHECK16 (ImageIcon. (ClassLoader/getSystemResource "check16.png"))
        CROSS16 (ImageIcon. (ClassLoader/getSystemResource "cross16.png"))
        state (fn [n]
                (first (filter #(= n (tg/value % :n))
                               (tg/vseq ssg 'State))))
        state-model (fn [n]
                      (@s2m (state n)))
        d (javax.swing.JDialog.)
        valid-state-tm (g/type-matcher ssg 'ValidState)
        valid-state? (fn [i] (valid-state-tm (tg/vertex ssg i)))
        cb-renderer (proxy [javax.swing.plaf.basic.BasicComboBoxRenderer] []
                      (getListCellRendererComponent [list value index isSelected cellHasFocus]
                        (let [^javax.swing.plaf.basic.BasicComboBoxRenderer$UIResource this this
                              ^JLabel default (proxy-super getListCellRendererComponent
                                                           list value index isSelected
                                                           cellHasFocus)]
                          (.setHorizontalAlignment default JLabel/CENTER)
                          (cond
                            (nil? value) (do (.setIcon default nil)
                                             (.setToolTipText default "No undone states"))
                            (valid-state? value) (do (.setIcon default CHECK16)
                                                     (.setToolTipText default "Valid state"))
                            :else (let [v (tg/vertex ssg value)]
                                    (.setIcon default CROSS16)
                                    (.setToolTipText default
                                                     (str "Failed state predicates: "
                                                          (vec (tg/value v :failed))))))
                          default)))
        content-pane (let [^JComponent cp(.getContentPane d)]
                       (doto cp (.setBorder (BorderFactory/createEmptyBorder 3 3 3 3))))
        rule-select-panel (JPanel.)
        states-panel (Box. BoxLayout/Y_AXIS)
        all-states-cb (doto (JComboBox.)
                        (.setRenderer cb-renderer))
        reset-all-states-cb! (fn []
                               (.removeAllItems all-states-cb)
                               (doseq [n (map #(tg/value % :n)
                                              (tg/vseq ssg 'State))]
                                 (.addItem all-states-cb n)))
        undone-states-cb (doto (JComboBox.)
                           (.setRenderer cb-renderer))
        button-panel (Box. BoxLayout/X_AXIS)
        select-rules-fn-promise (promise)
        apply-rules-button-promise (promise)
        reset-undone-states-cb! (fn reset-undone-states-cb! []
                                  (.removeAllItems undone-states-cb)
                                  (let [undone (map #(tg/value % :n)
                                                    (remove #(.containsAll
                                                              ^java.util.Set (tg/value % :done)
                                                              (map u/fn-name
                                                                   (@select-rules-fn-promise :ignored)))
                                                            (tg/vseq ssg 'State)))]
                                    (doseq [n undone]
                                      (.addItem undone-states-cb n))
                                    (.setEnabled ^JButton @apply-rules-button-promise
                                                 (pos? (count undone)))))
        rule-check-boxes (for [r rules]
                           (let [cb (JCheckBox. (let [a (action (u/fn-name r)
                                                                reset-undone-states-cb!)]
                                                  (.putValue a "rule" r)
                                                  a))]
                             (doto cb (.setSelected true))))
        show-done-attr-checkbox (JCheckBox. "Show :done")
        state-counts (fn []
                       (let [sc (tg/vcount ssg)
                             isc (tg/vcount ssg 'InvalidState)
                             tc (tg/ecount ssg)
                             itc (tg/ecount ssg 'InvalidTransition)]
                         ;; total, valid, invalid
                         [sc (- sc isc) isc
                          tc (- tc itc) itc]))
        no-of-states-label (JLabel.)
        no-of-transitions-label (JLabel.)
        no-of-valid-states-label (JLabel.)
        no-of-invalid-states-label (JLabel.)
        no-of-valid-transitions-label (JLabel.)
        no-of-invalid-transitions-label (JLabel.)
        no-of-invalid-states-label (JLabel.)
        state-space-valid-label (doto (JLabel.)
                                  (.setText "yes")
                                  (.setIcon CHECK16))
        reset-state-space-valid-label! (fn [failed-ssg-preds]
                                         (if (seq failed-ssg-preds)
                                           (doto state-space-valid-label
                                             (.setText "no")
                                             (.setIcon CROSS16)
                                             (.setToolTipText
                                              (str "Failed SSG predicates: "
                                                   (mapv u/fn-name failed-ssg-preds))))
                                           (doto state-space-valid-label
                                             (.setText "yes")
                                             (.setIcon CHECK16)
                                             (.setToolTipText
                                              "All state space predicates pass."))))
        reset-state-counts-labels! (fn []
                                     (let [[as vs is at vt it] (state-counts)]
                                       (.setText no-of-states-label (str as))
                                       (.setText no-of-transitions-label (str at))
                                       (.setText no-of-valid-states-label (str vs))
                                       (.setText no-of-invalid-states-label (str is))
                                       (.setText no-of-valid-transitions-label (str vt))
                                       (.setText no-of-invalid-transitions-label (str it))
                                       (.setText no-of-invalid-states-label (str is))))]
    (deliver select-rules-fn-promise (fn select-rules-fn [_]
                                       (for [^JCheckBox cb rule-check-boxes
                                             :when (.isSelected cb)]
                                         (.getValue (.getAction cb) "rule"))))
    (deliver apply-rules-button-promise
             (JButton. (action "Apply"
                               (fn []
                                 (let [n (.getSelectedItem undone-states-cb)
                                       ret (sss-fn @select-rules-fn-promise
                                                   (constantly (state n)))]
                                   (when ret
                                     (let [[_ _ failed-ssg-preds] ret]
                                       (reset-state-space-valid-label! failed-ssg-preds)))
                                   (reset-undone-states-cb!)
                                   (reset-all-states-cb!)
                                   (reset-state-counts-labels!))))))
    (.setTitle d "State Space Explorer")
    (.setDefaultCloseOperation d WindowConstants/DISPOSE_ON_CLOSE)
    (doto (javax.swing.ToolTipManager/sharedInstance)
      (.setEnabled true))
    (reset-all-states-cb!)
    (reset-undone-states-cb!)
    (reset-state-counts-labels!)
    (.setLayout content-pane (BoxLayout. content-pane BoxLayout/Y_AXIS))

    (.setBorder states-panel (BorderFactory/createTitledBorder "State Selection"))
    (let [upper (JPanel.)]
      (.setLayout upper (GridLayout. 2 3))
      (.add upper (JLabel. "All States:"))
      (.add upper all-states-cb)
      (.add upper (JButton. (action "View Model"
                                    #(viz/print-model
                                      (state-model
                                       (.getSelectedItem all-states-cb))
                                      :gtk))))
      (.add upper (JLabel. "Undone States:"))
      (.add upper undone-states-cb)
      (.add upper ^JButton @apply-rules-button-promise)
      (.add states-panel upper))

    (let [lower (Box. BoxLayout/X_AXIS)]
      (.add lower (doto (JPanel.)
                    (.setBorder (BorderFactory/createTitledBorder "# All S/T"))
                    (.add no-of-states-label)
                    (.add (JLabel. " / "))
                    (.add no-of-transitions-label)))
      (.add lower (doto (JPanel.)
                    (.setBorder (BorderFactory/createTitledBorder "# Valid S/T"))
                    (.add no-of-valid-states-label)
                    (.add (JLabel. " / "))
                    (.add no-of-valid-transitions-label)))
      (.add lower (doto (JPanel.)
                    (.setBorder (BorderFactory/createTitledBorder "# Invalid S/T"))
                    (.add no-of-invalid-states-label)
                    (.add (JLabel. " / "))
                    (.add no-of-invalid-transitions-label)))
      (.add lower (doto (JPanel.)
                    (.setBorder (BorderFactory/createTitledBorder
                                 "SSG valid?"))
                    (.add  state-space-valid-label)))
      (.add states-panel lower))

    (.setLayout rule-select-panel (GridLayout. (let [rc (count rules)]
                                                 (if (odd? rc)
                                                   (inc (quot rc 2))
                                                   (quot rc 2)))
                                               2))
    (.setBorder rule-select-panel (BorderFactory/createTitledBorder "Rule Selection"))
    (doseq [^JCheckBox rcb rule-check-boxes]
      (.add rule-select-panel rcb))

    (.add button-panel (Box/createHorizontalStrut 15))
    (.add button-panel show-done-attr-checkbox)
    (.add button-panel (Box/createHorizontalStrut 15))
    (.add button-panel (JButton.
                        (action "View State Space Graph"
                                #(apply
                                  viz/print-model
                                  ssg :gtk
                                  :node-attrs {(g/type-matcher ssg 'ValidState)
                                               "style=filled, fillcolor=green"
                                               (g/type-matcher ssg 'InvalidState)
                                               "style=filled, fillcolor=red"}
                                  :edge-attrs {(g/type-matcher ssg 'InvalidTransition)
                                               "color=red"}
                                  (when-not (.isSelected show-done-attr-checkbox)
                                    (list :excluded-attributes
                                          {(g/type-matcher ssg 'State) [:done]}))))))
    (.add button-panel (Box/createHorizontalGlue))
    (.add button-panel (JButton. (action "Done" #(.dispose d))))

    (.add content-pane states-panel)
    (.add content-pane rule-select-panel)
    (.add content-pane button-panel)
    (.pack d)
    (.setVisible d true)
    d))

(defn explore-state-space
  "Fires up a GUI that allows for creating and inspecting the state space by
  starting with initial model `model`, the given `comparefn` (see, e.g.,
  `funnyqt.generic/equal-models?`), and the given `rules`.  For a description
  of arguments, see `funnyqt.in-place/state-space-step-fn`.
  explore-state-space returns a vector of the form [ssg s2m-map] where ssg is
  the state space graph which has been built interactively, and s2m-map is the
  map from State vertices to the corresponding models.  This vector
  has :state-space-step-fn metadata attached whose value is the step-wise state
  space generation function used internally."
  ([model comparefn rules]
   (explore-state-space model comparefn rules {}))
  ([model comparefn rules options]
   (let [sss-fn (state-space-step-fn model comparefn rules options)
         ssg (:state-space-graph (meta sss-fn))
         s2m-map-v (:state2model-map (meta sss-fn))]
     (explore-state-space-dialog sss-fn rules)
     (with-meta [ssg @s2m-map-v]
       {:state-space-step-fn sss-fn}))))
