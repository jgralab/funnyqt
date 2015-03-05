(ns funnyqt.in-place
  "In-place transformation stuff."
  (:require [clojure.tools.macro   :as m]
            [funnyqt.generic       :as g]
            [funnyqt.visualization :as viz]
            [funnyqt.utils         :as u]
            [funnyqt.query         :as q]
            [funnyqt.pmatch        :as pm]
            [funnyqt.tg            :as tg])
  (:import
   (javax.swing JDialog JButton AbstractAction WindowConstants BoxLayout
                JPanel JLabel JScrollPane JComboBox Action JCheckBox
                BorderFactory)
   (java.awt.event ActionEvent ItemEvent ItemListener)
   (java.awt GridLayout GridBagLayout GridBagConstraints)))


;;# Rules

(def ^{:dynamic true
       :doc "A function that is invoked when a rule matches,
  mainly for debugging purposes.
  The function gets the following arguments: [r args match]

    - r is a symbol denoting the current matching rule
    - args is the vector of the rule's input arguments
    - match is the current match found by the rule"}
  *on-matched-rule-fn* nil)

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

(defn ^:private convert-spec
  "spec is ([args] [pattern] & body) or ([args] & body)."
  [name spec]
  (let [args (first spec)
        more (next spec)]
    (if (vector? (first more))
      ;; pattern vector given
      (let [pattern-vector (first more)
            bf          (@#'pm/transform-pattern-spec name pattern-vector args)
            custom-as   (:as (meta bf))
            matchsyms   (pm/bindings-to-argvec bf)
            pattern-vector (if custom-as
                             pattern-vector
                             (conj pattern-vector :as matchsyms))
            body        (next more)
            pattern     (gensym "pattern")
            matches     (gensym "matches")
            action-fn   (gensym "action-fn")
            match       (gensym "match")]
        (when-not (= custom-as (u/deep-vectorify custom-as))
          (u/errorf "The :as clause in patterns of in-plate rules must be a vector but was %s."
                    custom-as))
        `(~args
          (let [~pattern (pm/pattern ~(or name (gensym "anon-pattern"))
                                     ;; forall rules can benefit from parallel
                                     ;; pattern evaluation.
                                     {:eager ~(:forall (meta name))
                                      :sequential ~(:sequential (meta name))}
                                     ~args ~pattern-vector)
                ~matches (apply ~pattern ~args)
                ~action-fn (fn [~match]
                             (let [~matchsyms ~match]
                               (when *on-matched-rule-fn*
                                 (*on-matched-rule-fn* '~name ~args ~match))
                               (if *as-test*
                                 (let [curmatch# (atom ~matchsyms)]
                                   (with-meta (fn []
                                                (let [~matchsyms @curmatch#]
                                                  ~@(unrecur name body)))
                                     {:current-match-atom curmatch#
                                      :args ~args
                                      :all-matches ~matches}))
                                 (do ~@body))))]
            (if *as-pattern*
              ~matches
              (when (seq ~matches)
                ~(if (:forall (meta name))
                   (if (:no-result-vec (meta name))
                     `(loop [i# 0, matches# ~matches]
                        (if (seq matches#)
                          (do (~action-fn (first matches#))
                              (recur (inc i#) (rest matches#)))
                          i#))
                     `(mapv ~action-fn ~matches))
                   `(~action-fn (first ~matches))))))))
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
        [name more] (m/name-with-attributes (or name (gensym "anon-rule")) more)]
    (binding [pm/*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                                 (:pattern-expansion-context (meta *ns*))
                                                 pm/*pattern-expansion-context*)]
      `(fn ~@(when name [name])
         ~@(if (vector? (first more))
             ;; starts with argvec, so just one def
             (convert-spec name more)
             (mapv (partial convert-spec name) more))))))

(defmacro letrule
  "Establishes local rules just like `letfn` establishes local fns.
  Also see `rule` and `defrule`."
  {:arglists '([[rspecs] & body])}
  [rspecs & body]
  (when-not (vector? rspecs)
    (u/errorf "No rspec vector in letmapping!"))
  (binding [pm/*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                               (:pattern-expansion-context (meta *ns*))
                                               pm/*pattern-expansion-context*)]
    `(letfn [~@(map (fn [[n & more]]
                      `(~n ~@(if (vector? (first more))
                               (convert-spec n more)
                               (mapv (partial convert-spec n) more))))
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

  The `body` may contain arbitrary code acting upon `args` and the elements
  matched by `pattern`.

  Rules expand to plain Clojure functions.  When a rule gets applied, it tries
  to find a match.  If it can't find one, it returns logical false.  If it
  finds one, it applies its `body` on the match returning the value of the last
  form in `body`, which should be logical true by convention.

  Rules may have ^:forall metadata attached to their name.  Such a rule first
  finds all matches eagerly, and then applies the actions to each match in
  sequence.  The finding of matches is done in parallel (if some constraints
  hold, and there's no ^:sequential metadata).  The result of a ^:forall rule
  is the vector of action results (one for each match).  If you're not
  interested in that and want to save some memory, you can also add
  ^:no-result-vec metadata.  In that case, applying the ^:forall rule returns
  only the number of matches.  In any case, if there were no matches at all,
  nil is returned.

  When a rule matches, *on-matched-rule-fn* is invoked which you can use to
  inspect matches (i.e., for debugging).

  Also see `as-pattern` and `as-test`."
  {:arglists '([name doc-string? attr-map? [args] [pattern] & body]
               [name doc-string? attr-map? ([args] [pattern] & body)+])}
  [name & more]
  (let [[name more] (m/name-with-attributes name more)]
    (binding [pm/*pattern-expansion-context* (or (:pattern-expansion-context (meta name))
                                                 (:pattern-expansion-context (meta *ns*))
                                                 pm/*pattern-expansion-context*)]
      `(defn ~name ~(meta name)
         ~@(if (vector? (first more))
             (convert-spec name more)
             (mapv (partial convert-spec name) more))))))

;;# Higher order rule application functions

(defn apply-disjunctively
  "Applies the first matching rule in `rules` with `args` and returns its
  result.  If no rule matches, returns nil."
  [rules & args]
  (loop [rs rules]
    (when (seq rs)
      (or (apply (first rs) args)
          (recur (rest rs))))))

(defn apply-all
  "Applies all `rules` with `args` in sequence and returns the value of
  applying the given `combfn` to the results of all applications.

  Useful combining functions are `and*`, `or*`, `nand*`, `nor*` and `xor*`
  defined in the funnyqt.query namespace."
  [rules combfn & args]
  (loop [rs rules, rets []]
    (if (seq rs)
      (let [r (apply (first rs) args)]
        (recur (rest rs) (conj rets r)))
      (apply combfn rets))))

(defn apply-all:and
  "Applies all `rules` with `args` and returns logical true iff all rules could
  be applied.  This is identical to `apply-all` with `funnyqt.query/and*` as
  combfn."
  [rules & args]
  (apply-all rules q/and* args))

(defn apply-all:or
  "Applies all `rules` with `args` and returns logical true iff at least one
  rule could be applied.  This is identical to `apply-all` with
  `funnyqt.query/or*` as combfn."
  [rules & args]
  (apply-all rules q/or* args))

(defn apply-conjunctively
  "Applies `rules` in sequence with `args` until one rule returns logical
  false.  Returns the value of the last rule application iff all rules could be
  applied.  Else, it returns logical false.

  Thus,            (apply-conjunctively [r1 r2 r3 r4 r5] x y)
  is equivalent to (and (r1 x y) (r2 x y) (r3 x y) (r4 x y) (r5 x y))"
  [rules & args]
  (loop [rs rules, ret true]
    (if (seq rs)
      (when-let [v (apply (first rs) args)]
        (recur (rest rs) v))
      ret)))

(defn apply-conjunctively*
  "Applies `rules` in sequence until one rule returns logical false.  Returns
  the value of the last rule application iff all rules could be applied.  Else,
  it returns logical false.  The first rule is applied with `args`, all others
  are applied with the result of the previous rule application.

  Thus,            (apply-conjunctively* [r1 r2 r3 r4 r5] x y)
  is equivalent to (when-let [r (r1 x y)]
                     (when-let [r (apply r2 x)]
                       (when-let [r (apply r3 x)]
                         (when-let [r (apply r4 x)]
                           (when-let [r (apply r5 x)]
                             r)))))."
  [rules & args]
  (loop [rs rules, ret (or args true)]
    (if (seq rs)
      (when-let [v (apply (first rs) ret)]
        (recur (rest rs) v))
      ret)))

(defn apply-repeatedly
  "Applies the rule `r` with `args` as long as it returns logical true.
  Returns the number of successful applications or nil if it couldn't be
  applied at least once."
  [r & args]
  (loop [val (apply r args), i 0]
    (if val
      (recur (apply r args) (inc i))
      (when-not (zero? i) i))))

(defn apply-repeatedly*
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

(defn apply-ntimes
  "Applies the rule `r` at most `n` times and returns the number of successfull
  applications.  Stops as soon as `r` fails."
  [n r & args]
  (loop [i n]
    (if (and (pos? i) (apply r args))
      (recur (dec i))
      (- n i))))

(defn apply-ntimes*
  "Applies the rule `r` at most `n` times and returns the number of successfull
  applications.  Stops as soon as `r` fails.  On the first application, `r`
  receives `args`.  The second to last application receives the results of the
  previous application."
  [n r & args]
  (loop [i n, val args]
    (if (pos? i)
      (if-let [val (apply r val)]
        (recur (dec i) val)
        (- n i))
      (- n i))))

(defn apply-randomly
  "Randomly chooses one applicable rule in `rules` and applies it with `args`.
  Returns that rule's return value or nil if no rule was applicable."
  [rules & args]
  (loop [rs (set rules)]
    (when (seq rs)
      (let [r (rand-nth rs)
            v (apply r args)]
        (or v (recur (disj rs r)))))))

(defn ^:private select-rule-dialog [model rule-var-thunk-tups thunkp pos posp]
  (let [d  (javax.swing.JDialog.)
        content-pane (.getContentPane d)
        rp (JPanel.)
        sp (JScrollPane. rp)
        bp (JPanel.)
        gridbag (GridBagLayout.)
        gridbagconsts (GridBagConstraints.)]
    (letfn [(action ^Action [name f]
              (proxy [AbstractAction] [name]
                (actionPerformed [ev]
                  (f))))
            (deliver-action ^Action [name val]
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
      (.setLayout rp gridbag)
      (.add content-pane sp)
      (.add content-pane bp)
      (.setDefaultCloseOperation d WindowConstants/DISPOSE_ON_CLOSE)
      ;; The rule panel rp
      (set! (.gridwidth gridbagconsts) GridBagConstraints/REMAINDER)
      (doto (javax.swing.ToolTipManager/sharedInstance)
        (.setEnabled true))
      (doseq [[rvar thunk] rule-var-thunk-tups
              :let [label (JLabel. (str (:name (meta rvar)) ":"))
                    cb (JComboBox. (to-array (:all-matches (meta thunk))))
                    show-match-fn (fn smf
                                    ([] (smf :gtk))
                                    ([file]
                                     (let [els (concat (:args (meta thunk))
                                                       @(:current-match-atom (meta thunk)))]
                                       (viz/print-model
                                        model file :mark els
                                        :include (let [nodes (filter g/element? els)]
                                                   (concat nodes
                                                           (mapcat g/neighbors nodes)))))))
                    viewb (JButton. ^Action (action "Show Match" show-match-fn))
                    applyb (JButton. ^Action (deliver-action "Apply Rule" thunk))
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
                                   (reset! (:current-match-atom (meta thunk))
                                           (.getItem ev))
                                   (tooltip!)))))
        (tooltip!)
        (.add rp label)
        (.add rp cb)
        (.add rp viewb)
        (.add rp applyb)
        (.setConstraints gridbag applyb gridbagconsts))
      ;; The button rp bp
      (.add bp (JButton. ^Action (action "View model" #(viz/print-model model :gtk))))
      (.add bp (JButton. ^Action (deliver-action "Cancel" nil)))
      (.pack d)
      (if pos
        (.setLocation d pos)
        (.setLocationRelativeTo d nil))
      (.setVisible d true))))

(defn apply-interactively
  "Interactively applies the rules being the values of `rule-vars`."
  [model rule-vars & args]
  (loop [pos nil, posp (promise)]
    (let [rule-thunk-tups (mapcat (fn [rv]
                                    (when-let [thunk (as-test (apply @rv args))]
                                      [[rv thunk]]))
                                  rule-vars)
          t (promise)]
      (if (seq rule-thunk-tups)
        (do
          (select-rule-dialog model rule-thunk-tups t pos posp)
          (when-let [thunk @t]
            (let [pos @posp]
              (thunk)
              (recur pos (promise)))))
        (println "None of the rules is applicable.")))))

(def ^:private statespace-schema (tg/load-schema "state-space-schema.tg"))

(defn ^:private rule-name [r]
  (let [^String s (pr-str (class r))
        i (.lastIndexOf s (int\$))]
    (clojure.string/replace (subs s (inc i)) \_ \-)))

(defn state-space-step-fn
  "Creates and returns a function for step-wise state space exploration.
  The state space exploration uses `init-model` as initial model.  The returned
  step-function is overloaded on arity and accepts these arguments:

    []                        ;; arity zero
    [select-state-fn]         ;; arity one
    [select-state-fn rules]   ;; arity two

  `select-state-fn` is a function that receives the sequence of undone states
  in the state space graph and returns the one to which `rules` should be
  applied.

  `rules` defaults to the argument of the same name given to
  state-space-step-fn, and `select-state-fn` defaults to `clojure.core/first`.

  Applying the step-function returns true if some rule could be applied, else,
  i.e., if the `select-state-fn` couldn't selected some state, returns false.

  The step-function has the following metadata:

    {:state-space-graph the-state-space-graph
     :state2model-map volatile-state2model-map}

  So this metadata is where you extract the states space graph and the map from
  states to corresponding models.  The latter is a volatile so needs to be
  dereferenced (e.g., @volatile-state2model-map or using `clojure.core/deref`)
  in orded to obtain its current value."
  [init-model comparefn rules & additional-args]
  (let [ssg (tg/new-graph statespace-schema)
        create-state! (fn []
                        (tg/create-vertex! ssg 'State {:n (inc (tg/vcount ssg))}))
        state2model (volatile! {(create-state!) init-model})
        find-equiv-state (fn [m]
                           (first (filter #(comparefn m (@state2model %))
                                          (tg/vseq ssg 'State))))
        rule-names (set (map rule-name rules))]
    (with-meta (fn do-step
                 ([]
                  (do-step first rules))
                 ([select-state-fn]
                  (do-step select-state-fn rules))
                 ([select-state-fn rs]
                  (if-let [st (select-state-fn
                               (remove #(.containsAll
                                         ^java.util.Set (tg/value % :done)
                                         (if (identical? rs rules)
                                           rule-names
                                           (map rule-name rs)))
                                       (tg/vseq ssg 'State)))]
                    (do
                      (doseq [r rs
                              :let [m (g/copy-model (@state2model st))]]
                        (when (apply r m additional-args)
                          (let [nst (or (find-equiv-state m)
                                        (create-state!))]
                            (tg/create-edge! ssg 'Transition st nst {:rule (rule-name r)})
                            (when-not (contains? @state2model nst)
                              (vswap! state2model assoc nst m)))))
                      (tg/set-value! st :done (.plusAll
                                               ^org.pcollections.PSet (tg/value st :done)
                                               ^java.util.Collection (map rule-name rs)))
                      true)
                    false)))
      {:state-space-graph ssg
       :state2model-map state2model})))

(defn create-state-space
  "Takes the `model` as initial state and applies all `rules` to it (and
  optionally `additional-args`) possibly resulting in new states.  Old states
  are reused if a rule changes a model in such a way that it's equivalent to an
  older state's model.  This is achieved by testing if (comparefn old-model
  new-model) returns true for any old-model.  A predefined `comparefn` is
  `funnyqt.generic/equal-models?` which see.

  Returns [state-space-graph state2model-map] after every rule has been applied
  to every state.  Of course, if the model diverges through application of the
  rules, i.e., the number of states constantly increases, then this will never
  terminate.

  The state-space-graph is a TGraph conforming to a schema defining the vertex
  class State and the edge class Transition.  States have an integer attribute
  n which is assigned monotonically increasing starting with 1, and the
  Transition edges have an attribute rule naming the rule which triggered this
  transition.  Every State has one outgoing Transition per rule applicable to
  the model represented by that State, i.e., there are at most (count rules)
  outgoing Transitions at each State.

  The state2model-map is a map from State vertices to the model which this
  State represents."
  [model comparefn rules & additional-args]
  (let [sss-fn (apply state-space-step-fn model comparefn rules additional-args)]
    (while (sss-fn)) ;; Apply as long as it returns true
    [(:state-space-graph (meta sss-fn)) @(:state2model-map (meta sss-fn))]))

(defn ^:private explore-state-space-dialog [sss-fn rules]
  (let [ssg (:state-space-graph (meta sss-fn))
        s2m (:state2model-map (meta sss-fn))
        state (fn [n]
                (first (filter #(= n (tg/value % :n))
                               (tg/vseq ssg 'State))))
        state-model (fn [n]
                      (@s2m (state n)))
        action (fn ^Action [name f]
                 (proxy [AbstractAction] [name]
                   (actionPerformed [ev]
                     (f))))
        d (javax.swing.JDialog.)
        content-pane (.getContentPane d)
        rule-select-panel (JPanel.)
        states-panel (JPanel.)
        all-states-cb (JComboBox.)
        reset-all-states-cb! (fn []
                               (.removeAllItems all-states-cb)
                               (doseq [n (map #(tg/value % :n)
                                              (tg/vseq ssg 'State))]
                                 (.addItem all-states-cb n)))
        undone-states-cb (JComboBox.)
        button-panel (JPanel.)
        selected-rules (promise)
        reset-undone-states-cb! (fn reset-undone-states-cb! []
                                  (.removeAllItems undone-states-cb)
                                  (doseq [n (map #(tg/value % :n)
                                                 (remove #(.containsAll
                                                           ^java.util.Set (tg/value % :done)
                                                           (map rule-name (@selected-rules)))
                                                         (tg/vseq ssg 'State)))]
                                    (.addItem undone-states-cb n)))
        rule-check-boxes (for [r rules]
                           (let [cb (JCheckBox. (let [^Action a (action (rule-name r)
                                                                        reset-undone-states-cb!)]
                                                  (.putValue a "rule" r)
                                                  a))]
                             (doto cb (.setSelected true))))
        show-done-attr-checkbox (JCheckBox. "Show :done")]
    (deliver selected-rules (fn selected-rules []
                              (for [^JCheckBox cb rule-check-boxes
                                    :when (.isSelected cb)]
                                (.getValue (.getAction cb) "rule"))))
    (.setTitle d "State Space Explorer")
    (.setDefaultCloseOperation d WindowConstants/DISPOSE_ON_CLOSE)
    (doto (javax.swing.ToolTipManager/sharedInstance)
      (.setEnabled true))
    (reset-all-states-cb!)
    (reset-undone-states-cb!)
    (.setLayout content-pane (GridLayout. 3 1))

    (.setLayout states-panel (GridLayout. 2 3))
    (.setBorder states-panel (BorderFactory/createTitledBorder "State Selection"))
    (.add states-panel (JLabel. "All States:"))
    (.add states-panel all-states-cb)
    (.add states-panel (JButton. ^Action (action "View Model"
                                                 #(viz/print-model
                                                   (state-model
                                                    (.getSelectedItem all-states-cb))
                                                   :gtk))))

    (.add states-panel (JLabel. "Undone States:"))
    (.add states-panel undone-states-cb)
    (.add states-panel (JButton. ^Action (action "Apply"
                                                 (fn []
                                                   (let [n (.getSelectedItem undone-states-cb)]
                                                     (sss-fn (constantly (state n))
                                                             (@selected-rules))
                                                     (reset-undone-states-cb!)
                                                     (reset-all-states-cb!))))))

    (.setLayout rule-select-panel (GridLayout. (let [rc (count rules)]
                                                 (if (odd? rc)
                                                   (inc (quot rc 2))
                                                   (quot rc 2)))
                                               2))
    (.setBorder rule-select-panel (BorderFactory/createTitledBorder "Rule Selection"))
    (doseq [^JCheckBox rcb rule-check-boxes]
      (.add rule-select-panel rcb))

    (.setLayout button-panel (GridLayout. 1 3))
    (.add button-panel show-done-attr-checkbox)
    (.add button-panel (JButton.
                        ^Action
                        (action "View State Space Graph"
                                #(apply
                                  viz/print-model
                                  ssg :gtk
                                  (when-not (.isSelected show-done-attr-checkbox)
                                    (list :excluded-attributes {'State #{:done}}))))))
    (.add button-panel (JButton. ^Action (action "Done" #(.dispose d))))

    (.add content-pane states-panel)
    (.add content-pane rule-select-panel)
    (.add content-pane button-panel)
    (.pack d)
    (.setResizable d false)
    (.setVisible d true)
    d))

(defn explore-state-space
  "Fires up a GUI that allows for creating and inspecting the state space by
  starting with initial model `model`, the given `comparefn` (see, e.g.,
  `funnyqt.generic/equal-models?`), and the given `rules` plus
  `additional-args` applied to each rule in addition to the current model."
  [model comparefn rules & additional-args]
  (let [sss-fn (apply state-space-step-fn model comparefn rules additional-args)]
    (explore-state-space-dialog sss-fn rules)))
