(ns funnyqt.pmatch
  "Pattern Matching."
  (:use [funnyqt.utils :only [errorf pr-identity]])
  (:use [funnyqt.query :only [the for*]])
  (:use funnyqt.macro-utils)
  (:use funnyqt.protocols)
  (:require [funnyqt.tg :as tg]
            [funnyqt.query.tg :as tgq]
            [funnyqt.emf :as emf])
  (:require clojure.set)
  (:require [clojure.tools.macro :as m]))

(defn- verify-match-vector
  "Ensure that the match vector `match` and the arg vector `args` are disjoint.
  Throws an exception if they overlap, else returns `match`."
  [match args]
  (let [blist (bindings-to-arglist match)]
    (if (seq (clojure.set/intersection
              (set blist)
              (set args)))
      (errorf "Arglist and match vector overlap!")
      (if-let [double-syms (seq (mapcat (fn [[sym freq]]
                                          (when (> freq 1)
                                            (str "- " sym " is declared " freq " times\n")))
                                        (frequencies blist)))]
        (errorf "These symbols are declared multiple times:\n%s"
                (apply str double-syms))
        match))))

(defn- symbol-and-type [sym]
  (if-let [[_ s t] (re-matches #"(?:<-|-)?([a-zA-Z0-9]+)?(?::([a-zA-Z_0-9]+))?(?:-|->)?"
                               (name sym))]
    [(and s (symbol s)) (and t `'~(symbol t))]
    (errorf "No valid pattern symbol: %s" sym)))

(defn- edge-sym? [sym]
  (or (re-matches #"<-[a-zA-Z:_0-9]*-" (name sym))
      (re-matches #"-[a-zA-Z:_0-9]*->" (name sym))))

(defn- edge-dir [esym]
  (if (edge-sym? esym)
    (if (re-matches #"<-.*" (name esym))
      :in
      :out)
    (errorf "%s is not edge symbol." esym)))

(defn- blank-sym? [sym]
  (re-matches #"[a-zA-Z0-9]+" (name sym)))

(defn- transform-match-vector-tg [matchvec graph]
  (letfn [(alpha-omega [p]
            (if (= (edge-dir p) :out)
              `(tg/omega ~(first (symbol-and-type p)))
              `(tg/alpha ~(first (symbol-and-type p)))))]
    (loop [p nil, match matchvec, acc [], known #{}]
      (if (seq match)
        (let [sym (first match)]
          #_(println "IN:" p "\t" sym "\t" (fnext match) "\t| known:" known)
          (cond
           ;; `for` special keywords
           (#{:when :while} sym)
           (recur nil (nnext match) (conj acc sym (fnext match)) known)
           ;; `for` :let kw
           (= sym :let)
           (recur nil (nnext match) (conj acc sym (fnext match))
                  (apply conj known (bindings-to-arglist (fnext match))))
           ;; normal declaration: a (foo ...)
           (and (symbol? sym) (blank-sym? sym) (coll? (fnext match)))
           (recur nil (nnext match) (conj acc sym (fnext match))
                  (conj known sym))
           ;; destructuringforms: [a b] (call-to-some-fn ...)
           (and (coll? sym) (coll? (fnext match)))
           (recur nil (nnext match) (conj acc sym (fnext match))
                  (apply conj known (bindings-to-arglist [sym (fnext match)])))
           :else
           (let [[s t] (symbol-and-type sym)]
             (cond
              ;; No previous symbol
              (nil? p)
              (recur sym (next match)
                     (if (known s)
                       acc
                       (conj acc s (if (edge-sym? sym)
                                     `(tgq/eseq ~graph ~t)
                                     `(tgq/vseq ~graph ~t))))
                     (conj known s))
              ;; -x:E-> z:V, a (a was declared before), or
              (and (blank-sym? sym) (not (edge-sym? p)))
              (if (known sym)
                (recur sym (next match) acc known)
                (errorf "'%s' is not declared." sym))
              ;; -x:E-> a (a was declared before)
              (and (blank-sym? sym) (edge-sym? p))
              (if (known sym)
                (recur sym (next match)
                       (conj acc :when `(= ~sym ~(alpha-omega p)))
                       known)
                (errorf "'%s' is not declared." sym))
              ;; -a:E-> b:V (node after edge)
              (and (edge-sym? p) (not (edge-sym? sym)))
              (recur sym (next match)
                     (if (first (symbol-and-type p))
                       ;; The prev edge was named, so the node sym is the
                       ;; target
                       (apply conj acc `[:let [~s ~(alpha-omega p)]
                                         ~@(when t
                                             `[:when (has-type? ~s ~t)])])
                       ;; The prev edge was anonymous, so it declared sym
                       ;; already and we only need a type check here (if a
                       ;; type is given)
                       (if t
                         (conj acc :when `(has-type? ~s ~t))
                         acc))
                     (if (first (symbol-and-type p))
                       (conj known s)
                       known))
              ;; b:V -a:E-> (edge after node)
              (and (not (edge-sym? p)) (edge-sym? sym))
              (if s
                (recur sym (next match)
                       (conj acc s
                             `(tgq/iseq ~(first (symbol-and-type p))
                                        ~t ~(edge-dir sym)))
                       (conj known s))
                ;; Anonymous edge: a:A -:Foo-> b:B, mustn't create symbol for
                (let [nextsym (first (symbol-and-type (fnext match)))]
                  (recur sym (next match)
                         (conj acc nextsym
                               `(map tg/that
                                     (tgq/iseq ~(first (symbol-and-type p))
                                               ~t ~(edge-dir sym))))
                         (conj known nextsym))))
              ;; b:V, c:V (node after node)
              (and (not (edge-sym? p)) (not (edge-sym? sym)))
              (if (known s)
                (recur sym (next match)
                       (conj acc s) known)
                (recur sym (next match)
                       (conj acc s `(tgq/vseq ~graph ~t))
                       (conj known s)))
              :else (errorf "Cannot handle pattern %s at position %s"
                            matchvec sym)))))
        acc))))


(def ^:dynamic *pattern-match-context*
  nil)

(defn- shortcut-let-vector [lv]
  (mapcat (fn [[s v]]
            [:let [s v] :when s])
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

(defn transform-match-vector
  "Transforms patterns like a:X -:role-> b:Y to `for` syntax.
  (Only used internally)"
  [match args]
  ;; NOTE: the first element in args must be the graph/model symbol...
  (verify-match-vector
   (shortcut-bindings
    (let [m (first args)]
      (case *pattern-match-context*
        :tgraph (transform-match-vector-tg match m)
        :emf    (errorf "Not yet implemented.")
        match)))
   args))

(defn- convert-spec [[a m]]
  (let [tm (transform-match-vector m a)]
    `(~a
      (for ~tm
        ~(bindings-to-arglist tm)))))

(defmacro defpattern
  "Defines a pattern with `name`, optional `doc-string`, optional `attr-map`,
  an `args` vector, and a `match` vector.  When invoked, it returns a lazy seq
  of all matches of `match`.

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
  (let [[name more] (m/name-with-attributes name more)]
    `(defn ~name ~(meta name)
      ~@(if (seq? (first more))
          (map convert-spec more)
          (convert-spec more)))))
