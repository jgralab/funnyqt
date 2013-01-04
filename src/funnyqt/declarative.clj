(ns funnyqt.declarative
  (:use [funnyqt.protocols   :only [has-type?]])
  (:use [funnyqt.utils       :only [errorf]])
  (:use [funnyqt.query       :only [member? xor]])
  (:require [ordered.map     :as om])
  (:require [funnyqt.emf     :as emf])
  (:require [funnyqt.tg      :as tg])
  (:use [clojure.tools.macro :only [name-with-attributes macrolet mexpand-all]]))

(defn ^:private rule? [form]
  (if-let [[name args & body] form]
    (let [m (apply hash-map (apply concat (partition 2 body)))]
      (and (or (contains? m :generalizes)
               (contains? m :from)
               (contains? m :to))
           (if (contains? m :generalizes)
             (or (vector? (:generalizes m))
                 (errorf "Error in %s: :generalizes must be a vector" form))
         true)
           (if (xor (contains? m :from) (contains? m :to))
             (errorf "Error in %s: rules must contain :from and :to, %s."
                     form "or neither of both")
             true)
           (if (contains? m :from)
             (or (and (seq (:from m))
                      (= 2 (count (:from m))) ;; (quote Foo)
                      (symbol? (first (:from m))))
                 (errorf "Error in %s: :from must be a quoted symbol." form))
             true)
           (if (contains? m :to)
             (or (vector? (:to m))
                 (errorf "Error in %s: :to must be a vector." form))
             true)))
    (errorf "Error in %s: neither helper nor rule." form)))

(def ^{:dynamic true
       :doc "Actions deferred to the end of the transformation."}
  *deferred-actions*)

(def ^{:dynamic true
       :doc "A map {rule {input output}}."}
  *trace*)

(defmacro deferred
  "Captures a thunk (closure) that evaluates `body` as the last step of the
  transformation."
  [& body]
  `(swap! *deferred-actions* conj (fn [] ~@body)))

(defn ^:private rule-as-map [rule]
  (let [[name args & body] rule]
    (assoc (loop [b body, v []]
             (if (seq b)
               (if (keyword? (first b))
                 (recur (nnext b) (conj v (first b) (second b)))
                 (apply hash-map (conj v :body b)))
               (apply hash-map v)))
      :name name
      :args args)))

(defn ^:private make-lookups [arg where gens]
  (cons `(get ((deref *trace*) ~(keyword (name where))) ~arg)
        (map (fn [w] `(~w ~arg))
             gens)))

(defn ^:private type-constr [e t]
  (if t
    `(has-type? ~e ~t)
    true))

(defn ^:private create-vector [v outs]
  (let [v (loop [v v, r []]
            (if (seq v)
              (if (= (first (nnext v)) :model)
                (recur (nnext (nnext v)) (conj (into r (take 2 v))
                                               (outs (nth v 3))
                                               (nth v 3)))
                (recur (nnext v) (conj (into r (take 2 v))
                                       (outs (ffirst outs))
                                       (ffirst outs))))
              r))
        v (partition 4 v)]
    (vec (mapcat (fn [[sym type mk model]]
                   [sym (if (= mk :tg)
                          `(tg/create-vertex! ~model ~type)
                          `(emf/ecreate! ~model ~type))])
                 v))))

(defn ^:private convert-rule [outs rule]
  (let [m (rule-as-map rule)
        _ (when (> (count (:args m)) 1)
            (errorf "Error: Rules must have exactly one argument: %s" (:name m)))
        arg (first (:args m))
        create-vec (create-vector (:to m) outs)
        created (mapv first (partition 2 create-vec))
        retval (if (= (count created) 1)
                 (first created)
                 created)
        wl-vars (map first (partition 2 (:when-let m)))
        creation-form (when (seq created)
                        `(let ~create-vec
                           (swap! *trace* update-in [~(keyword (:name m))]
                                  assoc ~arg ~retval)
                           ~@(:body m)
                           ~retval))
        wl-and-creation-form (if (:when-let m)
                               `(let ~(vec (:when-let m))
                                  (when (and ~@wl-vars)
                                    ~creation-form))
                               creation-form)
        when-wl-and-creation-form (if (:when m)
                                    `(when ~(or (:when m) true)
                                       ~wl-and-creation-form)
                                    wl-and-creation-form)]
    (when-let [uks (seq (disj (set (keys m))
                              :name :args :from :to :when :when-let :body :generalizes))]
      (errorf "Unknown keys in declarative rule: %s" uks))
    `(~(:name m) ~(:args m)
      (when ~arg
        ;; First check if there's already an element created for arg in this
        ;; rule, or any of its specializing rules.
        (or ~@(make-lookups arg (:name m) (:generalizes m))
            ;; type constraint & :when constraint
            (when ~(type-constr arg (:from m))
              ~when-wl-and-creation-form))))))

(defmacro deftransformation [name & more]
  (let [[name more] (name-with-attributes name more)
        [args rules-and-fns] (if (vector? (first more))
                               [(first more) (next more)]
                               (errorf "Error: arg vector missing!"))
        [ins outs] (let [i (first args)
                         o (second args)]
                     (cond
                      (nil? i) (errorf "No input models given.")
                      (nil? o) (errorf "No output models given.")
                      (not (vector? i)) (errorf "Error: input models must be a vector.")
                      (not (vector? o)) (errorf "Error: output models must be a vector.")
                      :else [(apply om/ordered-map i) (apply om/ordered-map o)]))
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     rules-and-fns)
        top-rules   (filter #(:top (meta (first %))) rules)
        type-spec   (vec (cons :or (remove nil? (map (fn [r]
                                                       (let [m (rule-as-map r)]
                                                         (:from m)))
                                                     top-rules))))]
    (when-not (seq top-rules)
      (errorf "At least one rule has to be declared as top-level rule."))
    `(defn ~name ~(meta name)
       [~@(keys ins) ~@(keys outs)]
       (binding [*deferred-actions* (atom [])
                 *trace*            (atom {})]
         (letfn [~@fns
                 ~@(map (partial convert-rule outs) rules)]
           ~@(for [m (keys ins)
                   :let [kind (ins m)]]
               `(doseq [elem# ~(if (= :tg kind)
                                 `(tg/vseq ~m ~type-spec)
                                 `(emf/eallobjects ~m ~type-spec))]
                  (doseq [r# ~(mapv first top-rules)]
                    (r# elem#))))
           ~(if (= (count outs) 1)
              (first (first outs))
              (vec (keys outs))))))))

