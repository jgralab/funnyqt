(ns funnyqt.declarative
  (:use [funnyqt.protocols   :only [has-type?]])
  (:use [funnyqt.utils       :only [errorf]])
  (:use [funnyqt.query       :only [member? xor]])
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
             (or (symbol? (:from m))
                 (errorf "Error in %s: :from must be a symbol."))
             true)
           (if (contains? m :to)
             (or (vector? (:to m))
                 (errorf "Error in %s: :to must be a vector." form))
             true)))
    (errorf "Error in %s: neither helper nor rule." form)))

(def ^{:dynamic true
       :doc "Actions deferred to the end of the transformation."}
  *deferred-actions*)

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

(defn ^:private make-lookups [trace arg where gens]
  (cons `(get ((deref ~trace) ~(keyword (name where))) ~arg)
        (map (fn [w] `(~w ~arg))
             gens)))

(defn ^:private convert-rule [trace rule]
  (let [m (rule-as-map rule)
        ;; TODO: must be exactly one arg; check it!
        arg (first (:args m))]
    `(~(:name m) ~(:args m)
      (when ~arg
        (or ~@(make-lookups trace arg
                            (:name m) (:generalizes m)))))))

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
                      :else [(apply hash-map i) (apply hash-map o)]))
        [rules fns] ((juxt (partial filter rule?) (partial remove rule?))
                     rules-and-fns)
        trace-sym (gensym "trace")]
    `(defn ~name ~(meta name)
       [~@(keys ins) ~@(keys outs)]
       (binding [*deferred-actions* (atom [])]
         (let [~trace-sym (atom {})]
           (letfn [~@fns
                   ~@(map (partial convert-rule trace-sym) rules)]))))))


(deftransformation families2genealogy [[in :emf]
                                       [out :tg]]
  (member2person [m]
                 :generalizes [member2male member2female])
  (member2person-setter [m p]
                        (tg/set-value! p :name (emf/eget m :firstName))
                        (deferred (do p)))
  (member2male [m]
               :model out1
               :from Member
               :when (male? m)
               :to   [p Male]
               (member2person-setter m p)
               (tg/add-adj p :wife (member2female (spouse m))))
  (member2female [m]
                 :from Member
                 :when (not (male? m))
                 :to   [p Female]
                 (member2person-setter m p)))

#_(defn families2genealogy-atl [in out]
  (let [cache (atom {})
        deferred-actions (atom [])]
    (letfn [(member2person [m]
              (when m
                (or (get (@cache :member2male) m)
                    (get (@cache :member2female) m))))
            (member2person-setter [m p]
              (tg/set-value! p :fullName (emf/eget m :firstName))
              (swap! deferred-actions conj
                     (fn []
                       (tg/set-adjs! p :parents (map member2person
                                                     (parents-of m))))))
            (member2male [m]
              (when m
                (or (get (@cache :member2male) m)
                    (when (and (has-type? m 'Member) (male? m))
                      (let [p (create-vertex! out 'Male)]
                        (swap! cache update-in [:member2male] assoc m p)
                        (member2person-setter m p)
                        (when-let [w (member2female (wife m))]
                          (add-adj! p :wife w))
                        p)))))
            (member2female [m]
              (when m
                (or (get (@cache :member2female) m)
                    (when (and (has-type? m 'Member) (not (male? m)))
                      (let [p (create-vertex! out 'Female)]
                        (swap! cache update-in [:member2female] assoc m p)
                        (member2person-setter m p)
                        p)))))]
      (doseq [v (emf/eallobjects in)]
        (doseq [r [member2male member2female]]
          (r v)))
      (doseq [a @deferred-actions]
        (a))
      out)))