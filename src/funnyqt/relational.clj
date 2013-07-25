(ns funnyqt.relational
  "Generic relations"
  (:refer-clojure :exclude [==])
  (:require clojure.walk)
  (:use clojure.core.logic
        [clojure.core.logic.protocols :only [walk]]
        [funnyqt.utils :only [errorf]]
        funnyqt.relational.util))

(defmacro with-fresh
  "Replace all symbols with a leading question mark with fresh lvars.
  In addition, all occurences of `_' are replaced with fresh lvars, one per
  occurence.  That means, that in `forms` all occurences of ?foo will be
  unified, but all occurences of `_' are not."
  [& forms]
  (let [fs (clojure.walk/postwalk #(if (= '_ %) (gensym "?") %) forms)
        qsyms (vec (distinct (filter qmark-symbol? (flatten fs))))]
    `(fresh ~qsyms
       ~@fs)))

(defn echo
  "Prints the values of all `lvars`.  Always succeeds."
  ([lvars]
     (echo nil lvars))
  ([prompt lvars]
     (fn [a]
       (println (apply str
                       prompt
                       (interpose ", " (map (fn [v]
                                              (if (lvar? v)
                                                (let [w (walk a v)]
                                                  (str (.name v) " = " w))
                                                (str "### = " v)))
                                            lvars))))
       (succeed a))))

(defmacro condx
  "Expands into a `conda` checking if all vars used in the questions are ground.
  If so, then use a (conda ~@clauses), else use (conde ~@clauses).
  Thus, `condx` can be used as a generator just like `conde`, but if everything
  is ground, then the use of `conda` improves the performance."
  [& clauses]
  (let [vars (mapcat (fn [c]
                       (let [f (first c)]
                         (if (list? f)
                           (rest f)
                           [])))
                     clauses)]
    `(conda
      [(all ~@(map (fn [v] `(nonlvaro ~v)) vars))
       (conda ~@clauses)]
      [succeed
       (conde ~@clauses)])))

(defn- str-splits [s]
  (loop [idx 0, r []]
    (if (<= idx (count s))
      (recur (inc idx)
             (conj r [(subs s 0 idx) (subs s idx)]))
      r)))

(defn stro
  [x y xy]
  (fn [a]
    (let [wx  (walk a x)
          wy  (walk a y)
          wxy (walk a xy)]
      (cond
       (and (ground? wx) (ground? wy) (ground? wxy))
       (if (= (str wx wy) wxy) (succeed a) (fail a))

       (and (ground? wx) (ground? wy))
       (or (unify a [x y xy] [wx wy (str wx wy)])
           (fail a))

       (and (ground? wx) (ground? wxy) (string? wxy)
            (.startsWith ^String wxy wx))
       (or (unify a [x y xy] [wx (subs wxy (count wx)) wxy])
           (fail a))

       (and (ground? wy) (ground? wxy) (string? wxy)
            (.endsWith ^String wxy wy))
       (or (unify a [x y xy] [(subs wxy 0 (count wy)) wy wxy])
           (fail a))

       (ground? wxy)
       (to-stream
        (->> (map (fn [[s1 s2]]
                    (unify a [x y xy] [s1 s2 wxy]))
                  (str-splits wxy))
             (remove not)))

       ;; TODO: we should not fail here...
       :else (fail a)))))
