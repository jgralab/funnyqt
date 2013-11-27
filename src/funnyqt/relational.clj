(ns funnyqt.relational
  "Generic relations"
  (:require clojure.walk
	    [clojure.core.logic :as ccl]
	    [clojure.core.logic.protocols :as cclp]
	    [funnyqt.utils :as u]
	    [funnyqt.relational.util :as ru]))

(defmacro with-fresh
  "Replace all symbols with a leading question mark with fresh lvars.
  In addition, all occurences of `_' are replaced with fresh lvars, one per
  occurence.  That means, that in `forms` all occurences of ?foo will be
  unified, but all occurences of `_' are not."
  [& forms]
  (let [fs (clojure.walk/postwalk #(if (= '_ %) (gensym "?") %) forms)
	qsyms (vec (distinct (filter ru/qmark-symbol? (flatten fs))))]
    `(ccl/fresh ~qsyms
       ~@fs)))

(defn echo
  "Prints the values of all `lvars`.  Always succeeds."
  ([lvars]
     (echo nil lvars))
  ([prompt lvars]
     (fn [a]
       (println "################# " (or prompt "ECHO") " #################")
       (clojure.pprint/pprint a)
       (println (apply str
                       (map (fn [^clojure.core.logic.LVar v]
                              (if (ccl/lvar? v)
                                (let [w (cclp/walk a v)]
                                  (str (:oname v) " = " w "\n"))
                                (str "### = " v "\n")))
                            lvars)))
       (ccl/succeed a))))

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
    `(ccl/conda
      [(all ~@(map (fn [v] `(ccl/nonlvaro ~v)) vars))
       (ccl/conda ~@clauses)]
      [ccl/succeed
       (ccl/conde ~@clauses)])))

(defn ^:private str-splits [s]
  (loop [idx 0, r []]
    (if (<= idx (count s))
      (recur (inc idx)
	     (conj r [(subs s 0 idx) (subs s idx)]))
      r)))

(defn stro
  ([x y xy]
     (fn [a]
       (let [wx  (cclp/walk a x)
             wy  (cclp/walk a y)
             wxy (cclp/walk a xy)]
         (cond
          (and (ru/ground? wx) (ru/ground? wy) (ru/ground? wxy))
          (if (= (str wx wy) wxy) (ccl/succeed a) (ccl/fail a))

          (and (ru/ground? wx) (ru/ground? wy))
          (or (ccl/unify a xy (str wx wy))
              (ccl/fail a))

          (and (ru/ground? wx) (ru/ground? wxy) (string? wxy)
               (.startsWith ^String wxy wx))
          (or (ccl/unify a y (subs wxy (count wx)))
              (ccl/fail a))

          (and (ru/ground? wy) (ru/ground? wxy) (string? wxy)
               (.endsWith ^String wxy wy))
          (or (ccl/unify a x (subs wxy 0 (- (count wxy) (count wy))))
              (ccl/fail a))

          (ru/ground? wxy)
          (ccl/to-stream
           (->> (map (fn [[s1 s2]]
                       (ccl/unify a [x y] [s1 s2]))
                     (str-splits wxy))
                (remove not)))

          ;; TODO: we should not fail here...
          :else (ccl/fail a)))))
  ([x y z xyz]
     (ccl/fresh [front]
       (ccl/conde
        ;; This one works if x and y are ground
        [(stro x y front)
         (stro front z xyz)]
        ;; This one works if xyz is ground
        [(stro front z xyz)
         (stro x y front)]))))
