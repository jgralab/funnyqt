(ns funnyqt.relational
  "Generic relations"
  (:require clojure.walk
            clojure.string
	    [clojure.core.logic :as ccl]
	    [clojure.core.logic.protocols :as cclp]
	    [funnyqt.utils :as u]
	    [funnyqt.relational.util :as ru]
            [funnyqt.relational.tmp-elem :as tmp]
            [funnyqt.generic :as g])
  (:import (funnyqt.relational.tmp_elem WrapperElement)))

;;* Utils

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
       #_(clojure.pprint/pprint a)
       (println (clojure.string/join
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

;;# Model Relations

;;## Elements

(defn tmp-elemento
  ([m el]
   (fn [a]
     (let [gel (cclp/walk a el)]
       (cond
         (not (or (ru/fresh? gel) (tmp/tmp-or-wrapper-element? gel)))
         (u/errorf "tmp-element/2: el must be fresh or a ground Wrapper/TmpElement but was %s."
                   gel)

         (ru/ground? gel)
         (if (tmp/set-kind gel :element)
           (ccl/succeed a)
           (ccl/fail a))

         :else (ccl/to-stream
                (->> (map #(ccl/unify a el %)
                          (concat
                           (map (partial tmp/make-wrapper m el)
                                (g/elements m))
                           [(tmp/make-tmp-element m :element)]))
                     (remove not)))))))
  ([m el t]
   (fn [a]
     (let [gel (cclp/walk a el)
           gt  (cclp/walk a t)]
       (cond
         (not (ru/ground? gt))
         (u/errorf "tmp-elemento/3: type must be ground.")

         (not (or (ru/fresh? gel) (tmp/tmp-or-wrapper-element? gel)))
         (u/errorf "tmp-elemento/3: el must be fresh or a ground Wrapper/TmpElement but was %s."
                   gel)

         (ru/ground? gel) ;; TODO: we probably need something like tg/kind-aec-tup-from-spec, too
         (if (and (tmp/set-kind gel :element)
                  (tmp/set-type gel gt))
           (ccl/succeed a)
           (ccl/fail a))

         :else (ccl/to-stream
                (->> (map #(ccl/unify a el %)
                          (concat
                           (map (partial tmp/make-wrapper m el)
                                (g/elements m gt))
                           [(tmp/make-tmp-element m :element gt)]))
                     (remove not))))))))

(defn element
  "A relation where the element `el` has the type `t` in model `m`.  In fact,
  `t` may be any type specification (see `funnyqt.generic/type-matcher`)."
  ([m el]
     (if tmp/*make-tmp-elements*
       (tmp-elemento m el)
       (fn [a]
         (let [gel (cclp/walk a el)]
           (if (ru/ground? gel)
             (if (g/element? gel) (ccl/succeed a) (ccl/fail a))
             (ccl/to-stream
              (->> (map #(ccl/unify a el %)
                        (g/elements m))
                   (remove not))))))))
  ([m el t]
     (if tmp/*make-tmp-elements*
       (tmp-elemento m el t)
       (fn [a]
         (let [gel (cclp/walk a el)
               gt (cclp/walk a t)]
           (cond
             (or (and (ru/ground? gel) (not (g/element? gel)))
                 (and (ru/ground? gt) (not (or (symbol? gt) (coll? gt)))))
             (ccl/fail a)

             (and (ru/ground? gel) (ru/ground? gt))
             (if (g/has-type? gel gt)
               (ccl/succeed a)
               (ccl/fail a))

             (ru/ground? gel)
             (ccl/unify a t (g/qname gel))

             (ru/ground? gt)
             (ccl/to-stream
              (->> (map #(ccl/unify a el %) (g/elements m t))
                   (remove not)))

             :else (ccl/to-stream
                    (->> (for [elem (g/elements m t)]
                           (ccl/unify a [el t] [elem (g/qname elem)]))
                         (remove not)))))))))

;;## Relationships

(defn tmp-relationshipo [m rel src trg]
  (fn [a]
    (let [grel (cclp/walk a rel)
          gsrc (cclp/walk a src)
          gtrg (cclp/walk a trg)]
      (cond
        (not (or (ru/fresh? grel) (tmp/tmp-or-wrapper-element? grel)))
        (u/errorf "tmp-relationshipo: rel must be fresh or a ground Wrapper/TmpElement but was %s."
                  grel)

        (not (or (ru/fresh? gsrc) (tmp/tmp-or-wrapper-element? gsrc)))
        (u/errorf "tmp-relationshipo: src must be fresh or a ground Wrapper/TmpElement but was %s."
                  gsrc)

        (not (or (ru/fresh? gtrg) (tmp/tmp-or-wrapper-element? gtrg)))
        (u/errorf "tmp-relationshipo: trg must be fresh or a ground Wrapper/TmpElement but was %s."
                  gtrg)

        (tmp/wrapper-element? grel)
        (ccl/unify a [src trg]
                   (let [edge (.wrapped-element ^WrapperElement grel)]
                     [(tmp/make-wrapper m src (g/source edge))
                      (tmp/make-wrapper m trg (g/target edge))]))

        (and (ru/fresh? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
        (ccl/to-stream
         (->> (map (fn [ed]
                     (ccl/unify a rel ed))
                   (concat
                    (map (partial tmp/make-wrapper m rel)
                         (filter
                          #(= (.wrapped-element ^WrapperElement gtrg) (g/target %))
                          (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out)))
                    [(doto (tmp/make-tmp-element m :relationship)
                       (tmp/set-alpha src)
                       (tmp/set-omega trg))]))
              (remove not)))

        (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc) (tmp/wrapper-element? gtrg))
        (if (and (tmp/set-alpha grel src)
                 (tmp/set-omega grel trg))
          (ccl/succeed a)
          (ccl/fail a))

        (and (ru/fresh? grel) (tmp/wrapper-element? gsrc))
        (ccl/to-stream
         (->> (map (fn [ed-om]
                     (ccl/unify a [rel trg] ed-om))
                   (concat
                    (map (fn [ed]
                           [(tmp/make-wrapper m rel ed)
                            (tmp/make-wrapper m trg (g/target ed))])
                         (g/incident-relationships (.wrapped-element ^WrapperElement gsrc) nil :out))
                    [(let [ed (tmp/make-tmp-element m :relationship)]
                       (tmp/set-alpha ed src)
                       (tmp/set-omega ed trg)
                       [ed gtrg])]))
              (remove not)))

        (and (tmp/tmp-element? grel) (tmp/wrapper-element? gsrc))
        (if (and (tmp/set-alpha grel src)
                 (tmp/set-omega grel trg))
          (ccl/succeed a)
          (ccl/fail a))

        (and (ru/fresh? grel) (tmp/wrapper-element? gtrg))
        (ccl/to-stream
         (->> (map (fn [ed-al]
                     (ccl/unify a [rel src] ed-al))
                   (concat
                    (map (fn [ed]
                           [(tmp/make-wrapper m rel ed)
                            (tmp/make-wrapper m src (g/source ed))])
                         (g/incident-relationships (.wrapped-element ^WrapperElement gtrg) nil :in))
                    [(let [ed (tmp/make-tmp-element m :relationship)]
                       (tmp/set-alpha ed src)
                       (tmp/set-omega ed trg)
                       [ed gsrc])]))
              (remove not)))

        (and (tmp/tmp-element? grel) (tmp/wrapper-element? gtrg))
        (if (and (tmp/set-alpha grel src)
                 (tmp/set-omega grel trg))
          (ccl/succeed a)
          (ccl/fail a))

        :else (u/errorf "Can't handle (tmp-relationshipo %s %s %s %s)" m grel gsrc gtrg)))))

(defn relationshipo
  "A relation where `rel` is a relationship in model `m` starting at `src` and
  ending at `trg`. `m` has to be ground."
  [m rel src trg]
  (if tmp/*make-tmp-elements*
    (tmp-relationshipo m rel src trg)
    (fn [a]
      (let [grel     (cclp/walk a rel)
            gsrc (cclp/walk a src)
            gtrg (cclp/walk a trg)]
        (cond
         (or (and (ru/ground? grel) (not (g/relationship? grel)))
             (and (ru/ground? gsrc) (not (g/element? gsrc)))
             (and (ru/ground? gtrg) (not (g/element? gtrg))))
         (ccl/fail a)

         (ru/ground? grel)
         (ccl/unify a [src trg] [(g/source grel) (g/target grel)])

         (ru/ground? gsrc)
         (ccl/to-stream
          (->> (map #(ccl/unify a [rel trg] [% (g/target %)])
                    (g/incident-relationships gsrc nil :out))
               (remove not)))

         (ru/ground? gtrg)
         (ccl/to-stream
          (->> (map #(ccl/unify a [rel src] [% (g/source %)])
                    (g/incident-relationships gtrg nil :in))
               (remove not)))

         :else (ccl/to-stream
                (->> (for [edge (g/relationships m)]
                       (ccl/unify a [rel src trg]
                                  [edge (g/source edge) (g/target edge)]))
                     (remove not))))))))

