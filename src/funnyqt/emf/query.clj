(ns funnyqt.emf.query
  "More sophisticated constructs for querying EMF models."
  (:use funnyqt.utils)
  (:use funnyqt.generic)
  (:use funnyqt.generic-protocols)
  (:use ordered.set)
  (:use ordered.map)
  (:use funnyqt.emf.core)
  (:require clojure.set))

(add-long-doc! "TODO")

;;* Code

;;** Regular Path Descriptions

(defn <>--
  ([obj]
     (mapcat econtents (into-oset obj)))
  ([obj ts]
     (mapcat #(econtents % ts) (into-oset obj))))

(defn -->
  ([obj]
     (mapcat ecrossrefs (into-oset obj)))
  ([obj rs]
     (mapcat #(ecrossrefs % rs) (into-oset obj))))

(defn -->>
  ([obj]
     (mapcat erefs (into-oset obj)))
  ([obj rs]
     (mapcat #(erefs % rs) (into-oset obj))))

(defn --<>
  "Returns an ordered set containing `obj's container.  If there's none,
  returns the empty set."
  [obj]
  (map econtainer (into-oset obj)))

(defn <--
  ([obj]
     (mapcat inv-ecrossrefs (into-oset obj)))
  ([obj rs]
     (mapcat #(inv-ecrossrefs % rs) (into-oset obj)))
  ([obj rs container]
     (mapcat #(inv-ecrossrefs % rs container) (into-oset obj))))

(defn <<--
  ([obj]
     (mapcat inv-erefs (into-oset obj)))
  ([obj rs]
     (mapcat #(inv-erefs % rs) (into-oset obj)))
  ([obj rs container]
     (mapcat #(inv-erefs % rs container) (into-oset obj))))

(defn reachables
  "Returns the ordered set of EObjects reachable from `obj' by via the path
  description `p'.
  `obj' may be an EObject or a seq of EObjects."
  [obj p]
  (cond
   ;; funs: -->
   (fn? p) (into-oset (p obj))
   ;; funs with params: [--> :foo], [p-alt :foo :bar]
   (coll? p) (into-oset (apply (first p) obj (rest p)))
   ;; EReference names
   (qname? p) (into-oset (mapcat #(erefs % p) (into-oset obj)))
   :else (error (format "Don't know how to apply %s." p))))


(defn p-seq
  "Path sequence starting at `obj' and traversing `p'.
  `obj' may be an EObject or a seq of EObjects.
  `p' is a varargs seq of path descriptions."
  [obj & p]
  (if (seq p)
    (recur (reachables obj (first p)) (rest p))
    (into-oset obj)))

(defn p-opt
  "Path option starting at `obj' and maybe traversing `p'.
  `obj' may be an EObject or a seq of EObjects.
  `p' is a path description."
  [obj p]
  (into-oset obj (reachables obj p)))

(defn p-alt
  "Path alternative starting at `obj' and traversing one of `p'.
  `obj' may be an EObject or a seq of EObjects.
  `p' is a varags seq of the alternative path descriptions."
  [obj & p]
  (into-oset (mapcat #(reachables obj %) p)))

(defn p-+
  "Path iteration starting at `obj' and traversing `p' one or many times.
  `obj' may be an EObject or a seq of EObjects.
  `p' is a path description."
  ([obj p]
     (p-+ obj p false true))
  ([obj p d skip-obj]
     (let [obj (into-oset obj)
           n   (reachables (if (false? d) obj d) p)
           sv  (if skip-obj n (into-oset obj n))
           df  (clojure.set/difference n obj)]
       (if (seq df)
         (recur sv p df false)
         sv))))

(defn p-*
  "Path iteration starting at `obj' and traversing `p' zero or many times.
  `obj' may be an EObject or a seq of EObjects.
  `p' is a path description."
  [obj p]
  (p-+ obj p false false))

(defn p-exp
  "Path exponent starting at `obj' and traversing `p' `n' times, or at least `l'
  and at most `p' times.
  `obj' may be an EObject or a seq of EObjects.
  `n' or `l' and `obj' are integers with `l' <= `b'.
  `p' is a path description."
  ([obj l u p]
     {:pre [(<= l u) (>= l 0) (>= u 0)]}
     (loop [i (- u l), s (p-exp obj l p)]
       (if (pos? i)
         (let [ns (into s (reachables s p))]
           (if (= (count s) (count ns))
             s
             (recur (dec i) ns)))
         s)))
  ([obj n p]
     {:pre [(>= n 0)]}
     (if (zero? n)
       (into-oset obj)
       (recur (reachables obj p) (dec n) p))))

(defn p-restr
  "EObject restriction concerning `ts' and `pred' on each object in `objs'.
  ts is a type specification (see `eclass-matcher'), `pred' a predicate."
  ([objs ts]
     (p-restr objs ts identity))
  ([objs ts pred]
     (let [objs (into-oset objs)]
       (into-oset
        (if (seq objs)
          (let [tm (eclass-matcher ts)]
            (filter (every-pred tm pred)
                    objs))
          objs)))))

;;** Describing EObjects and EClasses

(extend-protocol Describable
  org.eclipse.emf.ecore.EClass
  (describe [this]
    {:name (qname this)
     :abstract (.isAbstract this)
     :interface (.isInterface this)
     :superclasses (seq (.getESuperTypes this))
     :attributes (into {}
                       (map (fn [^org.eclipse.emf.ecore.EAttribute attr]
                              [(keyword (.getName attr)) (.getEType attr)])
                            (seq (.getEAttributes this))))
     :references (into {}
                       (map (fn [^org.eclipse.emf.ecore.EReference ref]
                              [(keyword (.getName ref)) (.getEReferenceType ref)])
                            (seq (.getEReferences this))))})
  org.eclipse.emf.ecore.EObject
  (describe [this]
    {:eclass (.getName (.eClass this))
     :container (econtainer this)
     :attr-slots (into {}
                       (map (fn [^org.eclipse.emf.ecore.EAttribute attr]
                              (let [kw (keyword (.getName attr))]
                                [kw (eget this kw)]))
                            (seq (.getEAllAttributes (.eClass this)))))
     :ref-slots (into {}
                       (map (fn [^org.eclipse.emf.ecore.EReference ref]
                              (let [kw (keyword (.getName ref))]
                                [kw (eget this kw)]))
                            (seq (.getEAllReferences (.eClass this)))))}))

