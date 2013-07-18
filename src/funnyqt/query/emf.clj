(ns funnyqt.query.emf
  "More sophisticated constructs for querying EMF models."
  (:require [clojure.core.reducers :as r])
  (:use funnyqt.utils)
  (:use funnyqt.query)
  (:use funnyqt.protocols)
  (:require funnyqt.emf-protocols)
  (:use flatland.ordered.set)
  (:use flatland.ordered.map)
  (:use funnyqt.emf)
  (:import
   [org.eclipse.emf.ecore EObject EReference EStructuralFeature]))

;;# Regular Path Descriptions

(defn <>--
  "Returns the (direct) contents of EObject`obj` restricted by the type
  specification `ts` (see `funnyqt.protocols/type-matcher` for details).  `obj`
  may also be a collection of EObjects."
  ([obj]
     (oset (mapcat econtents (oset obj))))
  ([obj ts]
     (oset (mapcat #(econtents % ts) (oset obj)))))

(defn --->
  "Returns the EObjects cross-referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `eref-matcher` for
  details).  `obj` may also be a collection of EObjects.  In EMF,
  cross-referenced means referenced by a non-containment EReference."
  ([obj]
     (oset (mapcat ecrossrefs (oset obj))))
  ([obj rs]
     (oset (mapcat #(ecrossrefs % rs) (oset obj)))))

(defn -->
  "Returns the EObjects referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `eref-matcher` for
  details).  `obj` may also be a collection of EObjects.  In contrast to
  `--->`, this function includes both cross-references and containments."
  ([obj]
     (oset (mapcat erefs (oset obj))))
  ([obj rs]
     (oset (mapcat #(erefs % rs) (oset obj)))))

(defn --<>
  "Returns a seq containing `obj`s container.  If there's none,
  returns the empty set."
  [obj]
  (oset (mapcat #(when-let [c (econtainer %)]
                   [c])
                (oset obj))))

(defn <---
  "Returns all EObjects cross-referencing `obj` with a reference matching the
  reference specification `rs` (see `eref-matcher` for details).  `obj` may also
  be a collection of EObjects, in which case all objects cross-referencing any
  of the objects in `obj` is returned.  In EMF, cross-referenced means
  referenced by a non-containment EReference."
  ([obj]
     (oset (mapcat inv-ecrossrefs (oset obj))))
  ([obj rs]
     (oset (mapcat #(inv-ecrossrefs % rs) (oset obj))))
  ([obj rs container]
     (oset (mapcat #(inv-ecrossrefs % rs container) (oset obj)))))

(defn <--
  "Returns all EObjects referencing `obj` with a reference matching the
  reference specification `rs` (see `eref-matcher` for details).  `obj` may also
  be a collection of EObjects, in which case all objects referencing any of the
  objects in `obj` is returned.  In contrast to `<---', this function includes
  both cross-references and containments."
  ([obj]
     (oset (mapcat inv-erefs (oset obj))))
  ([obj rs]
     (oset (mapcat #(inv-erefs % rs) (oset obj))))
  ([obj rs container]
     (oset (mapcat #(inv-erefs % rs container) (oset obj)))))

(defn ^:private p-apply-emf
  [obj p]
  (cond
   ;; funs: --->
   (fn? p) (p obj)
   ;; funs with params: [---> :foo], [p-alt :foo :bar]
   (coll? p) (apply (first p) obj (rest p))
   ;; EReference names
   (prop-name? p) (oset (mapcat #(erefs % p) (oset obj)))
   :else (errorf "Don't know how to apply %s." p)))

(defn ^:private p-restr-emf
  "EObject restriction concerning `ts` and `pred` on each object in `objs`.
  ts is a type specification (see `funnyqt.protocols/type-matcher`), `pred` a
  predicate."
  ([objs ts]
     (p-restr-emf objs ts identity))
  ([objs ts pred]
     (let [objs (oset objs)]
       (oset
        (if (seq objs)
          (let [tm (type-matcher (first objs) ts)]
            (filter (every-pred tm pred)
                    objs))
          objs)))))

(defn reachables
  "Returns the ordered set of EObjects reachable from `obj` by via the path
  description `p`.  `obj` may be an EObject or a seq of EObjects."
  [obj p]
  (binding [*p-apply* p-apply-emf
            *p-restr* p-restr-emf]
    (*p-apply* obj p)))

