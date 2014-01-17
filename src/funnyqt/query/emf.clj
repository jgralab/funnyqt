(ns funnyqt.query.emf
  "More sophisticated constructs for querying EMF models."
  (:require [clojure.core.reducers :as r]
            [funnyqt.emf           :as emf]
            [funnyqt.utils         :as u]
            [funnyqt.query         :as q]
            [funnyqt.generic       :as g]
            [flatland.ordered.set  :as os]
            [flatland.ordered.map  :as om])
  (:import
   (org.eclipse.emf.ecore EObject EReference EStructuralFeature)))

;;# Regular Path Descriptions

(defn <>--
  "Returns the (direct) contents of EObject`obj` restricted by the type
  specification `ts` (see `funnyqt.generic/type-matcher` for details).  `obj`
  may also be a collection of EObjects."
  ([obj]
     (u/oset (mapcat emf/econtents (u/oset obj))))
  ([obj ts]
     (u/oset (mapcat #(emf/econtents % ts) (u/oset obj)))))

(defn --->
  "Returns the EObjects cross-referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `eref-matcher` for
  details).  `obj` may also be a collection of EObjects.  In EMF,
  cross-referenced means referenced by a non-containment EReference."
  ([obj]
     (u/oset (mapcat emf/ecrossrefs (u/oset obj))))
  ([obj rs]
     (u/oset (mapcat #(emf/ecrossrefs % rs) (u/oset obj)))))

(defn -->
  "Returns the EObjects referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `eref-matcher` for
  details).  `obj` may also be a collection of EObjects.  In contrast to
  `--->`, this function includes both cross-references and containments."
  ([obj]
     (u/oset (mapcat emf/erefs (u/oset obj))))
  ([obj rs]
     (u/oset (mapcat #(emf/erefs % rs) (u/oset obj)))))

(defn --<>
  "Returns a seq containing `obj`s container.  If there's none,
  returns the empty set."
  [obj]
  (u/oset (mapcat #(when-let [c (emf/econtainer %)]
                     [c])
                  (u/oset obj))))

(defn <---
  "Returns all EObjects cross-referencing `obj` with a reference matching the
  reference specification `rs` (see `eref-matcher` for details).  `obj` may also
  be a collection of EObjects, in which case all objects cross-referencing any
  of the objects in `obj` is returned.  In EMF, cross-referenced means
  referenced by a non-containment EReference."
  ([obj]
     (u/oset (mapcat emf/inv-ecrossrefs (u/oset obj))))
  ([obj rs]
     (u/oset (mapcat #(emf/inv-ecrossrefs % rs) (u/oset obj))))
  ([obj rs container]
     (u/oset (mapcat #(emf/inv-ecrossrefs % rs container) (u/oset obj)))))

(defn <--
  "Returns all EObjects referencing `obj` with a reference matching the
  reference specification `rs` (see `eref-matcher` for details).  `obj` may also
  be a collection of EObjects, in which case all objects referencing any of the
  objects in `obj` is returned.  In contrast to `<---', this function includes
  both cross-references and containments."
  ([obj]
     (u/oset (mapcat emf/inv-erefs (u/oset obj))))
  ([obj rs]
     (u/oset (mapcat #(emf/inv-erefs % rs) (u/oset obj))))
  ([obj rs container]
     (u/oset (mapcat #(emf/inv-erefs % rs container) (u/oset obj)))))

(defn ^:private p-apply-emf
  [obj p]
  (cond
   ;; funs: --->
   (fn? p) (p obj)
   ;; funs with params: [---> :foo], [p-alt :foo :bar]
   (coll? p) (apply (first p) obj (rest p))
   ;; EReference names
   (u/prop-name? p) (u/oset (mapcat #(emf/erefs % p) (u/oset obj)))
   :else (u/errorf "Don't know how to apply %s." p)))

(defn ^:private p-restr-emf
  "EObject restriction concerning `ts` and `pred` on each object in `objs`.
  ts is a type specification (see `funnyqt.generic/type-matcher`), `pred` a
  predicate."
  ([objs ts]
     (p-restr-emf objs ts identity))
  ([objs ts pred]
     (let [objs (u/oset objs)]
       (u/oset
        (if (seq objs)
          (let [tm (g/type-matcher (first objs) ts)]
            (filter (every-pred tm pred)
                    objs))
          objs)))))

(defn reachables
  "Returns the ordered set of EObjects reachable from `obj` by via the path
  description `p`.  `obj` may be an EObject or a seq of EObjects."
  [obj p]
  (binding [q/*p-apply* p-apply-emf
            q/*p-restr* p-restr-emf]
    (q/*p-apply* obj p)))
