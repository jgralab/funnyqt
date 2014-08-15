(ns funnyqt.query.emf
  "EMF-specific query functions"
  (:require [clojure.core.reducers :as r]
            [funnyqt.utils         :as u]
            [flatland.ordered.set  :as os]
            [funnyqt.emf           :as emf]))

;;# Regular Path Expressions

(defn <>--
  "Returns the (direct) contents of EObject `obj` restricted by the reference
  specification `rs` (see `funnyqt.emf/eref-matcher` for details).  `obj` may
  also be a collection of EObjects."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(emf/econtentrefs % nil) (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(emf/econtentrefs % rs)  (u/oset obj)))))

(defn --->
  "Returns the EObjects cross-referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `funnyqt.emf/eref-matcher`
  for details).  `obj` may also be a collection of EObjects.  In EMF,
  cross-referenced means referenced by a non-containment EReference."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(emf/ecrossrefs % nil) (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(emf/ecrossrefs % rs)  (u/oset obj)))))

(defn -->
  "Returns the EObjects referenced by `obj` where the references may be
  restricted by `rs`, a reference specification (see `funnyqt.emf/eref-matcher`
  for details).  `obj` may also be a collection of EObjects.  In contrast to
  `--->`, this function includes both cross-references and containments."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(emf/erefs % nil) (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(emf/erefs % rs)  (u/oset obj)))))

(defn --<>
  "Returns a seq containing `obj`s container.  If there's none,
  returns the empty set."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(when-let [c (emf/econtainer %)]
                                         [c])
                                      (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(when-let [c (emf/econtainer % rs)]
                                         [c])
                                      (u/oset obj)))))

(defn <---
  "Returns all EObjects cross-referencing `obj` with a reference matching the
  reference specification `rs` (see `funnyqt.emf/eref-matcher` for details).
  `obj` may also be a collection of EObjects, in which case all objects
  cross-referencing any of the objects in `obj` is returned.  In EMF,
  cross-referenced means referenced by a non-containment EReference.

  If no `container` is given, then only the opposite refs of `obj` are checked.
  Else, all objects in `container` are tested if they reference `obj` with a
  reference matching `rs`.  `container` may be either an EObject, a Resource, a
  ResourceSet, or a collection of EObjects.  For the former three, direct and
  indirect contents are checked, for collections only direct contents."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(emf/inv-ecrossrefs % nil nil) (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(emf/inv-ecrossrefs % rs  nil) (u/oset obj))))
  ([obj rs container]
     (into (os/ordered-set) (r/mapcat #(emf/inv-ecrossrefs % rs  container) (u/oset obj)))))

(defn <--
  "Returns all EObjects referencing `obj` with a reference matching the
  reference specification `rs` (see `funnyqt.emf/eref-matcher` for details).
  `obj` may also be a collection of EObjects, in which case all objects
  referencing any of the objects in `obj` is returned.  In contrast to `<---',
  this function includes both cross-references and containments.

  If no `container` is given, then only the opposite refs of `obj` are checked.
  Else, all objects in `container` are tested if they reference `obj` with a
  reference matching `rs`.  `container` may be either an EObject, a Resource, a
  ResourceSet, or a collection of EObjects.  For the former three, direct and
  indirect contents are checked, for collections only direct contents."
  ([obj]
     (into (os/ordered-set) (r/mapcat #(emf/inv-erefs % nil nil) (u/oset obj))))
  ([obj rs]
     (into (os/ordered-set) (r/mapcat #(emf/inv-erefs % rs  nil) (u/oset obj))))
  ([obj rs container]
     (into (os/ordered-set) (r/mapcat #(emf/inv-erefs % rs container) (u/oset obj)))))
