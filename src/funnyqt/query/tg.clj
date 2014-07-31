(ns funnyqt.query.tg
  "EMF-specific query functions"
  (:require [flatland.ordered.set :as os]
            [funnyqt.tg           :as tg]
            [funnyqt.query        :as q]
            [funnyqt.utils        :as u])
  (:import
   (de.uni_koblenz.jgralab.schema AggregationKind)))

;;# Regular Path Expressions

(defn -->
  "Returns the vertices reachable from `v` via outgoing incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (--> v nil nil))
  ([v ts]
     (--> v ts nil))
  ([v ts pred]
     (tg/--- v :out nil nil ts pred)))

(defn <--
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<-- v nil nil))
  ([v ts]
     (<-- v ts nil))
  ([v ts pred]
     (tg/--- v :in nil nil ts pred)))

(defn <->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<-> v nil nil))
  ([v ts]
     (<-> v ts nil))
  ([v ts pred]
     (tg/--- v :inout nil nil ts pred)))

(defn --->
  "Returns the vertices reachable from `v` via outgoing incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `-->`, traversal of edges with aggregation semantics is forbidden."
  ([v]
     (---> v nil nil))
  ([v ts]
     (---> v ts nil))
  ([v ts pred]
     (tg/--- v :out [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <---
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<--', traversal of edges with aggregation semantics is forbidden."
  ([v]
     (<--- v nil nil))
  ([v ts]
     (<--- v ts nil))
  ([v ts pred]
     (tg/--- v :in [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <-->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<->', traversal of edges with aggregation semantics is forbidden."
  ([v]
     (<--> v nil nil))
  ([v ts]
     (<--> v ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/NONE] [AggregationKind/NONE] ts pred)))

(defn <>--
  "Aggregation path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (<>-- v nil nil))
  ([v ts]
     (<>-- v ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/SHARED AggregationKind/COMPOSITE] ts pred)))

(defn --<>
  "Aggregation path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (--<> v nil nil))
  ([v ts]
     (--<> v ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/SHARED AggregationKind/COMPOSITE] nil ts pred)))

(defn <_>--
  "Aggregation-only path expression starting at whole `v`, optionally
  restricted by `ts` and `pred` (on the edges)."
  ([v]
     (<_>-- v nil nil))
  ([v ts]
     (<_>-- v ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/SHARED] ts pred)))

(defn --<_>
  "Aggregation-only path expression starting at part `v`, optionally restricted
  by `ts` and `pred` (on the edges)."
  ([v]
     (--<_> v nil nil))
  ([v ts]
     (--<_> v ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/SHARED] nil ts pred)))

(defn <*>--
  "Composition path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (<*>-- v nil nil))
  ([v ts]
     (<*>-- v ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/COMPOSITE] ts pred)))

(defn --<*>
  "Composition path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (--<*> v nil nil))
  ([v ts]
     (--<*> v ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/COMPOSITE] nil ts pred)))
