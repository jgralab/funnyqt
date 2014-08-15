(ns funnyqt.query.tg
  "TG-specific query functions"
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
     (tg/--- v :out nil nil nil nil))
  ([v ts]
     (tg/--- v :out nil nil ts nil))
  ([v ts pred]
     (tg/--- v :out nil nil ts pred)))

(defn <--
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :in nil nil nil nil))
  ([v ts]
     (tg/--- v :in nil nil ts nil))
  ([v ts pred]
     (tg/--- v :in nil nil ts pred)))

(defn <->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout nil nil nil nil))
  ([v ts]
     (tg/--- v :inout nil nil ts nil))
  ([v ts pred]
     (tg/--- v :inout nil nil ts pred)))

(defn --->
  "Returns the vertices reachable from `v` via outgoing incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `-->`, traversal of edges with composition semantics is forbidden."
  ([v]
     (tg/--- v :out
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] nil nil))
  ([v ts]
     (tg/--- v :out
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts nil))
  ([v ts pred]
     (tg/--- v :out
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts pred)))

(defn <---
  "Returns the vertices reachable from `v` via incoming incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<--', traversal of edges with composition semantics is forbidden."
  ([v]
     (tg/--- v :in
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] nil nil))
  ([v ts]
     (tg/--- v :in
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts nil))
  ([v ts pred]
     (tg/--- v :in
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts pred)))

(defn <-->
  "Returns the vertices reachable from `v` via all incidences,
  optionally restricted by `ts` and `pred` (on the edges).  In contrast to
  `<->', traversal of edges with composition semantics is forbidden."
  ([v]
     (tg/--- v :inout
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] nil nil))
  ([v ts]
     (tg/--- v :inout
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts nil))
  ([v ts pred]
     (tg/--- v :inout
             [AggregationKind/NONE AggregationKind/SHARED]
             [AggregationKind/NONE AggregationKind/SHARED] ts pred)))

(defn <>--
  "Aggregation path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout nil [AggregationKind/SHARED AggregationKind/COMPOSITE] nil nil))
  ([v ts]
     (tg/--- v :inout nil [AggregationKind/SHARED AggregationKind/COMPOSITE] ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/SHARED AggregationKind/COMPOSITE] ts pred)))

(defn --<>
  "Aggregation path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout [AggregationKind/SHARED AggregationKind/COMPOSITE] nil nil nil))
  ([v ts]
     (tg/--- v :inout [AggregationKind/SHARED AggregationKind/COMPOSITE] nil ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/SHARED AggregationKind/COMPOSITE] nil ts pred)))

(defn <->--
  "Aggregation-only path expression starting at whole `v`, optionally
  restricted by `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout nil [AggregationKind/SHARED] nil nil))
  ([v ts]
     (tg/--- v :inout nil [AggregationKind/SHARED] ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/SHARED] ts pred)))

(defn --<->
  "Aggregation-only path expression starting at part `v`, optionally restricted
  by `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout [AggregationKind/SHARED] nil nil nil))
  ([v ts]
     (tg/--- v :inout [AggregationKind/SHARED] nil ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/SHARED] nil ts pred)))

(defn <*>--
  "Composition path expression starting at whole `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout nil [AggregationKind/COMPOSITE] nil nil))
  ([v ts]
     (tg/--- v :inout nil [AggregationKind/COMPOSITE] ts nil))
  ([v ts pred]
     (tg/--- v :inout nil [AggregationKind/COMPOSITE] ts pred)))

(defn --<*>
  "Composition path expression starting at part `v`, optionally restricted by
  `ts` and `pred` (on the edges)."
  ([v]
     (tg/--- v :inout [AggregationKind/COMPOSITE] nil nil nil))
  ([v ts]
     (tg/--- v :inout [AggregationKind/COMPOSITE] nil ts nil))
  ([v ts pred]
     (tg/--- v :inout [AggregationKind/COMPOSITE] nil ts pred)))
