(ns funnyqt.internal
  "Protocols for implementation internals only.")

(defprotocol IAdjacenciesInternal
  "A protocol for retrieving adjacent elements."
  (adj-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.
  Errors on unknown roles.")
  (adjs-internal [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.
  Returns a reducible collection (see clojure.core.reducers).
  Errors on unknown roles.")
  (adj*-internal [this roles]
    "Gets the adjacent element of `this` navigating `roles`.
  Doesn't error on intermediate unset roles.")
  (adjs*-internal [this roles]
    "Gets all adjacent elements of `this` navigating `roles`.
  Returns a reducible collection (see clojure.core.reducers).
  Doesn't error on intermediate unset roles."))

