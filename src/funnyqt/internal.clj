(ns funnyqt.internal
  "Protocols for implementation internals only.")

(defprotocol IAdjacenciesInternal
  "A protocol for retrieving adjacent elements."
  (adjs-internal [el role allow-unknown-role single-valued]
    "Returns the collection of elements adjacent to `el` by reference `role`."))
