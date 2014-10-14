(ns funnyqt.internal
  "Protocols for implementation internals only.")

(defprotocol IAdjacenciesInternal
  "A protocol for retrieving adjacent elements."
  (adjs-internal [el role allow-unknown-role single-valued]
    "Returns the collection of elements adjacent to `el` by reference `role`.

  If `allow-unknown-role` is true, simply returns an empty collection if `role`
  is undefined for `el`.  Else, throws an exception in that case.

  If `single-valued` is true and `role` is a multi-valued role, throws an
  exception."))
