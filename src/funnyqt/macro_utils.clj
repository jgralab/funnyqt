(ns funnyqt.macro-utils
  "Macro utilities used by various namespaces."
  (:use funnyqt.utils))

;;# Expansion context

(def ^{:dynamic true}
  *expansion-context* :external)

(defn expansion-context-defn-maybe [n]
  "Return a definition form usable to be spliced in depending on
  *expansion-context*."
  (case *expansion-context*
    :internal [n]
    :external `[clojure.core/defn ~n ~(meta n)]
    (errorf "Unknown expansion context: %s" *expansion-context*)))

