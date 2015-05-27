(ns funnyqt.edn-test
  (:require [funnyqt.edn :refer :all]
            [funnyqt.tg  :as tg]))

(def g (tg/load-graph "test/input/familygraph.tg"))

