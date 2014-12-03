(ns user
  (:require [funnyqt.generic     :as g]
            [funnyqt.utils       :as u]
            [funnyqt.tg          :as tg]
            [funnyqt.emf         :as emf]
            [funnyqt.query       :as q]
            [funnyqt.pmatch      :as pmatch]
            [funnyqt.polyfns     :as poly]
            [funnyqt.in-place    :as ip]
            [funnyqt.model2model :as m2m]
            [funnyqt.bidi        :as bidi]
            [funnyqt.visualization :as viz]
            [vinyasa.inject      :as vinj]))

;; Inject into namespace .
(vinj/in [funnyqt.visualization print-model])
