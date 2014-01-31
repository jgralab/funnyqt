(ns funnyqt.relational.internal
  (:require [clojure.core.logic           :as ccl]
            [clojure.core.logic.protocols :as cclp]
            [funnyqt.relational.tmp-elem  :as tmp]
            [funnyqt.relational.util      :as ru]
            [funnyqt.utils                :as u]
            [funnyqt.generic              :as g])
  (:import (funnyqt.relational.tmp_elem WrapperElement TmpElement)))

(defn tmp-valueo [m o at val may-override]
  (fn [a]
    (let [go  (cclp/walk a o)
          gat (cclp/walk a at)]
      (cond
       (not (tmp/tmp-or-wrapper-element? go))
       (u/errorf "tmp-valueo: o has to be a ground Tmp/WrapperElement but was %s."
                 go)

       (not (keyword? gat))
       (u/errorf "tmp-valueo: at must be a ground keyword but was %s." gat)

       :else (do (tmp/add-attr go gat val may-override)
                 (ccl/succeed a))))))

(defn tmp-adjo [m o ref ro]
  (fn [a]
    (let [go  (cclp/walk a o)
          gref (cclp/walk a ref)
          gro (cclp/walk a ro)]
      (cond
       (not (tmp/tmp-or-wrapper-element? go))
       (u/errorf "tmp-adjo: o has to be a ground Tmp/WrapperElement but was %s."
                 go)

       (not (keyword? gref))
       (u/errorf "tmp-adjo: ref must be a ground keyword but was %s." gref)

       (or (and (tmp/tmp-or-wrapper-element? go) (tmp/tmp-or-wrapper-element? gro))
           (and (tmp/tmp-element? go)            (ru/fresh? gro)))
       (do (tmp/add-ref go gref ro)
           (ccl/succeed a))

       (and (tmp/wrapper-element? go) (ru/fresh? gro))
       (ccl/to-stream
        (->> (map #(ccl/unify a ro (if (fn? %) (%) %))
                  (concat
                   (map #(tmp/make-wrapper m ro %)
                        (g/adjs (.wrapped-element ^WrapperElement go) gref))
                   [#(let [refed (tmp/make-tmp-element m :element)]
                       (tmp/add-ref go gref ro)
                       refed)]))
             (remove not)))

       :else (u/errorf "unsupported args to tmp-adjo:\n  o = %s\n  ref = %s\n ro = %s"
                       go gref gro)))))
