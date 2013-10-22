(ns funnyqt.utils.tg
  (:require [funnyqt.tg :as tg])
  (:import (de.uni_koblenz.jgralab.schema VertexClass EdgeClass Attribute Schema)))

(defn ^:private no-nils [coll]
  (doall (remove nil? coll)))

(defmacro schema-ns-generator
  "A helper macro for generating a schema-specific API in some namespace.
  The namespace is named `nssym`.  If that's nil, then use the current
  namespace.

  `vc-fn` has to be a function that receives a VertexClass and returns a valid
  definition-form, e.g., a (defn stuff-with-that-vertex-class [...] ...).

  `ec-fn` has to be a function that receives an EdgeClass and returns a valid
  definition-form.

  `attr-fn` is a function that receives an attribute name (as keyword) and a
  set of attributed element classes that have such an attribute.  The function
  should return a valid definition form.

  `role-fn` is a function that receives a role name (as keyword) and a set of
  vertex classes that have such a role.  Again, the function should return a
  valid definition form."
  [schema-file nssym vc-fn ec-fn attr-fn role-fn]
  (let [^Schema schema (tg/load-schema
                        (if (.exists (clojure.java.io/file schema-file))
                          schema-file
                          (clojure.java.io/resource schema-file)))
        atts (atom {}) ;; map from attribute names given as keywords to set
        ;; of attributed element classes that have it
        refs (atom {}) ;; map from role names given as keywords to set of
        ;; [edgeclass dir] tuples that have it
        old-ns *ns*]
    `(do
       ~@(when nssym
           `[(ns ~nssym)])
       ;; The schema specific ones
       ~@(concat
          (no-nils
           (for [^VertexClass vc (seq (-> schema .getGraphClass .getVertexClasses))]
             (do
               (doseq [a (mapv #(keyword (.getName ^Attribute %))
                               (seq (.getOwnAttributeList vc)))]
                 (swap! atts
                        #(update-in %1 [%2] conj vc)
                        a))
               (when vc-fn
                 ((resolve vc-fn) vc)))))
          (no-nils
           (for [^EdgeClass ec (seq (-> schema .getGraphClass .getEdgeClasses))]
             (do
               ;; Collect attributes
               (doseq [a (mapv #(keyword (.getName ^Attribute %))
                               (seq (.getOwnAttributeList ec)))]
                 (swap! atts
                        #(update-in %1 [%2] conj ec)
                        a))
               ;; Collect roles
               (let [from-vc (-> ec .getFrom .getVertexClass)
                     from-rn (-> ec .getFrom .getRolename)]
                 (when (seq from-rn)
                   (swap! refs #(update-in %1 [from-rn] conj from-vc))))
               (let [to-vc (-> ec .getTo .getVertexClass)
                     to-rn (-> ec .getTo .getRolename)]
                 (when (seq to-rn)
                   (swap! refs #(update-in %1 [to-rn] conj to-vc))))
               (when ec-fn
                 ((resolve ec-fn) ec)))))
          (no-nils
           (when attr-fn
             (for [[a owners] @atts]
               ((resolve attr-fn) a owners))))
          (no-nils
           (when role-fn
             (for [[role owners] @refs]
               ((resolve role-fn) role owners)))))
       (in-ns '~(ns-name old-ns)))))
