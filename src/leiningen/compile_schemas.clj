(ns leiningen.compile-schemas
  "Generate & compile classes from the schema-tgs."
  (:use [leiningen.compile :only [eval-in-project]]))

(defn compile-schemas [project]
  (eval-in-project
   project
   '(leiningen.compile-schemas.core/compile-schemas)
   nil nil '(require 'leiningen.compile-schemas.core)))
