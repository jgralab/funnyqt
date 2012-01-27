(ns leiningen.gen-docs
  "Generate HTML docs."
  (:use [leiningen.compile :only [eval-in-project]]))

(defn gen-docs [project]
  (eval-in-project
   project
   '(leiningen.gen-docs.core/gen-docs)
   nil nil '(require 'leiningen.gen-docs.core)))
