(ns leiningen.html5-docs
  "Generate HTML5 API docs."
  (:use [leiningen.compile :only [eval-in-project]])
  (:require clojure.java.shell)
  (:require clojure.string))

(defn html5-docs [project]
  (eval-in-project
   project
   `(leiningen.html5-docs.core/html5-docs '~project)
   nil nil '(require 'leiningen.html5-docs.core)))
