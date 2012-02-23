(ns leiningen.gen-docs
  "Generate HTML docs."
  (:use [leiningen.compile :only [eval-in-project]])
  (:require clojure.java.shell)
  (:require clojure.string))

(defn gen-docs [project]
  (let [version-string {:version (:version project)
                        :revision (clojure.string/trim
                                   (:out (clojure.java.shell/sh
                                          "git" "rev-parse" "HEAD")))}]
    (eval-in-project
     project
     `(leiningen.gen-docs.core/gen-docs ~version-string)
     nil nil '(require 'leiningen.gen-docs.core))))
