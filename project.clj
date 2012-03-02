(defproject funnyqt "0.1.0"
  :description "A model querying and transformation library for TGraphs and EMF
  models developed as part of Tassilo Horn's dissertation studies.

  Everything's totally pre-pre-pre-alpha and subject to frequent, incompatible
  changes.  You've been warned, but have fun anyway."
  :dependencies [[org.clojure/clojure "1.4.0-beta1"]
		 [de.uni-koblenz.ist/jgralab "[6.1.2,)"]
                 [org.clojure/core.logic "0.6.7"]
                 [org.clojure/core.match "0.2.0-alpha9"]
		 [ordered "1.0.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.macro "0.1.1"]
                 [emf-xsd-sdk "2.7.1"]]
  :dev-dependencies [[hiccup "0.3.7"]]
  ;; Don't put version control dirs into the jar
  :jar-exclusions [#"(?:^|/).(svn|hg|git)/" ;; exclude version control stuff
                   #"leiningen/"            ;; exclude lein plugins
                   ]
  :warn-on-reflection true
  :jvm-opts ["-Xmx2G"]
  :license {:name "GNU General Public License, Version 3"
            :url "http://www.gnu.org/copyleft/gpl.html"
            :distribution :repo}
  :url "https://github.com/jgralab/funnyqt"
  ;; Stuff specific to generating API docs
  :html5-docs-name "FunnyQT"
  :html5-docs-page-title nil ;; => "FunnyQT API Documentation"
  :html5-docs-source-path "src/funnyqt"
  :html5-docs-ns-includes #"^funnyqt\..*"
  :html5-docs-ns-excludes #".*\.test\..*"
  :html5-docs-docs-dir nil ;; => "docs"
  :html5-docs-repository-src-url "https://github.com/jgralab/funnyqt/blob/master/src"
  ;; Pretty print results at the REPL
  :project-init (require 'clojure.pprint)
  :repl-options [:print clojure.pprint/pprint])
