(defproject funnyqt "1.0.6"
  :description "A model querying and transformation library for TGraphs and EMF
  models developed as part of Tassilo Horn's dissertation studies."
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.cache "0.6.5"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [de.uni-koblenz.ist/jgralab "8.1.0"]
                 [org.clojure/core.logic "0.8.11"]
                 [org.flatland/ordered "1.5.4"]
                 [org.clojure/tools.macro "0.1.5"]
                 [emf-xsd-sdk "2.11.1"]
                 [inflections "0.12.2" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[im.chit/vinyasa "0.4.7"]]
                   ;; Don't omit stack traces
                   :jvm-opts ^:replace ["-Xmx1G" "-XX:-OmitStackTraceInFastThrow"]}}
  ;; Don't put version control dirs into the jar
  :jar-exclusions [#"(?:^|/).(svn|hg|git)/"]
  :resource-paths ["resources"]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace ["-server" "-XX:+AggressiveOpts" "-Xmx1G"]
  :license {:name "GNU General Public License, Version 3 (or later)"
            :url "http://www.gnu.org/licenses/gpl.html"
            :distribution :repo}
  :url "http://funnyqt.org"
  :repl-options {:init (println "Welcome to FunnyQT!")}
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stuff specific to generating API docs
  :html5-docs-name "FunnyQT"
  ;; :html5-docs-page-title nil ;; => "FunnyQT API Documentation"
  ;; :html5-docs-source-path "src/"
  :html5-docs-docset-icons ["logos/icon16.png" "logos/icon32.png"]
  :html5-docs-ns-includes #"^funnyqt\..*"
  :html5-docs-ns-excludes #".*\.(internal|tmp-elem|relational\.util)$"
  ;; :html5-docs-docs-dir nil ;; => "docs"
  :html5-docs-repository-url #(str "https://github.com/jgralab/funnyqt/blob/v"
                                   (:version %)))
