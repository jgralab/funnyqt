(defproject funnyqt "0.0.1"
  :description "A model querying and transformation library.

  Everything's totally pre-pre-pre-alpha and subject to frequent, incompatible
  changes.  Ok, you've been warned, but have fun anyway. :-)"
  :dependencies [[org.clojure/clojure "1.3.0"]
		 [de.uni-koblenz.ist/jgralab "[5.3.1,)"]
                 [org.clojure/core.logic "[0.6.1,)"]
                 [org.clojure/core.match "[0.2.0-alpha8,)"]
		 [ordered "[0.3,)"]
                 [org.clojure/tools.macro "[0.1.1,)"]
                 ;; EMF Stuff (the xmi-jar pulls in common and ecore, too)
                 [org.eclipse.emf/org.eclipse.emf.ecore.xmi "[2.5.0.v20100521-1846,)"]]
  :dev-dependencies [[hiccup "0.3.7"]]
  ;; Don't put version control dirs into the jar
  :jar-exclusions [#"(?:^|/).(svn|hg|git)/" ;; exclude version control stuff
                   #"leiningen/"            ;; exclude lein plugins
                   ]
  :warn-on-reflection true
  :jvm-opts ["-Xmx2G"]
  ;; Pretty print results at the REPL
  :project-init (require 'clojure.pprint)
  :repl-options [:print clojure.pprint/pprint])
