(ns leiningen.compile-schemas.core
  (:import [de.uni_koblenz.jgralab.utilities.tgschema2java TgSchema2Java]))

(defn compile-schemas []
  (TgSchema2Java/main
   (into-array String ["--compile"
                       "--path" "classes/"
                       "--schema" "schemas/xml-schema.tg"
                       "--without-types"])))
