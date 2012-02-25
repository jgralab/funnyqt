(ns leiningen.html5-docs.core
  (:use hiccup.core)
  (:use clojure.java.io)
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pp])
  (:import [java.io File]))

(defn files-in [dirpath pattern]
  (for [file (-> dirpath File. file-seq)
        :when (re-matches pattern (.getName file))]
    (.getPath file)))

(def html-header
  "<!DOCTYPE html>")

(defn page-footer []
  [:footer
   [:a {:href "http://www.w3.org/html/logo/"}
    [:img {:src "http://www.w3.org/html/logo/badge/html5-badge-h-solo.png"
           :width "63" :height "64" :alt "HTML5 Powered"
           :title "HTML5 Powered"}]]])

(def css
  "body { margin: 10px;
          padding: 10px;
          font-family:'Palatino Linotype', 'Book Antiqua', Palatino,
                       FreeSerif, serif; }
  h1, h2, h3, h4 { color:#116275; }
  code { font-size:12px;
         font-family: 'DeJaVu Sans Mono', 'Bitstream Vera Sans Mono',
                      'Courier New', Courier, monospace; }
  pre { padding: 5px;
        border: 2px dashed #D1C7AC;
        background-color: #FBFDFF;
        font-size:12px;
        font-family: 'DeJaVu Sans Mono', 'Bitstream Vera Sans Mono',
                     'Courier New', Courier, monospace; }
  a { color: black; }
  a:hover { background-color: #A8DFE6; }
  a:visited { color:DarkSlateGray; }
  section, footer, header { width: auto;
                            float:left;
                            clear:both; }
  #toc { position: fixed; right: 0; top: auto;
         height: 90%;
         width: auto;
         margin: 5px; }
  #toc-listing-outer { height: 85%;
                       overflow: auto; overflow-x: hidden; }
  #toc-listing-inner { border: 2px dashed #D1C7AC;
                       padding: 5px; }")

(defn make-id
  [x]
  (str "ID"
       (-> x
           (clojure.string/replace "<" "SMALLER")
           (clojure.string/replace ">" "GREATER")
           (clojure.string/replace "*" "MUL")
           (clojure.string/replace "+" "PLUS")
           (clojure.string/replace "?" "QMARK"))))

(defn gen-index-page
  "Generates an index page."
  [project nsps]
  (let [pname (or (:html5-docs-name project) (:name project))]
    (html
     html-header
     [:html
      [:head
       [:meta {:charset "utf-8"}]
       [:title (or (:html5-docs-page-title project)
                   (str pname " API Documentation"))]
       [:style {:type "text/css"} css]]
      [:body
       [:header
        [:h1 pname [:small " (version " (:version project) ")"]]
        [:h4 (:description project)]]
       (when-let [url (:url project)]
         [:section
          "For more information, visit the "
          [:a {:href url} pname " Homepage"]
          "."])
       (when-let [lic (:license project)]
         [:section
          pname " is licensed under the "
          [:a {:href (:url lic)} (:name lic)]
          ". " [:small (:comments lic)]])
       [:section {:id "ns-toc"}
        [:h2 "Namespaces"]
        [:table
         (for [nsp nsps]
           [:tr
            [:td [:a {:href (str (name nsp) ".html")}
                  (name nsp)]]
            [:td [:div {:class "ns-toc-entry-desc"}
                  (:doc (meta (find-ns nsp)))]]])]]
       (page-footer)]])))

(defn gen-ns-toc
  "Generate a TOC of the other namespaces.
  nsp is the current namespace, nsps all namespaces."
  [nsp nsps]
  [:section {:id "ns-toc"}
   [:h2 "Other Namespaces"]
   [:table
    (for [onsp nsps
          :when (not= nsp onsp)]
      [:tr
       [:td [:a {:href (str (name onsp) ".html")}
             (name onsp)]]
       [:td [:div {:class "ns-toc-entry-desc"}
             (:doc (meta (find-ns onsp)))]]])
    [:tr
     [:td [:a {:href "index.html"} "Back to Index"]]
     [:td ""]]]])

(defn gen-public-vars-toc
  "Generates a TOC for all public vars in pubs."
  [pubs]
  [:div {:id "toc"}
   [:h2 "Public Vars"]
   [:div {:id "toc-listing-outer"}
    [:div {:id "toc-listing-inner"}
     (for [[s _] pubs]
       [:div
        [:a {:href (str "#" (make-id s))} (escape-html s)]])]]])

(defn indent
  [s]
  (for [line (str/split-lines s)]
    (str "  " line "\n")))

(defn gen-public-vars-details
  "Generates detailed docs for the public vars pubs."
  [pubs]
  [:section {:id "details"}
   [:h2 "Details of Public Vars"]
   (for [[s v] pubs]
     (let [es (escape-html s)
           id (make-id s)]
       [:div {:id id}
        (if (fn? (deref v))
          ;; A Function
          [:div
           [:h3 (if (:macro (meta v)) "Macro: " "Function: ") es]
           [:pre "Arglists:\n=========\n\n"
            (escape-html
             (html
              (binding [pp/*print-miser-width*  60
                        pp/*print-right-margin* 80]
                (map #(let [sig `(~s ~@%)]
                        (indent (with-out-str
                                  (pp/pprint sig))))
                     (:arglists (meta v))))))
            "\nDocstring:\n==========\n\n  "
            (escape-html
             (or (:doc (meta v))
                 "No docs attached."))]]
          ;; A Var
          [:div
           [:h3 (when (:dynamic (meta v)) "Dynamic ") "Var: " es]
           [:pre "  " (escape-html
                       (or (:doc (meta v))
                           "No docs attached."))]])
        ;; Link to sources
        [:a {:href (str "https://hg.uni-koblenz.de/horn/funnyqt/file/tip/src/"
                        (.replaceFirst ^String (:file (meta v)) ".*/funnyqt/src/" "")
                        "#l" (:line (meta v)))} "View Source"]
        " "
        [:a {:href "#top"} "Back to top"]]))])

(defmacro with-err-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn html5-docs
  [project]
  (let [docs-dir (or (:html5-docs-docs-dir project) "docs")
        err (with-err-str
              (println "Loading Files")
              (println "=============")
              (println)
              (doseq [f (files-in (or (:html5-docs-source-path project)
                                      (:source-path project))
                                  #".*\.clj")]
                (println "  -" f)
                (load-file f))
              (let [nsps (filter #(and
                                   (re-matches (:html5-docs-ns-includes project) (name %))
                                   (not (re-matches (:html5-docs-ns-excludes project) (name %))))
                                 (sort (map ns-name (all-ns))))
                    index-file (str docs-dir "/index.html")]
                (clojure.java.io/make-parents index-file)
                (spit index-file (gen-index-page project nsps))
                (println)
                (println "Generating Documentation")
                (println "========================")
                (println)
                (doseq [nsp nsps]
                  (spit (let [hf (str docs-dir "/" (name nsp) ".html")]
                          (println "  -" hf)
                          hf)
                        (html
                         html-header
                         [:html
                          [:head
                           [:meta {:charset "utf-8"}]
                           [:title (str "Namespace " nsp)]
                           [:style {:type "text/css"} css]]
                          (let [pubs (filter (fn [[_ v]]
                                               (:file (meta v)))
                                             (sort (ns-publics nsp)))]
                            [:body
                             ;; Namespace Header
                             [:article {:id "top"}
                              [:header
                               [:h1 "Namspace "(name nsp)]
                               [:h4 (:doc (meta (find-ns nsp)))]
                               [:details
                                [:summary "Usage Documentation"]
                                [:pre
                                 (or (escape-html (:long-doc (meta (find-ns nsp))))
                                     "Currently, there're no namespace docs.")]]]
                              ;; Namespace TOC
                              (gen-ns-toc nsp nsps)
                              ;; Contents
                              (gen-public-vars-details pubs)
                              ;; TOC of Vars
                              (gen-public-vars-toc pubs)]
                             (page-footer)])]))))
              (println)
              (println "Finished."))]
    (when (seq err)
      (println "Some warnings occured, see html5-docs.log.")
      (spit "html5-docs.log" err))))
