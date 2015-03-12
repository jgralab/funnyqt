(ns funnyqt.visualization
  "Model visualization functions.

Using the function `print-model`, TGraph and EMF models can be visualized,
either in a window or by printing them to PDF/PNG/JPG/SVG documents."
  (:require clojure.java.shell
            [clojure.string  :as str]
            [funnyqt.generic :as g]
            [funnyqt.emf     :as emf]
            [funnyqt.tg      :as tg]
            [funnyqt.query   :as q]
            [funnyqt.utils   :as u])
  (:import
   (de.uni_koblenz.jgralab Vertex Edge Graph AttributedElement)
   (de.uni_koblenz.jgralab.schema Attribute EdgeClass AggregationKind)
   (org.eclipse.emf.ecore EObject EAttribute EReference)
   (org.eclipse.emf.ecore.resource Resource ResourceSet)
   (java.awt Image)
   (javax.swing ImageIcon)))

;;* Visualization

;;** Generic stuff

(def ^{:dynamic true
       :doc "A set of elements to be included in the visualization.  This is
  not to be used directly but set by `print-model` according to its :include
  option."}
  *included*)

(def ^{:dynamic true
       :doc "A set of elements to be excluded in the visualization.  This is
  not to be used directly but set by `print-model` according to its :exclude
  option."}
  *excluded*)

(def ^{:dynamic true
       :doc "A map from predicates to set of attributes which should not be
  printed for elements that match the predicate."}
  *excluded-attributes*)

(def ^{:dynamic true
       :doc "A set of elements to be printed in red instead of black.  It might
  also be a predicate receiving a model element.  This is not to be used
  directly but set by `print-model` according to its :mark option."}
  *marked*)

(def ^{:dynamic true
       :doc "A map of the form {pred node-attrs}.  If (pred el) returns true,
  node-attrs is appended to its node definition.  Useful for colorizing etc."}
  *node-attrs*)

(def ^{:dynamic true
       :doc "A boolean determining if class names should be printed fully
  qualified or not.  This is not to be used directly but set by `print-model`
  according to its :qualified-names option."}
  *print-qualified-names*)

(defn ^:private dot-included? [o]
  (and (or (not *included*) ;; Not set ==> all are included
           (*included* o))
       (not (*excluded* o))))

(defn ^:private dot-escape [s]
  (if (nil? s)
    "null"
    (let [r (-> (str s)
                (clojure.string/replace "\n" "\\n")
                (clojure.string/replace "<" "&lt;")
                (clojure.string/replace ">" "&gt;")
                (clojure.string/replace "\"" "\\\"")
                (clojure.string/replace "{" "\\{")
                (clojure.string/replace "}" "\\}"))]
      (if (string? s)
        (str "\\\"" r "\\\"")
        r))))

(defn ^:private dot-options [opts]
  (letfn [(update [m k v]
            (if (get m k)
              m
              (assoc m k v)))]
    (let [m (apply hash-map opts)
          ;; :name is special and no DOT attr, so remove it
          gname (or (:name m) "Model")
          m (dissoc m :name)
          ;; ditto for :include
          include (:include m)
          m (dissoc m :include)
          ;; ditto for :exclude
          exclude (:exclude m)
          m (dissoc m :exclude)
          ;; ditto for :excluded-attributes
          excluded-attibutes (:excluded-attributes m)
          m (dissoc m :excluded-attributes)
          ;; ditto for :mark
          mark (:mark m)
          m (dissoc m :mark)
          ;; ditto for :node-attrs
          node-attrs (:node-attrs m)
          m (dissoc m :node-attrs)
          ;; ditto for :qualified-names
          qnames (:qualified-names m)
          m (dissoc m :qualified-names)
          ;; Add default values
          m (update m :ranksep 1.5)]
      (with-meta m
        {:name gname, :include include, :exclude exclude,
         :excluded-attributes excluded-attibutes, :mark mark,
         :node-attrs node-attrs, :qualified-names qnames}))))

(defn ^:private add-node-attrs [el]
  (let [attrs (str/join ", "
                        (remove not (map (fn [[pred attrs]]
                                           (when (pred el) attrs))
                                         *node-attrs*)))]
    (if (seq attrs)
      (str ", " attrs)
      attrs)))

(defn ^:private excluded-attributes [el]
  (set (mapcat (fn [[pred attrs]]
                 (when (pred el) attrs))
               *excluded-attributes*)))

;;** EMF stuff

(def ^{:private true, :dynamic true
       :doc "Opposite refs: those are not dotted, cause we already
  printed them from the other direction."}
  *emf-opposite-refs*)

(defn ^:private emf-dot-id [o]
  (str "O" (Integer/toHexString (hash o))))

(defn ^:private emf-dot-attributes [^EObject eo]
  (reduce str
          (for [^EAttribute attr (.getEAllAttributes (.eClass eo))
                :let [n (.getName attr)
                      ex-attrs (excluded-attributes eo)]
                :when (not (and ex-attrs (contains? ex-attrs (keyword n))))]
            (str n " = " (dot-escape (try (emf/eget eo (keyword n))
                                          (catch Exception _
                                            (emf/eget eo (keyword n)))))
                 "\\l"))))

(defn ^:private emf-dot-eobject [^EObject eo]
  (when (dot-included? eo)
    (let [h (emf-dot-id eo)]
      (str "  " h
           " [label=\"{{:" (if *print-qualified-names*
                             (g/qname eo)
                             (.getName (.eClass eo)))
           "}|"
           (emf-dot-attributes eo)
           "}\", shape=record, fontname=Sans, fontsize=14, "
           "color=" (if (*marked* eo)
                      "red" "black")
           (add-node-attrs eo)
           "];\n"))))

(defn ^:private emf-dot-contentrefs [^EObject eo]
  (let [h (emf-dot-id eo)
        dist (atom [2.0 4.0])]
    (reduce str
            (for [^EReference ref (seq (.getEAllContainments (.eClass eo)))
                  :let [oref (.getEOpposite ref)
                        n (.getName ref)]
                  t (when-let [x (emf/eget eo ref)]
                      (if (coll? x) x [x]))
                  :when (dot-included? t)]
              (do
                (swap! dist (fn [[x y]] [y x]))
                (str "  " h " -> " (emf-dot-id t)
                     " [dir=both, arrowtail=diamond, fontname=Sans, "
                     "labelangle=0, labeldistance= " (first @dist) ", "
                     "label=\"                    \", "
                     "headlabel=\"" n "\""
                     (when oref
                       (str ", taillabel=\"" (.getName oref) "\""))
                     "];\n"))))))

(defn ^:private emf-dot-crossrefs [^EObject eo]
  (let [h (emf-dot-id eo)
        dist (atom  [2.0 4.0])]
    (reduce str
            (for [^EReference ref (.getEAllReferences (.eClass eo))
                  :when (not (q/member? ref @*emf-opposite-refs*))
                  :when (not (or (.isContainment ref)
                                 (.isContainer ref)))
                  :let [oref (.getEOpposite ref)]
                  t (emf/ecrossrefs eo ref)
                  :when (dot-included? t)
                  :let [h2 (emf-dot-id t)]]
              (do
                (when oref
                  (swap! *emf-opposite-refs* conj oref))
                (swap! dist (fn [[x y]] [y x]))
                (str "  " h " -> " h2
                     " [dir="
                     (if oref "none" "forward")
                     ", fontname=Sans, "
                     "labelangle=0, labeldistance= " (first @dist) ", "
                     "label=\"                    \", "
                     "headlabel=\"" (.getName ref) "\""
                     (when oref
                       (str ", taillabel=\"" (.getName oref) "\""))
                     "];\n"))))))

(defn ^:private emf-dot-ereferences [eo]
  (when (dot-included? eo)
    (str (emf-dot-contentrefs eo)
         (emf-dot-crossrefs eo))))

(defn ^:private emf-dot-model [m]
  (str
   (reduce str
           (map emf-dot-eobject
                (emf/eallcontents m)))
   (binding [*emf-opposite-refs* (atom #{})]
     (reduce str
             (map emf-dot-ereferences
                  (emf/eallcontents m))))))

;;** TGraph stuff

(defn ^:private tg-dot-attributes [^AttributedElement elem]
  (reduce str
          (for [^Attribute attr (.getAttributeList (.getAttributedElementClass elem))
                :let [n (.getName attr)
                      ex-attrs (excluded-attributes elem)]
                :when (not (and ex-attrs (contains? ex-attrs (keyword n))))]
            (str n " = " (dot-escape (tg/value elem (keyword n))) "\\l"))))

(defn ^:private tg-dot-vertex [^Vertex v]
  (when (dot-included? v)
    (str "  v" (tg/id v)
         " [label=\"{{v" (tg/id v) ": "
         (if *print-qualified-names*
           (g/qname v)
           (.getSimpleName (.getAttributedElementClass v)))
         "}|"
         (tg-dot-attributes v)
         "}\", shape=record, fontname=Sans, fontsize=14, "
         "color=" (if (*marked* v) "red" "black")
         (add-node-attrs v)
         "];\n")))

(defn tg-role-name [^de.uni_koblenz.jgralab.schema.IncidenceClass ic]
  (when ic
    (let [r (.getRolename ic)]
      (if (seq r)
        r
        (first (remove nil? (map tg-role-name
                                 (.getOwnSubsettedIncidenceClasses ic))))))))

(defn ^:private tg-dot-edge [^Edge e]
  (when (and (not (*excluded* e))
             (dot-included? (tg/alpha e))
             (dot-included? (tg/omega e)))
    (let [^EdgeClass ec (.getAttributedElementClass e)
          fr  (tg-role-name (.getFrom ec))
          fak (-> ec .getFrom .getAggregationKind)
          tr  (tg-role-name (.getTo ec))
          tak (-> ec .getTo   .getAggregationKind)]
      (str "  v" (tg/id (tg/alpha e)) " -> v" (tg/id (tg/omega e))
           " [id=e" (tg/id e) ", label=\"e" (tg/id e) ": "
           (if *print-qualified-names*
             (g/qname e)
             (.getSimpleName (.getAttributedElementClass e)))
           "\\l"
           (tg-dot-attributes e)
           "\", dir=both, fontname=Sans, "
           "labelangle=0, labeldistance=3.0, "
           "color=" (if (*marked* e) "red" "black")
           (when (seq fr) (str ", taillabel=\"" fr "\""))
           (when (seq tr) (str ", headlabel=\"" tr "\""))
           (condp = fak
             AggregationKind/COMPOSITE ", arrowhead=diamond"
             AggregationKind/SHARED    ", arrowhead=odiamond"
             AggregationKind/NONE      ", arrowhead=normal")
           (condp = tak
             AggregationKind/COMPOSITE ", arrowtail=diamond"
             AggregationKind/SHARED    ", arrowtail=odiamond"
             AggregationKind/NONE      ", arrowtail=none")
           "];\n"))))

(defn ^:private tg-dot-model [g]
  (str
   (reduce str
           (map tg-dot-vertex
                (tg/vseq g)))
   (reduce str
           (map tg-dot-edge
                (tg/eseq g)))))

;;** Main

(def dot-model-fns
  "A map from model representation class to function creating a DOT string with
  all elements of this model.  The function accepts the model as its only
  argument.  It should produce a DOT string of the form:

    el1 [<dot-attributes>];
    ...
    elN [<dot-attributes>];
    el1 -> el2 [<dot-attributes>];
    ...
    elI -> elJ [<dot-attributes>];

  That is, it should contain all edges and vertices to be visualized.
  The DOT syntax is described at http://www.graphviz.org/Documentation.php.

  The surrounding

    digraph SomeName { <dot-attributes>; ... }

  is added automatically.

  By default, there is an entry for an EMF Resource, an EMF ResourceSet and one
  for a JGraLab Graph."
  {Resource    emf-dot-model
   ResourceSet emf-dot-model
   Graph       tg-dot-model})

(defn ^:private dot-model [m opts]
  (let [opts (dot-options opts)]
    (when (and (:name (meta opts))
               (re-matches #"\S*(\s|[-])\S*" (:name (meta opts))))
      (u/errorf "The :name must not contain whitespace or hyphens: '%s'"
                (:name (meta opts))))
    (binding [*included* (when-let [included (:include (meta opts))]
                           (set included))
              *excluded* (set (:exclude (meta opts)))
              *excluded-attributes* (or (:excluded-attributes (meta opts))
                                        {})
              *marked*   (if-let [mark (:mark (meta opts))]
                           (cond
                             (fn? mark)   mark
                             (coll? mark) (set mark)
                             :else (u/errorf ":mark must be a function or collection but was a %s: %s."
                                             (class mark) mark))
                           (constantly false))
              *node-attrs* (or (:node-attrs (meta opts)) {})
              *print-qualified-names* (:qualified-names (meta opts))]
      (str "digraph " (:name (meta opts)) " {"
           (clojure.string/join
            \;
            (for [[k v] opts]
              (str (name k) "=" v)))
           ";\n\n"
           (if-let [[_ f] (q/the (fn [[cls _]]
                                   (instance? cls m))
                                 dot-model-fns)]
             (f m)
             (u/errorf "No dotting function defined for %s." (class m)))
           "}"))))

(defn print-model
  "Prints a visualization of model `m` (a TGraph or EMF Resource or ResourceSet)
  to the file `f`.
  The file type is determined by its extension (dot, xdot, ps, svg, svgz, png,
  gif, pdf).

  `f` may also be the keyword :gtk.  In that case, no file is actually printed,
  but instead a GTK+ window showing the model is created.  `f` may also be the
  keyword :image, in which nothing is shown or printed, but the visualization
  is returned as a java.awt.Image.

  Additional `opts` may be specified.  Those are usually DOT Graph
  Attributes (http://www.graphviz.org/content/attrs), e.g.,

    (print-model m \"test.pdf\" :ranksep 2.2)

  Additionally, the non-DOT :name option may be used to give a name to the
  model, which affecs the title of the generated PDF for example:

    (print-model m \"test.pdf\" :ranksep 2.2 :name \"MyModel\")

  The :name must be a valid DOT ID, e.g., it must not contain whitespaces.

  An :include and an :exclude option may be given, both being seqs of model
  elements to include/exclude from printing.  If :include is nil, everything is
  included.  :exclude overrides :include.  A note for TGraphs: :include
  and :exclude usually specify only vertices.  Edges are printed if their alpha
  and omega vertex are printed.  To forbid printing of certain edges where
  alpha/omega are printed, it is possible to add them to :exclude, though.

  The option :excluded-attributes is a map of the form {pred [:attr1 :attr2]}
  determining that these two attributes should not be printed for elements for
  which pred holds.

  A :mark option is supported, too.  It is a seq of elements that should be
  highlighted (printed in red color instead of black).  It may also be a
  predicate that gets an element and returns true/false.

  A :node-attrs option may be specified.  It is a map of the form {pred
  node-attrs} where pred is a predicate applied to each element in the model.
  If (pred el) returns true, this element gets the string node-attrs appended
  to its definition.  You can use that for example to colorize the output,
  e.g., :node-attrs {#(g/has-type? % 'Person) \"fillcolor=\\\"green\\\"\"} will
  print all Person elements with a green background.  All predicates are
  tested, and all node-attrs of all succeeding preds are appended.  Note that
  using :mark and :node-attrs where the latter sets the dot color attribute may
  interfer in case a marked node is also matched my the
  corresponding :node-attrs predicate.  In this case, the latter takes
  precedence.

  If the option :qualified-names is set to true, the element types will be
  printed as fully qualified names.  The default is false, where only simple
  class names are printed."
  [m f & opts]
  (let [ds (dot-model m opts)
        dot (fn
              ([fmt]
               (let [r (clojure.java.shell/sh "dot" (str "-T" fmt) :in ds)]
                 (or (zero? (:exit r))
                     (u/errorf "Dotting failed: %s" (:err r)))))
              ([fmt f]
               (let [r (clojure.java.shell/sh "dot" (str "-T" fmt) "-o" f :in ds)]
                 (or (zero? (:exit r))
                     (u/errorf "Dotting failed: %s" (:err r))))))
        exts #{"dot" "xdot" "ps" "svg" "svgz" "png" "gif" "pdf" "eps"}
        fmt (if (string? f)
              (get exts (second (re-matches #".*\.([^.]+)$" f)))
              f)]
    (when-not fmt
      (u/errorf "Unknown file format: %s. Supported extensions are %s." f exts))
    (when (string? f)
      (println (format "Printing visualization to %s." f)))
    (condp = fmt
      :gtk   (dot "gtk")
      :image (let [tmp (java.io.File/createTempFile "img" "png")
                   path (.getPath tmp)]
               (when (dot "png" path)
                 (.getImage (ImageIcon. path))))
      (dot fmt f))))
