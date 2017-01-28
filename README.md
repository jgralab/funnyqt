<link rel="icon" href="https://raw.githubusercontent.com/jgralab/funnyqt/master/logos/icon32.png">

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://secure.travis-ci.org/jgralab/funnyqt.png)](http://travis-ci.org/jgralab/funnyqt)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jgralab/funnyqt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Clojars Project](http://clojars.org/funnyqt/latest-version.svg)](http://clojars.org/funnyqt)
<object data="http://jgralab.github.io/funnyqt/images/funnyqt.svg" type="image/svg+xml" width="600">
  <img src="http://jgralab.github.io/funnyqt/images/funnyqt.png" width="600">
</object>

# Welcome!

FunnyQT is a model querying and transformation library.  It currently supports
the [Java Graph Laboratory](http://jgralab.github.io/jgralab) and the
[Eclipse Modeling Framework](http://www.eclipse.org/modeling/emf/).

**News:** The FunnyQT book (my dissertation) is now available in a dead-tree
version or as gratis **Open Access** version, see [here](#dissertation).

# Table of Contents

1. [Documentation](#documentation)
  - [FunnyQT Overview](#funnyqt-overview)
  - [API Documentation](#api-documentation)
  - [Publications](#publications)
2. [Getting FunnyQT](#getting-funnyqt)
  - [Getting a Release](#getting-a-release)
  - [Getting the Git Version](#getting-the-git-version)
3. [Getting Support](#getting-support)
  - [Contact the Author](#contact-the-author)
  - [IRC Channel](#irc-channel)
  - [Gitter Channel](#gitter-channel)
  - [Found a Bug?](#found-a-bug)

# Documentation

Check out the [overview description](#funnyqt-overview), my
[dissertation](#dissertation), the
[generated API documentation](#api-documentation), and the
[papers about FunnyQT](#publications).

## FunnyQT Overview

The FunnyQT API is structured into the following sub-APIs realized in several
namespaces.

The foundation is an extensive **core API** providing typical _model management
services_ such as creating, loading, and storing models, accessing, creating,
and deleting model elements, and accessing model element properties.  The core
API is subdivided into two framework-specific namespaces (EMF or JGraLab) and
one namespace providing functions working with either kind of model
representation.

  - _funnyqt.emf_: The EMF-specific core API
  - _funnyqt.tg_: The TGraph-specific core API (JGraLab)
  - _funnyqt.generic_: Core functions working with both kinds of models/model
    elements, and protocols extended on EMF and JGraLab interfaces

The **functional querying API** builds upon the core API and provides features
such as navigation inside models in terms of role names, quantified
expressions, and _regular path expressions_.  Like the core API, some
framework-specific parts are outsourced into their own two querying namespaces.

  - _funnyqt.query_: Generic querying services such as quantified expressions,
    sorting functions, and generic regular path operators.
  - _funnyqt.query.emf_: EMF-specific regular path operators
  - _funnyqt.query.tg_: TGraph-specific regular path operators

FunnyQT's **polymorphic function API** provides constructs for defining
functions dispatching on metamodel type.

  - _funnyqt.polyfns_: Polymorphic function API

The **pattern-matching API** provides constructs for defining patterns using
node and edge symbols.  When a pattern is called on some model, it returns a
lazy sequence of all matches in the model.

  - _funnyqt.pmatch_: Pattern-matching in models

Based on the pattern-matching API, there's a **in-place transformation API**
that allows to define rules consisting of a pattern and a sequence of actions.
When a rule is invoked, it searches for a match in the model and applies its
actions to it.  Typical control structures such as as-long-as-possible
application of a rule are provided as higher-order functions.

  - _funnyqt.in-place_: In-place transformations and state-space exploration

FunnyQT provides two kinds of *model transformation APIs*.  The **extensional
model transformation API** builds upon GReTL's concepts of extensional
semantics and is an operational transformation API, whereas the **model2model
API** realizes a rule-based approach to model transformations.

  - _funnyqt.extensional_: Extensional model transformation API
  - _funnyqt.model2model_: The rule-based model transformation API

The **relational querying API** allows for Prolog-like logical querying of
models using the [core.logic](http://github.com/clojure/core.logic) library.

  - _funnyqt.relational_: Generic relations for elements, relationships,
    attributes, and references

Based on the relational querying API, there's a **bidirectional transformation
API**.

  - _funnyqt.bidi_: Bidirectional model transformations

Such a transformation specifies correspondences between a left and a right
model in terms of relations between elements in those models.  Such a
transformation can be used to generate a new right model from a given left
model and vice versa.  Additionally, it can be used to synchronize between two
existing models in either direction.

For TGraphs, there is a **co-evolution transformation API**.

- _funnyqt.coevo.tg_: Co-evolution transformations

Co-evolution transformations allow to evolve a schema and a conforming graph
simultaneously at runtime.

Finally, there are some **utility and helper APIs**.

  - _funnyqt.utils_: Generic utility functions
  - _funnyqt.visualization_: Visualizing models using
    [GraphViz](http://www.graphviz.org/)
  - _funnyqt.xmltg_: Converting arbitrary XML documents to DOM-like TGraphs and
    back again
  - _funnyqt.edn_: Persistence of model-related data in the
    [EDN](https://github.com/edn-format/edn) format.

## Dissertation

My dissertation _A Functional, Comprehensive and Extensible Multi-Platform
Querying and Transformation Approach_ discusses any aspect of FunnyQT (version
1.0.x) in detail with examples and motivations.  You can buy the book here:

- Directly at the publisher [Logos Verlag Berlin GmbH](http://www.logos-verlag.de/cgi-bin/engbuchmid?isbn=4291)
- From [Amazon](https://www.amazon.de/Functional-Comprehensive-Extensible-Multi-Platform-Transformation/dp/3832542914)

Alternatively, the electronic **Open Access** version
([CC BY-NC 3.0 DE](https://creativecommons.org/licenses/by-nc/3.0/)) of the
book is also available (gratis!) from several sources.

- [Zenodo](https://zenodo.org/record/58302#.V5D3DRJ96is)
- [Issuu](https://issuu.com/logosverlag/docs/isbn-978-3-8325-4291-7/1)
- [Deutsche Nationalbibliothek](https://portal.dnb.de/opac.htm?method=simpleSearch&cqlMode=true&query=idn%3D1107450268)

## API Documentation

The FunnyQT API docs generated for the current release are
[here](http://www.tsdh.org/funnyqt-api/).

## Publications

- _Model Querying with FunnyQT_, Tassilo Horn, ICMT 2013,
  http://dx.doi.org/10.1007/978-3-642-38883-5_7

- _Solving the TTC 2013 Flowgraphs Case with FunnyQT_, Tassilo Horn, TTC 2013,
  http://dx.doi.org/10.4204/EPTCS.135.7

- _Solving the Class Diagram Restructuring Transformation Case with FunnyQT_,
  Tassilo Horn, TTC 2013, http://dx.doi.org/10.4204/EPTCS.135.9

- _Solving the Petri-Nets to Statecharts Transformation Case with FunnyQT_,
  Tassilo Horn, TTC 2013, http://dx.doi.org/10.4204/EPTCS.135.11

- _Bidirectional Model Transformations With FunnyQT_, Tassilo Horn, rejected at
  both BX'14 and ICMT'14, but I consider it to be my best paper so far anyway
  (and the only one about FunnyQT bidirectional transformations),
  http://userpages.uni-koblenz.de/~horn/mypapers/funnyqt-bidi2014.pdf

- _Solving the TTC Movie Database Case with FunnyQT_, Tassilo Horn,
  [TTC 2014](http://ceur-ws.org/Vol-1305/paper4.pdf)

- _Solving the TTC FIXML Case with FunnyQT_, Tassilo Horn,
  [TTC 2014](http://ceur-ws.org/Vol-1305/paper5.pdf)

- _Solving the TTC Model Execution Case with FunnyQT_, Tassilo Horn,
  [TTC 2015](http://www.transformation-tool-contest.eu/solutions_execution.html)

- _Solving the TTC Java Refactoring Case with FunnyQT_, Tassilo Horn,
  [TTC 2015](http://www.transformation-tool-contest.eu/solutions_refactoring.html)

- _Solving the TTC Train Benchmark Case with FunnyQT_, Tassilo Horn,
  [TTC 2015](http://www.transformation-tool-contest.eu/solutions_train.html)

- _Graph Pattern Matching as an Embedded Clojure DSL_, Tassilo Horn,
  [ICGT 2015](http://btn1x4.inf.uni-bayreuth.de/icgt2015)

# Getting FunnyQT

There are two ways of getting FunnyQT.  When you want to use it for writing
your own queries and transformations, you should
[get the latest release](#getting-a-release) available with Leiningen.  If you
want to have the bleeding edge version from git, e.g., to validate that some
bug has been fixed, see how to [get the git version](#getting-the-git-version).

## Getting a Release

If you don't intend to hack on FunnyQT itself but you just want to use it for
writing queries and transformations, the easiest possibility is to use the
latest FunnyQT release that's available via the
[Clojars Maven repository](http://clojars.org/funnyqt).

To create query/transformation project using FunnyQT, simply perform the
following steps:

1. Install the `lein` shell (or bat) script as explained on the
   [Leiningen](http://leiningen.org) homepage.

2. Create a new project.

    ```
    $ lein new my-funnyqt-transform
    Generating a project called my-funnyqt-transform based on the 'default' template.
    To see other templates (app, lein plugin, etc), try `lein help new`.
    ```

3. Declare that your project depends on FunnyQT.  Replace 0.42.0 below with
   whatever is the the [current FunnyQT release](http://clojars.org/funnyqt).

    ```
    $ cd my-funnyqt-transform
    $ edit project.clj
    # add funnyqt in the project :dependencies
    (defproject my-funnyqt-transform "0.1.0-SNAPSHOT"
      :description "FIXME: write description"
      :url "http://example.com/FIXME"
      :license {:name "Eclipse Public License"
                :url "http://www.eclipse.org/legal/epl-v10.html"}
      :dependencies [ [funnyqt "0.42.0"] ])
    ```

4. Now start a REPL and start hacking.  Leiningen will take care of fetching
   all dependencies such as Clojure and FunnyQT.

    ```
    nREPL server started on port 44783 on host 127.0.0.1 - nrepl://127.0.0.1:44783
    REPL-y 0.3.5, nREPL 0.2.6
    Clojure 1.7.0-alpha5
    OpenJDK 64-Bit Server VM 1.8.0_40-b20
       Docs: (doc function-name-here)
             (find-doc "part-of-name-here")
     Source: (source function-name-here)
    Javadoc: (javadoc java-object-or-class-here)
       Exit: Control+D or (exit) or (quit)
    Results: Stored in vars *1, *2, *3, an exception in *e
    user=> ;; Here you go!
    ```

## Getting the Git Version

FunnyQT uses [Leiningen](http://leiningen.org) for retrieving all its
dependencies from various Maven repositories, building, publishing, and test
automation.

Getting started is really simple:

1. Install the `lein` shell (or bat) script as explained on the
   [Leiningen](http://leiningen.org) homepage.

2. Clone the FunnyQT git repository:

    ```
    $ git clone https://github.com/jgralab/funnyqt.git
    ```

3. Just to be sure everything's fine, you might want to execute the test cases.

    ```
    $ cd funnyqt
    $ lein test
    ```

4. You are ready to go.  Start a REPL and start hacking.

    ```
    $ lein repl
    REPL started; server listening on localhost port 22815
    user=> (use 'funnyqt.tg)
    nil
    user=> (def g (load-graph "test/input/greqltestgraph.tg"))
    #<Graph c06de1c7-f4ec0906-21cfbc86-28c31aa1 (1175): RouteMap>>
    user=> (vseq g 'localities.City)
    (#<v6: localities.City> #<v7: localities.City> #<v8: localities.City>)
    ```

# Getting Support

## Contact the Author

FunnyQT is developed by Tassilo Horn ([tsdh](http://github.com/tsdh)).  If you
want, you can get in touch with me via [email](mailto:tsdh@gnu.org),
[IRC](#irc-channel), [Gitter](#gitter-channel), or
[G+](http://plus.google.com/u/0/+TassiloHorn).

## IRC Channel

Join us on the official [JGraLab IRC Channel](irc://irc.freenode.net/#jgralab)
(channel `#jgralab` on `irc.freenode.net`).  If you don't have or don't want to
install an IRC client, you can also
[chat directly in your browser](http://webchat.freenode.net/?channels=jgralab).

## Gitter Channel

FunnyQT also has a Gitter channel:
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/jgralab/funnyqt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge).

If I'm not around, just ask your question and come back later.  I'll reply as
soon as I can.

## Found a Bug?

Although that should be quite impossible and is surely an error on your side
`>:)`, if you found a bug or miss an important feature, file a bug report in
the [FunnyQT issue tracker](http://github.com/jgralab/funnyqt/issues).

# License

Copyright (C) 2011-2017 Tassilo Horn <tsdh@gnu.org> & The JGraLab Team <ist@uni-koblenz.de>

Distributed under the
[General Public License, Version 3](http://www.gnu.org/copyleft/gpl.html) with
the following additional grant:

    Additional permission under GNU GPL version 3 section 7

    If you modify this Program, or any covered work, by linking or combining it
    with Eclipse (or a modified version of that program or an Eclipse plugin),
    containing parts covered by the terms of the Eclipse Public License (EPL),
    the licensors of this Program grant you additional permission to convey the
    resulting work.  Corresponding Source for a non-source form of such a
    combination shall include the source code for the parts of JGraLab used as
    well as that of the covered work.

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

<!-- Local Variables:        -->
<!-- mode: markdown          -->
<!-- indent-tabs-mode: nil   -->
<!-- End:                    -->
