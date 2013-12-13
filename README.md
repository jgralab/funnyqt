<object data="http://jgralab.github.io/funnyqt/images/funnyqt.svg" type="image/svg+xml" width="600">
  <img src="http://jgralab.github.io/funnyqt/images/funnyqt.png" width="600">
</object>

# FunnyQT

FunnyQT is a model querying and transformation library.  It currently supports
the [Java Graph Laboratory](http://jgralab.github.io/jgralab) and the
[Eclipse Modeling Framework](http://www.eclipse.org/modeling/emf/).

Current build status: [![Build Status](https://secure.travis-ci.org/jgralab/funnyqt.png)](http://travis-ci.org/jgralab/funnyqt)

# Documentation

## FunnyQT Overview

## API Documentation

The FunnyQT API docs are
[here](http://userpages.uni-koblenz.de/~horn/funnyqt-docs/).

# Getting Support

## Contact the Author

FunnyQT is developed by Tassilo Horn ([tsdh](http://github.com/tsdh)).  If you
want, you can get in touch with me via email, IRC (see below), or
[G+](http://plus.google.com/u/0/+TassiloHorn).

## IRC Channel

Join us on the official [JGraLab IRC Channel](irc://irc.freenode.net/#jgralab)
(channel `#jgralab` on `irc.freenode.net`).  If you don't have or don't want to
install an IRC client, you can also
[chat directly in your browser](http://webchat.freenode.net/?channels=jgralab).

## Found a Bug?

Although that should be quite impossible and is surely an error on your side
`>:)`, if you found a bug or miss an important feature, file a bug report in
the [FunnyQT issue tracker](http://github.com/jgralab/funnyqt/issues).

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


    $ lein new my-funnyqt-transform
    Generating a project called my-funnyqt-transform based on the 'default' template.
    To see other templates (app, lein plugin, etc), try `lein help new`.

3. Declare that your project depends on FunnyQT.  Replace 0.13.0 below with
   whatever is the the [current FunnyQT release](http://clojars.org/funnyqt).

    $ cd my-funnyqt-transform
    $ edit project.clj
    # add funnyqt in the project :dependencies
    (defproject my-funnyqt-transform "0.1.0-SNAPSHOT"
      :description "FIXME: write description"
      :url "http://example.com/FIXME"
      :license {:name "Eclipse Public License"
                :url "http://www.eclipse.org/legal/epl-v10.html"}
      :dependencies [[org.clojure/clojure "1.5.1"]
                     [funnyqt "0.13.0"]])

4. Now start a REPL and start hacking.  Leiningen will take care of fetching
   all dependencies such as Clojure and FunnyQT.


    $ lein repl
    nREPL server started on port 39869 on host 127.0.0.1
    REPL-y 0.3.0
    Clojure 1.5.1
        Docs: (doc function-name-here)
              (find-doc "part-of-name-here")
      Source: (source function-name-here)
     Javadoc: (javadoc java-object-or-class-here)
        Exit: Control+D or (exit) or (quit)
     Results: Stored in vars *1, *2, *3, an exception in *e

    user=> ;; Here you go!

## Getting the Git Version

FunnyQT uses [Leiningen](http://leiningen.org) for retrieving all its
dependencies from various Maven repositories, building, publishing, and test
automation.

Getting started is really simple:

1. Install the `lein` shell (or bat) script as explained on the
[Leiningen](http://leiningen.org) homepage.

2. Clone the FunnyQT git repository:

    $ git clone https://github.com/jgralab/funnyqt.git

3. Just to be sure everything's fine, you might want to execute the test cases.

    $ cd funnyqt
    $ lein test

4. You are ready to go.  Start a REPL and start hacking.

    $ lein repl
    REPL started; server listening on localhost port 22815
    user=> (use 'funnyqt.tg)
    nil
    user=> (def g (load-graph "test/input/greqltestgraph.tg"))
    #<Graph c06de1c7-f4ec0906-21cfbc86-28c31aa1 (1175): RouteMap>>
    user=> (vseq g 'localities.City)
    (#<v6: localities.City> #<v7: localities.City> #<v8: localities.City>)

# License

Copyright (C) 2011-2013 Tassilo Horn <horn@uni-koblenz.de> & The JGraLab Team <ist@uni-koblenz.de>

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


<!-- Local Variables:        -->
<!-- mode: markdown          -->
<!-- indent-tabs-mode: nil   -->
<!-- End:                    -->
