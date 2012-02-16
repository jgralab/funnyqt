# FunnyQT

FunnyQT is a model querying and transformation library.  It supports (or is
going to support) TGraph and EMF models.

## Installation and Building

FunnyQT uses [Leiningen](https://github.com/technomancy/leiningen) for
retrieving all its dependencies from various Maven repositories, building,
publishing, and test automation.

Getting started is really simple:

1. Install the `lein` shell (or bat) script as explained in the
**Installation** section of the
[Leiningen](https://github.com/technomancy/leiningen) page.

2. Fetch the FunnyQT dependencies:

```
$ cd funnyqt
$ lein deps
```

3. Just to be sure everything's fine, you might want to execute the test cases.

```
$ lein test
```

4. You are ready to go.  Start a REPL and start hacking.  (When using the plain
REPL instead of the REPLs in a sophisticated IDE such as
[SLIME](http://common-lisp.net/project/slime/) or
[Counterclockwise](http://code.google.com/p/counterclockwise/), it's highly
recommended to have [rlwrap](http://utopia.knoware.nl/~hlub/rlwrap/#rlwrap)
installed to get some better editing and history support.)

```
$ lein repl
REPL started; server listening on localhost port 22815
user=> (use 'funnyqt.tg.core)
nil
user=> (use 'funnyqt.tg.query)
nil
user=> (def g (load-graph "test/greqltestgraph.tg"))
#<Graph c06de1c7-f4ec0906-21cfbc86-28c31aa1 (1175): RouteMap>>
user=> (vseq g 'localities.City)
(#<v6: localities.City> #<v7: localities.City> #<v8: localities.City>)
```

## License

Copyright (C) 2011-2012 The JGraLab Team <ist@uni-koblenz.de>

Distributed under the General Public License (Version 3), with the following
additional grant:

```
Additional permission under GNU GPL version 3 section 7

If you modify this Program, or any covered work, by linking or combining it
with Eclipse (or a modified version of that program or an Eclipse plugin),
containing parts covered by the terms of the Eclipse Public License (EPL), the
licensors of this Program grant you additional permission to convey the
resulting work.  Corresponding Source for a non-source form of such a
combination shall include the source code for the parts of FunnyQT and JGraLab
used as well as that of the covered work.
```


<!-- Local Variables:        -->
<!-- mode: markdown          -->
<!-- indent-tabs-mode: nil   -->
<!-- End:                    -->
