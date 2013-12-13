<object data="http://jgralab.github.io/funnyqt/images/funnyqt.svg" type="image/svg+xml" width="600">
  <img src="http://jgralab.github.io/funnyqt/images/funnyqt.png" width="600">
</object>

# FunnyQT

FunnyQT is a model querying and transformation library.  It supports (or is
going to support) TGraph and EMF models.  It's part of Tassilo Horn's
dissertation studies.

Current build status: [![Build Status](https://secure.travis-ci.org/jgralab/funnyqt.png)](http://travis-ci.org/jgralab/funnyqt)

## Documentation

The FunnyQT API docs are
[here](http://userpages.uni-koblenz.de/~horn/funnyqt-docs/).

## IRC Channel

Join us on the official [JGraLab IRC Channel](irc://irc.freenode.net/#jgralab)
(channel `#jgralab` on `irc.freenode.net`).  If you don't have or don't want to
install an IRC client, you can also
[chat directly in your browser](http://webchat.freenode.net/?channels=jgralab).


## Installation and Building

FunnyQT uses [Leiningen](https://github.com/technomancy/leiningen) for
retrieving all its dependencies from various Maven repositories, building,
publishing, and test automation.

Getting started is really simple:

1. Install the `lein` shell (or bat) script as explained in the
**Installation** section of the
[Leiningen](https://github.com/technomancy/leiningen) page.

2. Just to be sure everything's fine, you might want to execute the test cases.

```
$ cd funnyqt
$ lein test
```

3. You are ready to go.  Start a REPL and start hacking.

```
$ lein repl
REPL started; server listening on localhost port 22815
user=> (use 'funnyqt.tg)
nil
user=> (use 'funnyqt.query.tg)
nil
user=> (def g (load-graph "test/greqltestgraph.tg"))
#<Graph c06de1c7-f4ec0906-21cfbc86-28c31aa1 (1175): RouteMap>>
user=> (vseq g 'localities.City)
(#<v6: localities.City> #<v7: localities.City> #<v8: localities.City>)
```

## License

Copyright (C) 2011-2013 Tassilo Horn & The JGraLab Team <ist@uni-koblenz.de>

Distributed under the
[General Public License, Version 3](http://www.gnu.org/copyleft/gpl.html).

<!-- Local Variables:        -->
<!-- mode: markdown          -->
<!-- indent-tabs-mode: nil   -->
<!-- End:                    -->
