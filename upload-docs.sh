#!/bin/zsh

scp -r docs/* horn@helena.uni-koblenz.de:/home/horn/public_html/funnyqt-docs/

ssh horn@helena.uni-koblenz.de \
    'cd /home/horn/public_html/funnyqt-docs && chmod -R 644 *'

echo Fini.

