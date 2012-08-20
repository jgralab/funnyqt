#!/bin/zsh

echo "Deleting the old docs"
rm docs/*

lein marg && lein html5-docs

if [[ $? -eq 0 ]]; then
    ssh horn@helena.uni-koblenz.de 'rm /home/horn/public_html/funnyqt-docs/*'
    scp -r docs/* horn@helena.uni-koblenz.de:/home/horn/public_html/funnyqt-docs/
    ssh horn@helena.uni-koblenz.de \
	'cd /home/horn/public_html/funnyqt-docs && chmod -R 644 *'
    echo Fini.
else
    echo Error.
fi


