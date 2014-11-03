#!/bin/zsh

echo "Deleting the old docs"
rm docs/*

lein with-profile docs marg && lein with-profile docs html5-docs

if [[ $? -eq 0 ]]; then
    ssh horn@helena.uni-koblenz.de 'rm /home/horn/public_html/funnyqt-docs/*'
    scp -r docs/* horn@linux.uni-koblenz.de:/home/horn/public_html/funnyqt-docs/
    ssh horn@linux.uni-koblenz.de \
	'cd /home/horn/public_html/funnyqt-docs && chmod -R 644 *'
    echo Fini.
else
    echo Error.
fi


