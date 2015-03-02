#!/bin/zsh

echo "Deleting the old docs"
rm docs/*

lein with-profile docs html5-docs

if [[ $? -eq 0 ]]; then
    echo "Deleting the old docs on helena"
    ssh horn@linux.uni-koblenz.de 'rm /home/horn/public_html/funnyqt-docs/*'
    echo "Copying over the new docs"
    scp -r docs/* horn@linux.uni-koblenz.de:/home/horn/public_html/funnyqt-docs/
    echo "Adjusting the permissions"
    ssh horn@linux.uni-koblenz.de \
	'cd /home/horn/public_html/funnyqt-docs && chmod -R 644 *'
    echo Fini.
else
    echo Error.
fi

lein with-profile docs html5-docs :docset
