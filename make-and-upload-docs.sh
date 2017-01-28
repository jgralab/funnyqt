#!/bin/zsh

echo "Deleting the old docs"
rm docs/*

lein with-profile docs html5-docs

if [[ $? -eq 0 ]]; then
    echo "Deleting the old docs on helena"
    ssh horn@134.119.24.195 'rm /home/horn/www/funnyqt-api/*'
    echo "Copying over the new docs"
    scp -r docs/* horn@134.119.24.195:/home/horn/www/funnyqt-api/
    echo "Adjusting the permissions"
    ssh horn@134.119.24.195 \
	'cd /home/horn/www/funnyqt-api && chmod -R 644 *'
    echo Fini.
else
    echo Error.
fi

lein with-profile docs html5-docs :docset
