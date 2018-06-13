#!/bin/bash

ITEMS=(assocCollections collections-Heaps collections-Sets sequences)

function cloneRepo() {

    SECONDS=5

    if [ -d $1 ] ; then

        echo "Error: $1: Repository exists!"
        exit 1

    fi

    echo -e "Cloning repository: '$2', in $SECONDS seconds..." ; sleep $SECONDS

    git clone $2


}


function setupSandboxes() {

    if [ ! -d $1 ] ; then

        echo "Error: $1: Directory does not exist!"
        exit 1

    fi

    cd $1

    if [ ! -d ".cabal-sandbox" ] ; then

        cabal sandbox init

        cabal sandbox add-source ../criterion/

        cabal sandbox add-source ../edison/edison-api/

        cabal sandbox add-source ../edison/edison-core/

    fi

    for item in ${ITEMS[*]}
    do
        cd $item

        cabal sandbox init --sandbox=../.cabal-sandbox/

        cabal install --dependencies-only

        cd ..

    done


}


function main() {

    cloneRepo 'criterion' 'https://github.com/green-haskell/criterion.git'

    cloneRepo 'edison' 'https://github.com/gilbertomelfe/edison.git'

    setupSandboxes 'edison-benchmark'


}


main





