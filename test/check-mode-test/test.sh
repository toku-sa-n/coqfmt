#!/bin/bash

OCAMLRUNPARAM=b

test() {
    cat $1 | opam exec -- dune exec ../../bin/main.exe -- --check 1>/dev/null 2>/dev/null

    if [ $? -ne $2 ]
    then
        echo "Test failed: $1"
        exit 1
    fi
}

test formatted.v 0
test unformatted.v 1
