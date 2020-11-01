#!/usr/bin/env nix-shell
#!nix-shell -i bash --argstr target Smoketests
set -e -u -o pipefail
set -x

cd Smoketests

runhaskell Setup.hs configure --enable-tests
runhaskell Setup.hs build
runhaskell Setup.hs test
