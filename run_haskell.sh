#!/usr/bin/env bash

source .envrc
echo "Haskell:"
ghc -O2 -o haskell/haskell -no-keep-hi-files -no-keep-o-files haskell/Main
time haskell/haskell