#!/usr/bin/env bash

source .envrc
echo "Javascript Recursive:"
time node node/
echo ""
echo "Javascript Iterative:"
time node node/index_iterative.js