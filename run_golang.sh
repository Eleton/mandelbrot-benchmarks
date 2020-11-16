#!/usr/bin/env bash

source .envrc
echo "Golang multi threaded:"
cd golang
go build ./cmd/routines
cd -
time golang/routines
echo ""
echo "Golang naive:"
cd golang
go build ./cmd/naive
cd -
time golang/naive