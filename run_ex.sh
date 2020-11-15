#!/usr/bin/env bash

source .envrc
echo "Elixir Single:"
time elixir elixir/mandelbrot.exs
echo ""
echo "Elixir Multi:"
time elixir elixir/mandelbrot_multi.exs