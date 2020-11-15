#!/usr/bin/env bash

source .envrc &&
echo "Javascript Recursive:" &&
time node node/ &&
echo "" &&
echo "Javascript Iterative:" &&
time node node/index_iterative.js &&
echo "" &&
echo "Elixir Single:" &&
time elixir elixir/mandelbrot.exs &&
echo "" &&
echo "Elixir Multi:" &&
time elixir elixir/mandelbrot_multi.exs &&
echo "" &&
echo "Rust:" &&
cargo build --release --manifest-path=rust/Cargo.toml &&
time ./rust/target/release/rust