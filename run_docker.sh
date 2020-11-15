#!/usr/bin/env bash

source .envrc
echo "Javascript Recursive:"
docker run -v $PWD/node:/node -v $PWD/results:/results --entrypoint "bash" --env-file .env node -c "time node /node"
echo ""
echo "Javascript Iterative:"
docker run -v $PWD/node:/node -v $PWD/results:/results --entrypoint "bash" --env-file .env node -c "time node /node/index_iterative.js"
echo ""
echo "Elixir Single:"
docker run -v $PWD/elixir:/elixir -v $PWD/results:/results --entrypoint "bash" --env-file .env elixir -c "time elixir /elixir/mandelbrot.exs"
echo ""
echo "Elixir Multi:"
docker run -v $PWD/elixir:/elixir -v $PWD/results:/results --entrypoint "bash" --env-file .env elixir -c "time elixir /elixir/mandelbrot_multi.exs"
echo ""
echo "Rust:"
docker run -v $PWD/rust:/rust rust cargo build --release --manifest-path=/rust/Cargo.toml
docker run -v $PWD/rust:/rust -v $PWD/results:/results --entrypoint "bash" --env-file .env elixir -c "time /rust/target/release/rust"