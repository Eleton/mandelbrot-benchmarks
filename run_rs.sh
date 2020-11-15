#!/usr/bin/env bash

source .envrc &&
echo "Rust:" &&
cargo build --release --manifest-path=rust/Cargo.toml &&
time ./rust/target/release/rust