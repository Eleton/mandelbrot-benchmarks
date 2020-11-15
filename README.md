# Mandelbrot

Calculating a [mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set) with multiple languages for benchmarking.

![alt text](image.png)

## Calculations

At the moment there are four calculations written in two languages:

1. Javscript Recursively.
   Written in Node.js, using a recursive algorithm.
2. Javscript Iteratively.
   Written in Node.js, using a iterative algorithm.
3. Elixir Single. Written in Elixir, without any concurrent calculations.
4. Elixir Multi. Written in Elixir, with concurrent calculations.
5. Rust. Written in Rust.

## How to run?

Assuming Node, Elixir and Rust are installed, run:

```
bash run.sh
```

If you only want to run the scripts of one language:

```
bash run_js.sh
bash run_ex.sh
bash run_rs.sh
```

Result files will be generated and placed in `results/`.

Parameters can be edited in `.envrc`.

`index.html` will illustrate the mandelbrot images. Serve with local server, for example:

```
npx http-server -p 4000
```

and visit `localhost:4000` in your web browser.

## Current Benchmarks

```
Javascript Recursive:
0m8.350s

Javascript Iterative:
0m6.322s

Elixir Single:
0m19.015s

Elixir Multi:
0m11.086s

Rust:
0m0.396s
```
