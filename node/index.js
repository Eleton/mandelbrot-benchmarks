const fs = require("fs");
// const { add, multiply, abs, matrix } = "./math.js";
const { log } = console;

const add = ([a, b], [c, d]) => [a + c, b + d];
const multiply = ([a, b], [c, d]) => [a * c - b * d, a * d + c * b];
const abs = ([a, b]) => Math.sqrt(a ** 2 + b ** 2);

const range = (min, max, steps) =>
  new Array(steps).fill(min).map((x, i) => x + (i * (max - min)) / steps);
const matrix = (x1, x2, y1, y2, xres, yres) =>
  range(y1, y2, yres).map((b) => range(x1, x2, xres).map((a) => [a, b]));

let { CENTER_X, CENTER_Y, ZOOM, ITERATIONS, WIDTH, HEIGHT } = process.env;

// let [, , cx, cy, zoom, iterations] = process.argv;
cx = parseFloat(CENTER_X) || 0;
cy = parseFloat(CENTER_Y) || 0;
zoom = parseFloat(ZOOM) || 1;
iterations = parseInt(ITERATIONS) || 50;
width = parseInt(WIDTH) || 600;
height = parseInt(HEIGHT) || 600;

const mandel = (z, m) => {
  const brot = (complex, iteration, max) => {
    const c = multiply(complex, complex);
    if (abs(c) > 2) {
      return iteration;
    }
    if (iteration === max) {
      return 0;
    }
    return brot(add(c, z), iteration + 1, max);
  };
  return brot(z, 0, m);
};

const mat = matrix(cx - zoom, cx + zoom, cy - zoom, cy + zoom, width, height);
const result = mat.map((row) => row.map((z) => mandel(z, iterations)));

fs.writeFileSync("results/js_recursive.json", JSON.stringify(result));
