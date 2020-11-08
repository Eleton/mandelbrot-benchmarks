const fs = require("fs");
const { add, multiply, abs, matrix } = require("./math");
const { log } = console;

let { CENTER_X, CENTER_Y, ZOOM, ITERATIONS, WIDTH, HEIGHT } = process.env;

// let [, , cx, cy, zoom, iterations] = process.argv;
cx = parseFloat(CENTER_X) || 0;
cy = parseFloat(CENTER_Y) || 0;
zoom = parseFloat(ZOOM) || 1;
iterations = parseInt(ITERATIONS) || 50;
width = parseInt(WIDTH) || 600;
height = parseInt(HEIGHT) || 600;

// const mandel = (z, m) => {
//   const brot = (complex, iteration, max) => {
//     const c = multiply(complex, complex);
//     if (abs(c) > 2) {
//       return iteration;
//     }
//     if (iteration === max) {
//       return 0;
//     }
//     return brot(add(c, z), iteration + 1, max);
//   };
//   return brot(z, 0, m);
// };

const mandelbrot = (z, m) => {
  let complex = [...z];
  for (let i = 0; i < m; i++) {
    complex = multiply(complex, complex);
    if (abs(complex) > 2) {
      return i;
    }
    complex = add(complex, z);
  }
  return 0;
};

const mat = matrix(cx - zoom, cx + zoom, cy - zoom, cy + zoom, width, height);
const result = mat.map((row) => row.map((z) => mandelbrot(z, iterations)));

fs.writeFileSync("results/js_iterative.json", JSON.stringify(result));
