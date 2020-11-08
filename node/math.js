const add = ([a, b], [c, d]) => [a + c, b + d];
const multiply = ([a, b], [c, d]) => [a * c - b * d, a * d + c * b];
const abs = ([a, b]) => Math.sqrt(a ** 2 + b ** 2);

const range = (min, max, steps) =>
  new Array(steps).fill(min).map((x, i) => x + (i * (max - min)) / steps);

const matrix = (x1, x2, y1, y2, xres, yres) =>
  range(y1, y2, yres).map((b) => range(x1, x2, xres).map((a) => [a, b]));

module.exports = { add, multiply, abs, matrix };
