use std::fs;
use std::env;

fn main() {
  let cx: f64 = match env::var("CENTER_X") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => -0.5,
  };
  let cy: f64 = match env::var("CENTER_Y") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => 0.0,
  };
  let zoom: f64 = match env::var("ZOOM") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => 1.0,
  };
  let width = match env::var("WIDTH") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => 600,
  };
  let height = match env::var("HEIGHT") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => 600,
  };
  let iterations = match env::var("ITERATIONS") {
    Ok(val) => val.parse().unwrap(),
    Err(_e) => 500,
  };

  let grid = matrix(cx - zoom, cx + zoom, cy - zoom, cy + zoom, width, height);
  let result = grid
    .into_iter()
    .map(|row|
      row
        .into_iter()
        .map(|z|
          mandel(z, iterations).to_string()
        ).collect::<Vec<String>>()
        .join(",")
    ).collect::<Vec<String>>()
    .join("\n");

  match fs::write("results/rust.csv", result.as_bytes()) {
    Ok(_val) => (),
    Err(_e) => println!("Failed =(")
  }
}

fn add((a,b): (f64, f64), (c,d): (f64, f64)) -> (f64, f64) {
  (a+c, b+d)
}

fn multiply((a,b): (f64, f64), (c,d): (f64, f64)) -> (f64, f64) {
  (a*c - b*d, a*d + b*c)
}

fn abs((a, b): (f64, f64)) -> f64 {
  (a*a + b*b).sqrt()
}

fn range(min: f64, max: f64, steps: u32) -> Vec<f64> {
  let r = 0..steps;
  r.map(|step| min + (step as f64)*(max - min)/(steps as f64)).collect::<Vec<f64>>()
}

fn matrix(x1: f64, x2: f64, y1: f64, y2: f64, xres: u32, yres: u32) -> Vec<Vec<(f64, f64)>> {
  range(y1, y2, yres)
    .into_iter()
    .map(|b|
      range(x1, x2, xres)
        .into_iter()
        .map(|a|
          (a, b)
        ).collect::<Vec<(f64, f64)>>()
      ).collect::<Vec<Vec<(f64, f64)>>>()
}

fn brot(complex: (f64, f64), z: (f64, f64), iteration: u32, max: u32) -> u32 {
  let c = multiply(complex, complex);
  if abs(c) > 2.0 {
    return iteration;
  }
  if iteration == max {
    return 0;
  }
  return brot(add(c, z), z, iteration + 1, max);
}

fn mandel(z: (f64, f64), m: u32) -> u32 {
  brot(z, z, 0, m)
}