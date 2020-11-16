package main

import (
	mandelbrot "github.com/eleton/mandelbrot-benchmarks/golang"
	"os"
	"strconv"
)

func main() {
	xCenter, _ := strconv.ParseFloat(os.Getenv("CENTER_X"), 64)
	yCenter, _ := strconv.ParseFloat(os.Getenv("CENTER_Y"), 64)
	zoom, _ := strconv.ParseFloat(os.Getenv("ZOOM"), 64)
	depth, _ := strconv.ParseInt(os.Getenv("ITERATIONS"), 10, 64)
	width, _ := strconv.ParseInt(os.Getenv("WIDTH"), 10, 64)
	height, _ := strconv.ParseInt(os.Getenv("HEIGHT"), 10, 64)

	matrix := generateMatrix(xCenter, yCenter, zoom, int(width), int(height))
	result := naiveMandelbrot(int(depth), matrix)

	stringMatrix := make([][]string, len(result))
	for i, r := range result {
		stringMatrix[i] = make([]string, len(result[i]))
		for j, n := range r {
			stringMatrix[i][j] = strconv.Itoa(n)
		}
	}
	mandelbrot.WriteResult(stringMatrix, "results/golang_naive.csv")
}

func generateMatrix(xCenter float64, yCenter float64, zoom float64, width int, height int) [][]complex128 {
	yStep := (2*zoom) / float64(height)
	xStep := (2*zoom) / float64(width)
	matrix := make([][]complex128, height + 1)
	i := 0
	for y := yCenter - zoom; y < yCenter + zoom; y += yStep {
		row := make([]complex128, width + 1)
		matrix[i] = row
		i++
		j := 0
		for x := xCenter - zoom; x < xCenter + zoom; x += xStep {
			row[j] = complex(x, y)
			j++
		}
	}
	return matrix
}

func naiveMandelbrot(maxDepth int, cMatrix [][]complex128) [][]int {
	result := make([][]int, len(cMatrix))
	for i, r := range cMatrix {
		result[i] = make([]int, len(r))
		for j, c := range r {
			result[i][j] = mandelbrot.Mandelbrot(maxDepth, c)
		}
	}
	return result
}