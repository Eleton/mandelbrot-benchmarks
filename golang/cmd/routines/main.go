package main

import (
	mandelbrot "github.com/eleton/mandelbrot-benchmarks/golang"
	"os"
	"runtime"
	"strconv"
	"sync"
)

type matrixRow struct {
	number int
	row []complex128
}

type mandelbrotRow struct {
	number int
	row []int
}

func main() {
	xCenter, _ := strconv.ParseFloat(os.Getenv("CENTER_X"), 64)
	yCenter, _ := strconv.ParseFloat(os.Getenv("CENTER_Y"), 64)
	zoom, _ := strconv.ParseFloat(os.Getenv("ZOOM"), 64)
	depth, _ := strconv.ParseInt(os.Getenv("ITERATIONS"), 10, 64)
	width, _ := strconv.ParseInt(os.Getenv("WIDTH"), 10, 64)
	height, _ := strconv.ParseInt(os.Getenv("HEIGHT"), 10, 64)

	matrixRows := startRowGenerator(xCenter, yCenter, zoom, width, height)
	resultRows := startWorkers(depth, matrixRows)
	result := aggregateResult(height, resultRows)
	mandelbrot.WriteResult(result, "results/golang_multi.csv")
}

func startRowGenerator(xCenter float64, yCenter float64, zoom float64, width int64, height int64) chan matrixRow {
	matrixRows := make(chan matrixRow)
	go func() {
		yStep := (2*zoom) / float64(height)
		xStep := (2*zoom) / float64(width)
		i := -1
		for y := yCenter - zoom; y < yCenter + zoom; y += yStep {
			row := make([]complex128, width)
			i++
			j := 0
			for x := xCenter - zoom; x < xCenter + zoom; x += xStep {
				row[j] = complex(x, y)
				j++
			}
			matrixRows <- matrixRow{i, row}
		}
		close(matrixRows)
	}()
	return matrixRows
}

func startWorkers(depth int64, matrixRows chan matrixRow) chan mandelbrotRow {
	wg := &sync.WaitGroup{}
	resultRows := make(chan mandelbrotRow)
	go func() {
		for i := 0; i < runtime.NumCPU(); i++ {
			wg.Add(1)
			go func() {
				calculateMandelbrotRows(matrixRows, int(depth), resultRows)
				wg.Done()
			}()
		}
		wg.Wait()
		close(resultRows)
	}()
	return resultRows
}

func calculateMandelbrotRows(matrixRows chan matrixRow, maxDepth int, resultRows chan mandelbrotRow) {
	for cs := range matrixRows {
		row := make([]int, len(cs.row))
		for i, c := range cs.row {
			row[i] = mandelbrot.Mandelbrot(maxDepth, c)
		}
		resultRows <- mandelbrotRow{cs.number, row}
	}
}

func aggregateResult(height int64, resultRows chan mandelbrotRow) [][]string {
	result := make([][]string, height+2)
	for r := range resultRows {
		result[r.number] = make([]string, len(r.row))
		for i, n := range r.row {
			result[r.number][i] = strconv.Itoa(n)
		}
	}
	return result
}