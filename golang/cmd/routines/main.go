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
	resultRows := startWorkers(depth, runtime.NumCPU(), matrixRows)
	result := aggregateResult(height, resultRows)
	mandelbrot.WriteResult(result, "results/golang_multi.csv")
}

// startRowGenerator generates and emit one matrixRow to the resulting channel
// for each row in the matrix described by the input.
func startRowGenerator(xCenter float64, yCenter float64, zoom float64, width int64, height int64) <-chan matrixRow {
	matrixRows := make(chan matrixRow)
	go func() {
		yStep := (2*zoom) / float64(height)
		xStep := (2*zoom) / float64(width)
		y := yCenter - zoom
		for i := 0; i < int(height);  i++ {
			row := make([]complex128, width)
			y += yStep
			x := xCenter - zoom
			for j := 0; j < int(width); j++ {
				x += xStep
				row[j] = complex(x, y)
			}
			matrixRows <- matrixRow{i, row}
		}
		close(matrixRows)
	}()
	return matrixRows
}

// Starts a number of goroutines that together ingests the output of matrixRows
// and sends the processed rows on the returned channel.
// The returned channel will close when matrixRows is closed and all results are processed.
func startWorkers(depth int64, workers int, matrixRows <-chan matrixRow) <-chan mandelbrotRow {
	wg := &sync.WaitGroup{}
	resultRows := make(chan mandelbrotRow)
	go func() {
		for i := 0; i < workers; i++ {
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

// calculateMandelbrotRows consumes the matrixRows channel and generates a mandelbrotRow that it emits to resultRows.
func calculateMandelbrotRows(matrixRows <-chan matrixRow, maxDepth int, resultRows chan<- mandelbrotRow) {
	for cs := range matrixRows {
		row := make([]int, len(cs.row))
		for i, c := range cs.row {
			row[i] = mandelbrot.Mandelbrot(maxDepth, c)
		}
		resultRows <- mandelbrotRow{cs.number, row}
	}
}

// mandelbrotRow takes a channel of mandelbrotRow's and aggregates all rows emitted until it is closed.
func aggregateResult(height int64, resultRows <-chan mandelbrotRow) [][]string {
	result := make([][]string, height)
	for r := range resultRows {
		result[r.number] = make([]string, len(r.row))
		for i, n := range r.row {
			result[r.number][i] = strconv.Itoa(n)
		}
	}
	return result
}