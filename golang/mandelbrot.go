package golang

import (
	"encoding/csv"
	"math/cmplx"
	"os"
)

func Mandelbrot(max int, c complex128) int {
	current := 0 + 0i
	for i := 0; i < max; i++ {
		if cmplx.Abs(current) > 2 {
			return i
		} else {
			current = (current * current) + c
		}
	}
	return 0
}


func WriteResult(result [][]string, name string) {
	csvFile, err := os.Create(name)
	if err != nil {
		panic(err)
	}
	csvwriter := csv.NewWriter(csvFile)
	err = csvwriter.WriteAll(result)
	if err != nil {
		panic(err)
	}
}