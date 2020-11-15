module Main where

import Data.Complex
import Data.Maybe
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (intercalate)
import System.Environment (lookupEnv)

mandelbrot :: Int -> Complex Float -> Int
mandelbrot maxDepth c = moveMaxToZero $
  stepsUntilDivergence complexZero zStep diverging maxDepth
  where zStep zLast = zLast^2 + c
        diverging z = magnitude z > 2
        complexZero = 0 :+ 0
        moveMaxToZero i
          | i == maxDepth = 0
          | otherwise     = i

stepsUntilDivergence :: a -> (a -> a) -> (a -> Bool) -> Int -> Int
stepsUntilDivergence start step diverging depth =
    length . takeWhile (not . diverging) . take depth $ iterate step start

mandelbrotMap xCenter yCenter zoom iterations width height =
  fmap (mandelbrot iterations) <$> matrix
      where
        matrix = [[x :+ y | x <- range xCenter width] | y <- range yCenter height]
        range c l = [c - zoom, c - zoom +((2*zoom) / l)..(c + zoom)]

main :: IO ()
main = do
  resultingMap <- mandelbrotMap
    <$> (maybe (-0.5) read <$> lookupEnv "CENTER_X")
    <*> (maybe 0      read <$> lookupEnv "CENTER_Y")
    <*> (maybe 1      read <$> lookupEnv "ZOOM")
    <*> (maybe 500    read <$> lookupEnv "ITERATIONS")
    <*> (maybe 600    read <$> lookupEnv "WIDTH")
    <*> (maybe 600    read <$> lookupEnv "HEIGHT")
  resultingMap
      <&> intercalate "," . fmap show
      & intercalate "\n"
      & writeFile "results/haskell.csv"