module Main where

import Data.Complex
import Data.Maybe
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.List (intercalate)
import System.Environment (lookupEnv)

-- | Given a maximum depth, maxDepth, and a complex point, c,
-- this returns the iterations required before it diverges, (||z|| > 2).
-- Returns zero if it the point is part of the mandelbrot set.
mandelbrot :: Int -> Complex Float -> Int
mandelbrot maxDepth c = moveMaxToZero $ stepsUntil diverging complexZero zStep maxDepth
  where zStep zLast = zLast^2 + c
        diverging z = magnitude z > 2
        complexZero = 0 :+ 0
        moveMaxToZero i
          | i == maxDepth = 0
          | otherwise     = i

-- | Given a predicate, a starting point, a step function and a maximum number of steps
-- this functions returns the number of steps needed until the predicate is true.
stepsUntil :: (a -> Bool) -> a -> (a -> a) -> Int -> Int
stepsUntil pred start step depth = length . takeWhile (not . pred) . take depth $ iterate step start

-- | Generates a matrix representing the mandelbrot set.
mandelbrotMap xCenter yCenter zoom iterations width height = fmap (mandelbrot iterations) <$> matrix
  where matrix = [[x :+ y | x <- range xCenter width] | y <- range yCenter height]
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
  writeFile "results/haskell.csv" .  intercalate "\n" $ intercalate "," . fmap show <$> resultingMap