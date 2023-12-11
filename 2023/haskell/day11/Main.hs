{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Linear.V2
import Data.List (transpose)
import Linear (dot)

type Input = [V2 Int]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse text
  = [ V2 x y
    | (row, x) <- zip rows $ zipWith (+) [0..] $ offsets rows
    , (char, y) <- zip row $ zipWith (+) [0..] $ offsets $ transpose rows
    , char == '#'
    ]
  where
    rows = lines text
    offsets = scanl1 (+) . map (fromEnum . isBlank)
    isBlank = all (== '.')

part1 :: Input -> Int
part1 gs = sum [manhattan g g' | g <- gs, g' <- gs, g < g']

part2 :: Input -> Int
part2 _ = 0

manhattan a b = V2 1 1 `dot` abs (a - b)
