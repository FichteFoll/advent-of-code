{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, solve) where

import Data.List (transpose)
import Linear.V2

type Input = [String]
type Input' = [V2 Int]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 = solve 2

part2 :: Input -> Int
part2 = solve 1000000

solve gapSize rows = sum [manhattan g g' | g <- gs, g' <- gs, g < g']
  where gs = galaxies gapSize rows

galaxies :: Int -> Input -> Input'
galaxies gapSize rows
  = [ V2 x y
    | (row, x) <- zip rows $ zipWith (+) [0..] $ offsets rows
    , (char, y) <- zip row $ zipWith (+) [0..] $ offsets $ transpose rows
    , char == '#'
    ]
  where
    offsets = scanl1 (+) . map ((* pred gapSize) . fromEnum . all (== '.'))

manhattan = (sum .) . (abs .) . (-)
