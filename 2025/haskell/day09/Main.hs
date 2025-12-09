{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Linear.V2
import Data.List.Extra (splitOn)

type Point = V2 Int
type Input = [Point]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map ((\[x,y] -> V2 x y) . map read . splitOn ",") . lines

part1 :: Input -> Int
part1 pts = maximum [product $ abs (pt2 - pt1) + V2 1 1 | pt1 <- pts, pt2 <- pts, pt1 < pt2]

part2 :: Input -> Int
part2 _ = 0
