{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List (transpose)
import Data.List.Split (splitWhen)

type Input = [[String]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = splitWhen null . lines

part1 :: Input -> Int
part1 = sum . map gridScore

part2 :: Input -> Int
part2 _ = 0

gridScore :: [String] -> Int
gridScore grid = case (findMirrorH grid, findMirrorH $ transpose grid) of
  ([h], []) -> 100 * h
  ([], [v]) -> v
  x -> error $ "got two or no matches " ++ show x

findMirrorH :: [String] -> [Int]
findMirrorH grid
  = [ i
    | i <- [1..pred $ length grid]
    , let up = reverse $ take i grid
    , let down = drop i grid
    , and $ zipWith (==) up down
    ]
