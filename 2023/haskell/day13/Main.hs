{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Lens (element, (&), (%~))
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
part1 = sum . map (head . gridScore)

part2 :: Input -> Int
part2 = sum . map gridScore'

gridScore :: [String] -> [Int]
gridScore grid = map (*100) (findMirrorH grid) ++ findMirrorH (transpose grid)

gridScore' :: [String] -> Int
-- there *could* be multiple results here, but we only take the first
gridScore' grid = head . filter (/= original) . concatMap gridScore . p2ify $ grid
  where original = head $ gridScore grid

findMirrorH :: [String] -> [Int]
findMirrorH grid
  = [ i
    | i <- [1..pred $ length grid]
    , let up = reverse $ take i grid
    , let down = drop i grid
    , and $ zipWith (==) up down
    ]

p2ify :: [String] -> [[String]]
p2ify grid
  = [grid & element y . element x %~ flipChar
    | y <- [0..pred $ length grid]
    , x <- [0..pred $ length $ head grid]
    ]
  where
    flipChar '#' = '.'
    flipChar '.' = '#'
    flipChar _ = undefined
