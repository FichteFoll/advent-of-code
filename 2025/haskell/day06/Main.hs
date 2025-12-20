{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
parse = map (liftA2 (:) last init . transpose) . splitWhen (all (== ' ')) . transpose . lines

part1 :: Input -> Int
part1 = solve (map read)

part2 :: Input -> Int
part2 = solve (map read . transpose)

solve :: ([String] -> [Int]) -> [[String]] -> Int
solve parseNums = sum . map calcLine
  where
    calcLine (opLine:ns) = foldl1 (op opLine) $ parseNums ns
    op ('*':_) = (*)
    op ('+':_) = (+)
