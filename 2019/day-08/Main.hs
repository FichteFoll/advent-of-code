module Main where

import Data.List
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

parse :: String -> [Char]
parse = head . lines

part1 :: [Char] -> Int
part1 input = (*) <$> count '1' <*> count '2'
  $ minimumBy (comparing $ count '0') $ chunksOf (25*6) input
  where count c s = length $ elemIndices c s

delayer :: [[Char]] -> [Char]
delayer layers = map (head . filter ('2' /=)) $ transpose layers

part2 :: [Char] -> [Char]
part2 = unlines . chunksOf 25 . map char . delayer . chunksOf (25*6)
  where
    char '0' = ' '
    char '1' = '█'

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2:\n" ++ (part2 input)
