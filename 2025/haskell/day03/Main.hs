{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.Char (digitToInt)

type Input = [[Int]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (map digitToInt) . lines

part1 :: Input -> Int
part1 = solve 2

part2 :: Input -> Int
part2 = solve 12

solve count = sum . map (maxJolt count)
  where
    maxJolt :: Int -> [Int] -> Int
    maxJolt 0 _ = 0
    maxJolt rest xs = 10 ^ pred rest * d + maxJolt (pred rest) xs'
      where d = maximum $ take (length xs - rest + 1) xs
            xs' = tail $ dropWhile (/= d) xs
