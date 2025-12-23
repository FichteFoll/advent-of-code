{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List.Extra (dropEnd)

type Input = [String]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 = solve 1

part2 :: Input -> Int
part2 = solve 11

solve :: Int -> Input -> Int
solve count = sum . map (read . maxJolt count)

maxJolt :: Int -> String -> String
maxJolt (-1) _ = ""
maxJolt rest xs = c : maxJolt (pred rest) xs'
  where c = maximum $ dropEnd rest xs
        xs' = tail $ dropWhile (/= c) xs
