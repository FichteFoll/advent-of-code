{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

import Data.List (transpose)

type Input = [[String]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = transpose . map words . lines

part1 :: Input -> Int
part1 = sum . map (calcLine . reverse)
  where
    calcLine (ops:ns) = foldl1 (f ops) $ map read ns
    f "*" = (*)
    f "+" = (+)

part2 :: Input -> Int
part2 _ = 0
