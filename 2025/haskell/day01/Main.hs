{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

import Control.Arrow ((&&&), Arrow (second), (<<<))

type Input = [(Char, Int)]


main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (second read <<< (head &&& tail)) . lines

part1 :: Input -> Int
part1 = length . filter (== 0) . scanl (\a (c, n) -> op c a n `mod` 100) 50
  where
    op 'L' = (-)
    op 'R' = (+)

part2 :: Input -> Int
part2 = length . filter (== 0) . map (`mod` 100) . concat . scanl (\a (c, n) -> take n $ tail $ iterate (op c) (last a)) [50]
  where
    op 'L' = pred
    op 'R' = succ
