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
parse = map transpose . splitWhen (all (== ' ')) . transpose . lines

part1 :: Input -> Int
part1 = solve (map read)

part2 :: Input -> Int
part2 = solve (map (read . reverse) . transpose)

solve :: Foldable t => ([String] -> t Int) -> [[String]] -> Int
solve parseBlock = sum . map (calcLine . reverse)
  where
    calcLine (ops:ns) = foldl1 (f ops) $ parseBlock ns
    f ('*':_) = (*)
    f ('+':_) = (+)
