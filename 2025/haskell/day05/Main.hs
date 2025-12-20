{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Arrow
import Data.Tuple.Extra (both)
import Data.Ix (inRange, rangeSize)
import Data.List (sortOn)

type Input = ([(Int, Int)], [Int])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse
  = map (both read . fmap tail . break (== '-'))
  *** map read . tail
  <<< break null
  <<< lines

part1 :: Input -> Int
part1 (ranges, ns) = length $ filter (flip any ranges . flip inRange) ns

part2 :: Input -> Int
part2 = sum . map rangeSize . foldl insertRange [] . sortOn fst . fst

insertRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
insertRange [] r = [r]
insertRange (r@(a,b):rs) r'@(a',b')
  | inRange r a' = insertRange rs (min a a', max b b')
  | otherwise = r : insertRange rs r'
