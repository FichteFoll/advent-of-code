{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Arrow
import Data.Tuple.Extra (both)
import Data.Ix (inRange)

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
part2 _ = 0
