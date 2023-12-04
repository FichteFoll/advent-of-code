-- {-# LANGUAGE OverloadedStrings #-}

module Main (main, parse, part1, part2) where

import Control.Applicative
import Debug.Trace
import Data.List (intersect)

type Input = [([Int], [Int])]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (mapPair (map read) . split . drop 2 . words) . lines
  where
    split = fmap (drop 1) . break (== "|")
    mapPair a = liftA2 (,) (a . fst) (a . snd)

part1 :: Input -> Int
part1 = sum . map (points . length . uncurry intersect)
  where
    points 0 = 0
    points n = 2 ^ (n - 1)

part2 :: Input -> Int
part2 x = 0
