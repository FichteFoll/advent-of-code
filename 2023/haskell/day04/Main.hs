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
part1 = sum . map (points . wins)
  where
    points 0 = 0
    points n = 2 ^ (n - 1)

part2 :: Input -> Int
part2 = flip part2' [0]

part2' :: Input -> [Int] -> Int
part2' [] _ = 0
part2' cards [] = part2' cards [0]
part2' (card:cards) (extra:extras) = count + part2' cards extras'
  where
    count = 1 + extra
    extras' = zipWithPad (+) 0 extras (replicate (wins card) count)

wins = length . uncurry intersect

zipWithPad f empty xs ys = take maxLength $ zipWith f (pad xs) (pad ys)
  where
    maxLength = max (length xs) (length ys)
    pad v = v ++ repeat empty
