{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.Char (ord)
import Data.Maybe

type Input = [[Int]]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (map (\c -> ord c - ord '0')) . lines

part1 :: Input -> Int
part1 = solve 2

part2 :: Input -> Int
part2 = solve 12

solve count = sum . map (foldl1 (\a b -> a * 10 + b) . batteryPack count)
  where
    nums = [9, 8 .. 1]

    batteryPack :: Int -> [Int] -> [Int]
    batteryPack rest xs = head $ mapMaybe (batteryPack' rest xs) nums

    batteryPack' :: Int -> [Int] -> Int -> Maybe [Int]
    batteryPack' 0 _ _ = Just []
    batteryPack' _ [] _ = Nothing
    batteryPack' rest (x:xs) n
      | n == x = fmap (x:) $ listToMaybe $ mapMaybe (batteryPack' (pred rest) xs) nums
      | otherwise = batteryPack' rest xs n
