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
part1 = sum . map ((\(a, b) -> a * 10 + b) . batteryPair)
  where
    nums = [9, 8 .. 1]

    batteryPair :: [Int] -> (Int, Int)
    batteryPair xs = head $ mapMaybe (batteryPair' xs) nums

    batteryPair' :: [Int] -> Int -> Maybe (Int, Int)
    batteryPair' [] _ = Nothing
    batteryPair' (x:xs) n
      | n == x = fmap (x,) $ listToMaybe $ mapMaybe (findSecond xs) nums
      | otherwise = batteryPair' xs n

    findSecond :: [Int] -> Int -> Maybe Int
    findSecond [] _ = Nothing
    findSecond (x:xs) n
      | n == x = Just x
      | otherwise = findSecond xs n

part2 :: Input -> Int
part2 _ = 0
