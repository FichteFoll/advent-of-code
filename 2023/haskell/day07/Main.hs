{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, cardKey) where

import Data.Function (on)
import Data.List (sort, sortBy, group, elemIndex, sortOn)
import Control.Arrow ((&&&), first)
import Data.Ord (Down(..))

type Input = [(String, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (fmap (read . tail) . break (== ' ')) . lines

part1 :: Input -> Int
part1 = solve False

part2 :: Input -> Int
part2 = solve True

solve j = sum . zipWith (*) [1..] . map snd . sortBy (cardKey j)

cardKey jokers = compare `on` (tier &&& cardOrd) . fst
  where
    tier hand
      | null hand = [0]
      | jokers && 'J' `elem` hand = mapHead succ . tier $ dropOnceOn (== 'J') hand
      | otherwise = sortOn Down . map length . group . sort $ hand
    cardOrd = map (`elemIndex` cardOrder)
    cardOrder | jokers    = "23456789TQKA"
              | otherwise = "23456789TJQKA"
    dropOnceOn f xs = takeWhile (not . f) xs ++ tail (dropWhile (not . f) xs)
    mapHead f = uncurry (<>) . first (map f) . splitAt 1
