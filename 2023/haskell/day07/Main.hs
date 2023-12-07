{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2, cardKey) where

import Data.Function (on)
import Data.List (sort, sortBy, group, elemIndex)
import Control.Arrow ((&&&))

type Card = Char
type Hand = [Card]
type Input = [(Hand, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (fmap (read . tail) . break (== ' ')) . lines

part1 :: Input -> Int
part1 = sum . zipWith (*) [1..] . map snd . sortBy cardKey

part2 :: Input -> Int
part2 _ = 0

cardKey = compare `on` (tier &&& cardOrd) . fst
  where
    tier = sortBy (flip compare) . map length . group . sort
    cardOrd = map (`elemIndex` cardOrder)
    cardOrder = "23456789TJQKA"
