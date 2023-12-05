{-# LANGUAGE OverloadedStrings #-}

module Main (main, parse, part1, part2, resolve) where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Ix
import Debug.Trace
import Data.List.Split
-- import qualified Data.Text as T

-- maps a source range to an offset
type MapFrag = ((Int, Int), Int)
type Input = ([Int], [[MapFrag]])

toMapFrag [dest, source, len] = ((source, source + len - 1), dest - source)
toMapFrag x = error $ "bad list:" ++ show x

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse text = (seeds, maps)
  where
    seeds = map read $ drop 1 $ words $ head $ lines text
    maps = map parseBlock blocks
    parseBlock = map (toMapFrag . map read . words) . drop 1
    blocks = drop 1 $ splitWhen null $ lines text

part1 :: Input -> Int
part1 (seeds, maps) = minimum $ map (\n -> foldl resolve n maps) seeds

part2 :: Input -> Int
part2 (seeds, maps) = minimum $ map (\n -> foldl resolve n maps) (unrange seeds)

unrange :: [Int] -> [Int]
unrange [] = []
unrange (a:b:xs) = take b [a..] ++ unrange xs
unrange [_] = error "list must have an even number of items"

resolve :: Int -> [MapFrag] -> Int
resolve n [] = n
resolve n ((src, offset):_) | inRange src n = n + offset
resolve n (_:ms) = resolve n ms

-- debug s x = trace (s ++ ": " ++ show x) x
