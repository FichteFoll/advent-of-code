{-# LANGUAGE OverloadedStrings #-}

module Main (main, parse, part1, part2, resolve, resolveRange) where

import Control.Lens
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (sort)
import Data.List.Split (splitWhen)

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
    -- map fragments must be sorted for part2
    parseBlock = sort . map (toMapFrag . map read . words) . drop 1
    blocks = drop 1 $ splitWhen null $ lines text

part1 :: Input -> Int
part1 (seeds, maps) = minimum $ map (\n -> foldl resolve n maps) seeds

part2 :: Input -> Int
part2 (seeds, maps) = minimum $ map fst $ foldl resolveRange (rangify seeds) maps

resolve :: Int -> [MapFrag] -> Int
resolve n [] = n
resolve n ((src, offset):_) | inRange src n = n + offset
resolve n (_:ms) = resolve n ms

rangify :: [Int] -> [(Int, Int)]
rangify [] = []
rangify (a:b:xs) = (a, a + b - 1) : rangify xs
rangify [_] = error "list must have an even number of items"

resolveRange :: [(Int, Int)] -> [MapFrag] -> [(Int, Int)]
resolveRange rs ms = concatMap (`resolveRange'` ms) rs

resolveRange' :: (Int, Int) -> [MapFrag] -> [(Int, Int)]
resolveRange' r [] = [r]
resolveRange' r@(r1, r2) m@(((mr1, mr2), offset):ms)
  | mr1 <=  r1 && r1 <= mr2 && r2 <= mr2 = [over both (+ offset) r]
  | mr1 <=  r1 && r1 <= mr2              = (r1 + offset, mr2 + offset) : resolveRange' (succ mr2, r2) ms
  |  r1 <  mr1 && r2 >= mr1              = (r1, pred (min r2 mr1)) : resolveRange' (minmax r2 mr1) m
  | mr2 <   r1                           = resolveRange' r ms
  | r2  <  mr1                           = [r]
  | otherwise = error "case not covered"
  where minmax a b = (min a b, max a b)
