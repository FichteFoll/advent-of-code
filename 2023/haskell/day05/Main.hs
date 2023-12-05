{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main, parse, part1, part2, resolve) where

import Control.Applicative
import Control.Lens
import Data.Char
import Debug.Trace
import Data.List.Split
-- import qualified Data.Text as T

data Map = Map { _dest :: Int, _source :: Int, _len :: Int }
  deriving Show
$(makeLenses ''Map)
type Input = ([Int], [[Map]])

toMap [a, b, c] = Map a b c
toMap x = error $ "bad list:" ++ show x

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
    parseBlock = map (toMap . map read . words) . drop 1
    blocks = drop 1 $ splitWhen null $ lines text

part1 :: Input -> Int
part1 (seeds, maps) = minimum $ map (\n -> foldl resolve n maps) seeds

part2 :: Input -> Int
part2 x = 0

resolve :: Int -> [Map] -> Int
resolve n [] = n
resolve n (m:_) | 0 <= offset && offset < m ^. len = m ^. dest + offset
  where offset = n - m ^. source
resolve n (_:ms) = resolve n ms

-- debug s x = trace (s ++ ": " ++ show x) x
