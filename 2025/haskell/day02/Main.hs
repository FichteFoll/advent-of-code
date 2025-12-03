{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Arrow
import Data.List.Extra (splitOn)
import Data.Tuple.Extra (both)
import Data.List.Split (chunksOf)
import Data.List (nub)

type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (both read . fmap tail . break (== '-')) . splitOn ","

part1 :: Input -> Int
part1 = solve isInvalid1

part2 :: Input -> Int
part2 = solve isInvalid2

solve :: (Num a, Foldable t, Enum a) => (a -> Bool) -> t (a, a) -> a
solve check = sum . concatMap (filter check . uncurry enumFromTo)

isInvalid1 :: Int -> Bool
isInvalid1 = show >>> max 2 . (`div` 2) . length &&& id >>> splitsEqually
-- isInvalid1 =
--   show
--   >>> (even . length &&& halvesEqual)
--   >>> uncurry (&&)
--   where halvesEqual = ((`div` 2) . length &&& id) >>> uncurry splitAt >>> uncurry (==)

isInvalid2 :: Int -> Bool
isInvalid2 =
  show
  >>> id &&& enumFromTo 1 . (`div` 2) . length  -- (s, ds)
  >>> first (flip $ curry splitsEqually)
  >>> uncurry any

splitsEqually :: (Int, String) -> Bool -- (d, s)
splitsEqually =
  second (length &&& id) -- (d, (l, s))
  >>> second fst &&& second snd -- ((d, l), (d, s))
  >>> checkDivides *** checkChunks
  >>> uncurry (&&)
  where
    checkDivides :: (Int, Int) -> Bool
    checkDivides = uncurry (flip mod) >>> (== 0)

    checkChunks :: (Int, String) -> Bool
    checkChunks = uncurry chunksOf >>> nub >>> length >>> (== 1)
