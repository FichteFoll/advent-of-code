module Main (main, parse, part1, part2) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Char
import Data.List.Split (splitOn)
import Debug.Trace

-- (time, distance)
type Input = [(Int, Int)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = uncurry zip . over both (map read . drop 1 . words) . fmap (drop 1) . break (== '\n')

part1 :: Input -> Int
part1 = product . map numWins
  where numWins (time, dist) = length $ filter (> dist) $ uncurry (zipWith (*)) $ liftA2 (,) id reverse [1..pred time]

part2 :: Input -> Int
part2 x = 0
