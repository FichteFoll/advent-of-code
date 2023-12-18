{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TupleSections #-}

module Main (main, parse, part1, part2) where

import Linear.V2
import Data.List (nub)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Input = Map (V2 Int) Char
-- pos & direction
type Beam = (V2 Int, V2 Int)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse text = Map.fromList
  [ (V2 x y, c)
  | (y, row) <- zip [0..] $ lines text
  , (x, c) <- zip [0..] row
  ]

part1 :: Input -> Int
-- TODO render map of energized tiles to find where it goes wrong
part1 = length . nub . traceShowId . flip energize [(V2 0 0, V2 1 0)]

part2 :: Input -> Int
part2 _ = 0

energize :: Input -> [Beam] -> [V2 Int]
energize grid = map fst . Set.toList . go Set.empty
  where
    go seen [] = trace "end" seen
    go seen (x:xs)
      | x `Set.member` seen = trace ("skipping " ++ show x) $ go seen xs
      | otherwise = go (Set.insert x seen) $ xs ++ traceShowId (step grid x)

step :: Input -> Beam -> [Beam]
step grid (pos, dir@(V2 dx dy))
  | c == '/'  = move pos $ rot !! fromEnum (dx /= 0) $ dir
  | c == '\\' = move pos $ rot !! fromEnum (dy /= 0) $ dir
  | c == '-' && dy /= 0 = split
  | c == '|' && dx /= 0 = split
  | otherwise = trace ("do nothing at " ++ show pos ++ " dir: " ++ show dir ++ " c: " ++ [c]) $ move pos dir
  where
    c = grid ! pos
    move p d = map (, d) $ filter (`Map.member` grid) [trace ("checking " ++ show (p+d)) $ p + d]
    split = concatMap (move pos . ($ dir)) rot
    rot = [rotR, rotL]
    rotR (V2 x y) = V2 (negate y) (abs x)
    rotL (V2 x y) = V2 (abs y) (negate x)
