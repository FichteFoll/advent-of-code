{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TupleSections #-}

module Main (main, parse, part1, part2) where

import Data.List (nub, singleton)
import Data.Map.Strict (Map, (!))
import Linear.V2
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
part1 g = solve g (V2 0 0, V2 1 0)

part2 :: Input -> Int
part2 g = maximum $ map (solve g) beams
  where
    beams =
      [ (pos, negate dir)
      | pos <- Map.keys g
      , x <- [(-1)..1]
      , y <- [(-1)..1]
      , let dir = V2 x y
      , sum (abs dir) == 1
      , not $ (pos + dir) `Map.member` g
      ]

solve :: Input -> Beam -> Int
solve g = length . nub . energize g . singleton

energize :: Input -> [Beam] -> [V2 Int]
energize grid = map fst . Set.toList . go Set.empty
  where
    go seen [] = seen
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = go (Set.insert x seen) $ xs ++ step grid x

step :: Input -> Beam -> [Beam]
step grid (pos, dir@(V2 dx dy))
  | c == '/'  = move pos $ rot !! fromEnum (dx /= 0) $ dir
  | c == '\\' = move pos $ rot !! fromEnum (dy /= 0) $ dir
  | c == '-' && dy /= 0 = split
  | c == '|' && dx /= 0 = split
  | otherwise = move pos dir
  where
    c = grid ! pos
    move p d = map (, d) $ filter (`Map.member` grid) [p + d]
    split = concatMap (move pos . ($ dir)) rot
    rot = [rotR, rotL]
    rotR (V2 x y) = V2 (negate y) x
    rotL (V2 x y) = V2 y (negate x)
