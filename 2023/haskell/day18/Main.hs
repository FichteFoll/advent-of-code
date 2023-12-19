{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Arrow ((&&&))
import Control.Lens ((^.), bimap)
import Data.Semialign (Unzip(unzipWith))
import Linear.V2
import qualified Data.Set as Set

type Plan = (V2 Int, Int)
type Input = [(Plan, Plan)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (parseLine . words) . lines
  where
    parseLine [[d], n, col] =
      ( (parseDir d, read n)
      , (parseDir $ col !! 7, read $ "0x" ++ take 5 (drop 2 col))
      )
    parseLine ws = error $ "parse error: " ++ show ws
    parseDir c
      | c `elem` "R0" = V2 1 0
      | c `elem` "D1" = V2 0 1
      | c `elem` "L2" = V2 (-1) 0
      | c `elem` "U3" = V2 0 (-1)
      | otherwise     = undefined

part1 :: Input -> Int
-- Example input includes the starting point in the last instruction,
-- so we don't add it again.
part1 = lagoonSize . dig (V2 0 0) . map fst

part2 :: Input -> Int
-- TODO uses too much RAM, quite expectedly
part2 = lagoonSize . dig (V2 0 0) . map snd

dig :: V2 Int -> [Plan] -> [V2 Int]
dig _ [] = []
dig pos ((dir, n):xs) = take n (tail $ iterate (+ dir) pos) ++ dig (pos + fmap (* n) dir) xs

-- Traverse all points outside of the bounding box
-- that don't cross the border.
-- Everything not seen is then part of the lagoon.
-- I hate this code but it's the first idea I had that should work.
lagoonSize :: [V2 Int] -> Int
lagoonSize outline = (succ maxX - minX) * (succ maxY - minY) - length extra
  where
    (xs, ys) = unzipWith ((^. _x) &&& (^. _y)) outline
    (minX, maxX) = extend $ minMax xs
    (minY, maxY) = extend $ minMax ys
    minMax = minimum &&& maximum
    extend = bimap pred succ
    extra = go Set.empty [V2 minX minY]
    go seen [] = seen
    go seen (pt:pts)
      | inBounds pt && notOutline pt && not (Set.member pt seen)
        = go (Set.insert pt seen) $ neighbors pt ++ pts
      | otherwise = go seen pts
    inBounds (V2 x y) = minX <= x && x <= maxX && minY <= y && y <= maxY
    notOutline pt = not $ Set.member pt $ Set.fromList outline

neighbors :: V2 Int -> [V2 Int]
neighbors (V2 x y) =
  [ V2 (x-1) y
  , V2 x (y-1)
  , V2 (x+1) y
  , V2 x (y+1)
  ]
