{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Control.Arrow
import Data.List (elemIndices)

type Input = (Int, [S.IntSet])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse
  = head . elemIndices 'S' . head
  &&& filter (not . S.null) . map (S.fromDistinctAscList . elemIndices '^') . tail
  <<< lines

part1 :: Input -> Int
part1 = fst . solve

part2 :: Input -> Int
part2 = sum . M.elems . snd . solve

-- We can compute both solutions in a single iteration.
-- Part 1 could be optimized if done separately, but this saves code.
solve :: (Foldable t, Num a) => (Int, t S.IntSet) -> (Int, M.IntMap a)
solve (startPos, grid) = foldl lineStep (0, M.singleton startPos 1) grid

lineStep :: Num a => (Int, M.IntMap a) -> S.IntSet -> (Int, M.IntMap a)
lineStep (c, timelineMap) splitters
  = (c + S.size changeKeys, foldl1 (M.unionWith (+)) [unchanged, splitLeft, splitRight])
  where
    beamKeys = M.keysSet timelineMap
    changeKeys = S.intersection beamKeys splitters
    unchanged = M.restrictKeys timelineMap (beamKeys S.\\ changeKeys)
    changed = M.restrictKeys timelineMap changeKeys
    splitLeft = M.mapKeysMonotonic pred changed
    splitRight = M.mapKeysMonotonic succ changed
