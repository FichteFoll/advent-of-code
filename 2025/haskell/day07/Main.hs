{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import Control.Arrow

type Input = (Int, [S.IntSet])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse
  = head . posWhereEq 'S' . head
  &&& map S.fromDistinctAscList . filter (not . null) . map (posWhereEq '^') . tail
  <<< lines

part1 :: Input -> Int
part1 = fst . solve

part2 :: Input -> Int
part2 = sum . M.elems . snd . solve

-- We can compute both solutions in a single iteration.
-- Part 1 could be optimized if done separately, but this saves code.
solve :: (Foldable t, Num a) => (M.Key, t S.IntSet) -> (Int, M.IntMap a)
solve (startPos, grid) = foldl lineStep (0, M.singleton startPos 1) grid
  where
    lineStep (c, timelineMap) splitters = (c + S.size intersections, M.unionWith (+) unchanged split)
      where
        beamSet = M.keysSet timelineMap
        intersections = S.intersection beamSet splitters
        unchanged = M.restrictKeys timelineMap (beamSet S.\\ intersections)
        split = liftA2 (M.unionWith (+)) (M.mapKeys pred) (M.mapKeys succ) $ M.restrictKeys timelineMap intersections

posWhereEq :: Eq b => b -> [b] -> [Int]
posWhereEq c = map fst . filter ((== c) . snd) . zip [0..]
