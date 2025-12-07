{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Arrow

type Input = (Int, [[Int]])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = head . posWhereEq 'S' . head &&& filter (not . null) . map (posWhereEq '^') . tail <<< lines

part1 :: Input -> Int
part1 (startPos, grid) = fst $ foldl lineStep (0, IS.singleton startPos) $ map IS.fromDistinctAscList grid
  where
    lineStep (c, beams) splitters = (c + IS.size intersections, IS.union unchanged split)
      where
        intersections = IS.intersection beams splitters
        unchanged = beams IS.\\ intersections
        split = liftA2 IS.union (IS.mapMonotonic pred) (IS.mapMonotonic succ) intersections

part2 :: Input -> Int
part2 (startPos, grid) = sum $ M.elems $ foldl lineStep (M.singleton startPos 1) $ map S.fromList grid
  where
    lineStep timelineMap splitters = M.unionWith (+) unchanged split
      where
        beamSet = M.keysSet timelineMap
        intersections = S.intersection beamSet splitters
        unchanged = M.restrictKeys timelineMap (beamSet S.\\ intersections)
        split = liftA2 (M.unionWith (+)) (M.mapKeys pred) (M.mapKeys succ) $ M.restrictKeys timelineMap intersections

posWhereEq :: Eq b => b -> [b] -> [Int]
posWhereEq c = map fst . filter ((== c) . snd) . zip [0..]
