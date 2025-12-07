{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import qualified Data.IntSet as IS
import Control.Arrow

type Input = (IS.IntSet, [IS.IntSet])

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = IS.fromList . posWhereEq 'S' . head &&& map (IS.fromList . posWhereEq '^') <<< lines

part1 :: Input -> Int
part1 (start, grid) = fst $ foldl lineStep (0, start) $ tail grid
  where
    lineStep (c, beams) splitters = (c + IS.size intersections, IS.union unchanged split)
      where
        intersections = IS.intersection beams splitters
        unchanged = beams IS.\\ intersections
        split = liftA2 IS.union (IS.map pred) (IS.mapMonotonic succ) intersections

part2 :: Input -> Int
part2 _ = 0

posWhereEq :: Eq b => b -> [b] -> [Int]
posWhereEq c = map fst . filter ((== c) . snd) . zip [0..]
