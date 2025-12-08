{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.List.Extra (splitOn, sort, sortOn)
import qualified Data.Set as S

import Linear.Metric (distance)
import Linear.V3

type Point = V3 Float
type Input = [Point]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 1000 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map ((\[x,y,z] -> V3 x y z) . map read . splitOn ",") . lines

part1 :: Int -> Input -> Int
part1 n pts = product $ take 3 $ sortOn negate $ map length (circuitIter !! n)
  where
    -- sortedPts = sort pts
    distances = sort [(dist, pt1, pt2) | pt1 <- pts, pt2 <- pts, pt1 < pt2, let dist = pt1 `distance` pt2]
    circuitIter = scanl step (map S.singleton pts) distances
    -- find circuit of pt1 and join it with circuit of pt2
    step circuits (_, pt1, pt2) = if pt1 `S.member` circ2 then circuits else circuits'
      where
        circ2 = head $ filter (S.member pt2) circuits
        noCirc2 = filter (not . S.member pt2) circuits
        circuits' = mapIf (S.member pt1) (S.union circ2) noCirc2


part2 :: Input -> Int
part2 _ = 0

mapIf :: (b -> Bool) -> (b -> b) -> [b] -> [b]
mapIf f m = map (\x -> if f x then m x else x)
