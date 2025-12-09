{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.Function (on)
import Data.List.Extra (splitOn, sortOn)
import qualified Data.Set as S

import Linear.Metric (distance)
import Linear.V3

type Point = V3 Int
type Input = [Point]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 1000 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map ((\[x,y,z] -> V3 x y z) . map read . splitOn ",") . lines

part1 :: Int -> Input -> Int
part1 n pts = product $ take 3 $ sortOn negate $ map length $ circuitIter pts (distances pts) !! n

part2 :: Input -> Int
part2 pts = sum $ (* V3 1 0 0) $ uncurry (*) $ dists !! i
  where i = length $ takeWhile ((/= 1) . length) $ tail $ circuitIter pts dists
        dists = distances pts

circuitIter :: [Point] -> [(Point, Point)] -> [[S.Set Point]]
circuitIter pts = scanl step (map S.singleton pts)
  where
    -- find circuit of pt1 and join it with circuit of pt2 if they are different
    step circuits (pt1, pt2) = if pt1 `S.member` circ2 then circuits else circuits'
      where
        circ2 = head $ filter (S.member pt2) circuits
        noCirc2 = filter (not . S.member pt2) circuits
        circuits' = mapIf (S.member pt1) (S.union circ2) noCirc2

distances :: [Point] -> [(Point, Point)]
distances pts = sortOn dist [(pt1, pt2) | pt1 <- pts, pt2 <- pts, pt1 < pt2]
  where dist = uncurry (distance `on` fmap fromIntegral)

mapIf :: (b -> Bool) -> (b -> b) -> [b] -> [b]
mapIf f m = map (\x -> if f x then m x else x)
