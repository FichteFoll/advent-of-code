{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main, parse, part1, part2) where

import Data.List.Extra (splitOn, sortOn)
import Linear.V2

type Point = V2 Int
type Input = [Point]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map ((\[x,y] -> V2 x y) . map read . splitOn ",") . lines

part1 :: Input -> Int
part1 = maximum . map fst . rects

part2 :: Input -> Int
part2 pts = fst $ head $ filter (rectValid . snd) $ sortOn (negate . fst) $ rects pts
  where
    -- Check if any line spanned by two input points (= green tiles)
    -- lands within the rect spanned by pt1 and pt2,
    -- excluding the border (which is permitted).
    greenLines = (zip =<< tail) (last pts : pts)
    rectValid (a, b) = not $ any intersects greenLines
      where
        intersects (c, d) = and (liftA2 (<) (min c d) hi) && and (liftA2 (<) lo (max c d))
        lo = liftA2 min a b
        hi = liftA2 max a b

rects :: [Point] -> [(Int, (Point, Point))]
rects pts = [(rectSize a b, (a, b)) | a <- pts, b <- pts, a < b]

rectSize :: Point -> Point -> Int
rectSize a b = product $ abs (b - a) + V2 1 1
