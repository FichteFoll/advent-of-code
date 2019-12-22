module Main where

import Data.List
import Debug.Trace

import Intcode

type Point = (Int, Int)

part1 :: Tape -> Int
part1 intape = sum [last $ output $ run (newIM { tape = intape }) [x,y]
                   | x <- [0..49], y <- [0..49]]

{-
  Idea
    1. determine angles of borders with high coordinates
    2. from their mean angle, determine a point where the 100x100 square would theoretically fit
    3. find a point where it *actually* fits by increasing the distance step by step
    4. reduce coordinates individually until we don't find a smaller fit anymore

  Advantages
    - not entirely bruteforced, resulting in relatively stable runtime

  Disadvantages
    - calculation of the angle is off by 0.01 compared to the solution,
      which results in inaccuracy of 20-25
    - using BFS for step 3. would probably be faster, but is more effort to implement
    - still checks 2k points in step 1.
-}
part2 :: Tape -> Int
part2 intape = let (x,y) = smallestFit in x * 10000 + y
  where
    farPoints = sortOn (fmap negate)
      [(x,y) | x <- [1..1000], y <- [1..1000] , x == 1000 || y == 1000, covered (x,y)]
    borderPts = [head farPoints, last farPoints]
    -- borderPts = [(661,1000),(836,1000)]
    centerAngle = (/ 2) $ sum $ map angle borderPts
    diag = sqrt $ 2 * 99**2
    halfAngle = angle (head borderPts) - centerAngle
    startDist = diag / sin halfAngle / 2 - (diag / 2)
    loop2Start = loop1 startDist
    smallestFit = loop2 loop2Start

    loop1 :: Double -> Point
    loop1 dst | fits pt = pt
              -- | trace ("loop1 " ++ show pt) False = undefined
              | otherwise = loop1 (dst+1)
      where pt = ptFromAngle centerAngle dst

    loop2 :: Point -> Point
    loop2 pt@(x,y) | null next = pt
                   -- | trace ("loop2 " ++ show pt) False = undefined
                   | otherwise = loop2 $ head next
                   where next = filter fits [(x-2,y-2),(x-1,y-2),(x-2,y-1),(x-1,y-1),(x-1,y),(x,y-1)] -- I hate this line

    fits :: Point -> Bool
    fits pt@(x,y) = all covered [(x+99,y),(x,y+99)]
    covered :: Point -> Bool
    covered (x,y) = 1 == last (output $ run (newIM { tape = intape }) [x,y])

    ptFromAngle :: Double -> Double -> Point
    ptFromAngle angle' dist = (round $ dist * cos angle', round $ dist * sin angle')
    angle :: Point -> Double
    angle (x,y) = fromIntegral y `atan2` fromIntegral x


main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
