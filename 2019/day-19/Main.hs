module Main where

import Data.List.Split (chunksOf)
import Data.Char

import Intcode

type Point = (Int, Int)



part1 :: Tape -> Int
part1 intape = sum [last $ output $ run (newIM { tape = intape }) [x,y]
                   | x <- [0..49], y <- [0..49]]
{-
  TODO
    1. determine angle from high coordinates
    2. from the mean, begin at a point where the 100x100 would theoretically fit
    3. branch into all directions BFS until `isSmallestFit` matches
        discarding branches that don't `fit`
-}
part2 :: Tape -> Int
part2 intape = let (x,y) = smallestFit in 10000*x+y
  where
    smallestFit = head [(x,y) | x <- [1..], y <- [1..]
                        , isSmallestFit (x,y)]
    isSmallestFit pt = fits pt && all (not . fits) (tail $ rect pt (-1))
    fits pt = all covered $ rect pt 100
    covered (x,y) = 1 == (last $ output $ run (newIM { tape = intape }) [x,y])
    rect :: Point -> Int -> [Point]
    rect pt@(x,y) size = pt:[(x+xo,y+yo) | (xo,yo) <- [(size,0),(0,size)]] -- (x,y) is redundant

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  -- putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  -- print $ output $ run (newIM { tape = input }) [10,0]
