module Main where

import Data.List.Split (chunksOf)
import Data.Char (chr)

import Intcode

type Point = (Int, Int)

neighbours :: Point -> [Point]
neighbours (x,y) = [(x+xo,y+yo) | (xo,yo) <- [(1,0),(0,1),(-1,0),(0,-1)]]

parseMap :: String -> [Point]
parseMap m = concat [[(x,y) | (x,c) <- zip [0..] line, c /= '.']
                    | (y,line) <- zip [0..] $ lines m]

part1 :: Tape -> Int
part1 intape = sum $ map (uncurry (*)) [pt | pt <- pts, all (`elem` pts) $ neighbours pt]
  where pts = parseMap $ map chr $ output $ run (newIM { tape = intape }) []

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ part2 input
