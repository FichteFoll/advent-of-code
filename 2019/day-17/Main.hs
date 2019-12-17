module Main where

import Data.List.Split (chunksOf)
import Data.Char

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

part2 :: Tape -> Int
part2 intape = last $ output $ run (newIM { tape = 2:drop 1 intape }) inp
  where inp = map ord $ concat ["A,A,C,B,C,A,B,C,B,A\n"
                               ,"L,6,R,12,L,6,L,8,L,8\n"
                               ,"L,4,L,4,L,6\n"
                               ,"L,6,R,12,R,8,L,8\n"
                               ,"n\n"]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
