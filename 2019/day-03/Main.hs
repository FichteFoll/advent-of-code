module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set


data Point = Pt Int Int deriving (Eq, Ord, Show)

pairwise :: (Int -> Int -> Int) -> Point -> Point -> Point
pairwise op (Pt x1 y1) (Pt x2 y2) = Pt (op x1 x2) (op y1 y2)

dist :: Point -> Int
dist (Pt x y) = (abs x) + (abs y)


type Cmd = (Char, Int)

parse :: String -> [[Cmd]]
parse = map parseLines . lines
  where
    parseLines :: String -> [Cmd]
    parseLines = map split . splitOn ","

    split :: [Char] -> Cmd
    split word = (head word, (read . tail) word)

iterCmd :: Cmd -> [Point]
iterCmd ('R', n) = [Pt   i    0  | i <- [1..n]]
iterCmd ('D', n) = [Pt   0    i  | i <- [1..n]]
iterCmd ('L', n) = [Pt (-i)   0  | i <- [1..n]]
iterCmd ('U', n) = [Pt   0  (-i) | i <- [1..n]]

-- Build a set of points the wire covers
used :: Point -> [Cmd] -> Set Point
used start [] = Set.empty
used start (cmd:cmds) = (Set.fromList line) `Set.union` used newStart cmds
  where
    lineZero = iterCmd cmd
    line = map (pairwise (+) start) $ iterCmd cmd
    newStart = pairwise (+) start (last lineZero)

-- Determine step count until wire reaches given point
steps :: Point -> [Cmd] -> Int
steps target wire = stepCount (Pt 0 0) target wire 0
  where
    stepCount start target (cmd:cmds) count = case List.elemIndex target line of
      Just i -> count + i + 1
      Nothing -> stepCount newStart target cmds (count + (snd cmd))
      where
        lineZero = iterCmd cmd
        line = map (pairwise (+) start) $ iterCmd cmd
        newStart = pairwise (+) start (last lineZero)

intersections :: [[Cmd]] -> Set Point
intersections input = foldl1 Set.intersection points
  where points = map (used (Pt 0 0)) input

part1 :: [[Cmd]] -> Int
part1 input = minimum $ Set.map dist $ intersections input

part2 :: [[Cmd]] -> Int
part2 input = minimum [sum $ [steps target wire | wire <- input]
                      | target <- Set.toList $ intersections input]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
