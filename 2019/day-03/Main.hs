module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set


data Point a = Pt a a deriving (Eq, Ord, Show)

instance Functor Point where
  fmap f (Pt x y) = Pt (f x) (f y)

pairwise :: (a -> a -> b) -> Point a -> Point a -> Point b
pairwise op (Pt x1 y1) (Pt x2 y2) = Pt (op x1 x2) (op y1 y2)

dist :: Num a => Point a -> a
dist (Pt x y) = (abs x) + (abs y)

type IPoint = Point Int

----------------

parse :: String -> [[IPoint]]
parse = map parseLines . lines
  where
    parseLines :: String -> [IPoint]
    parseLines = map (tocoord . split) . splitOn ","

    split :: [Char] -> (Char, Int)
    split word = (head word, (read . tail) word)

    tocoord :: (Char, Int) -> IPoint
    tocoord ('U', n) = Pt   0 (-n)
    tocoord ('R', n) = Pt   n   0
    tocoord ('D', n) = Pt   0   n
    tocoord ('L', n) = Pt (-n)  0

iterLine :: IPoint -> [IPoint]
iterLine (Pt 0 0) = []
iterLine (Pt x 0) = [Pt i 0 | i <- rangeFromOne x]
iterLine (Pt 0 y) = [Pt 0 i | i <- rangeFromOne y]

rangeFromOne :: (Num a, Enum a, Ord a) => a -> [a]
rangeFromOne i
  | i < 0 = map negate [1..(-i)]
  | i > 0 = [1..i]
  | otherwise = []

-- Build a set of points the wire covers
used :: IPoint -> [IPoint] -> Set IPoint
used start [] = Set.empty
used start (cmd:cmds) = line `Set.union` used new_start cmds
  where
    line = Set.fromList $ map (pairwise (+) start) $ iterLine cmd
    new_start = pairwise (+) start cmd

-- Determine step count until wire reaches given point
steps :: IPoint -> [IPoint] -> Int
steps target wire = stepCount (Pt 0 0) target wire 0
  where
    stepCount start target (cmd:cmds) count = case List.findIndex (== target) line of
      Just i -> count + i + 1
      Nothing -> stepCount newStart target cmds (count + (dist cmd))
      where
        line = map (pairwise (+) start) $ iterLine cmd
        newStart = pairwise (+) start cmd

intersections :: [[IPoint]] -> Set IPoint
intersections input = foldl Set.intersection (head points) (tail points)
  where points = map (used (Pt 0 0)) input

part1 :: [[IPoint]] -> Int
part1 input = Set.findMin $ Set.map dist $ intersections input

part2 :: [[IPoint]] -> Int
part2 input = head $ List.sort $ stepList
  where
    stepList = [sum $ [steps target wire | wire <- input]
               | target <- Set.toList $ intersections input]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
