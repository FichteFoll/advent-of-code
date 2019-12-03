module Main where

import Debug.Trace (trace)
import Data.List.Split (splitOn)
import Data.List (sort, intersect)
-- import Data.Set (Set)
-- import qualified Data.Set as Set


data Point a = Pt a a deriving (Eq, Show)

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

-- build a list of points the wire covers
used :: IPoint -> [IPoint] -> [IPoint]
used start [] = []
used start (cmd:cmds) = line ++ used new_start cmds
  where
    line = map (pairwise (+) start) $ iterLine cmd
    new_start = pairwise (+) start cmd

    iterLine :: IPoint -> [IPoint]
    iterLine (Pt 0 0) = []
    iterLine (Pt x 0) = [Pt i 0 | i <- saturatedRange x]
    iterLine (Pt 0 y) = [Pt 0 i | i <- saturatedRange y]

    saturatedRange :: (Num a, Enum a, Ord a) => a -> [a]
    saturatedRange i
      | i < 0 = [i..(-1)]
      | i > 0 = [1..i]
      | otherwise = []

part1 :: [[IPoint]] -> Int
part1 input = head $ sort $ map dist $ foldl intersect (head points) (tail points)
  where points = map (used (Pt 0 0)) input

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
