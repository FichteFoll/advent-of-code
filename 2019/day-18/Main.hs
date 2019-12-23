{-# Language RecordWildCards #-}

module Main where

import Data.Char
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Control.DeepSeq (force)

import Debug.Trace (trace, traceShowId)

import Linear.V2

type Point = V2 Int

data UMap = UM { -- short for UndergroundMap
  umPassages :: !(Set Point),
  umStart :: !Point,
  umKeys :: !(Map Point Char),
  umDoors :: !(Map Point Char)
} deriving (Show)

parse :: String -> UMap
parse input =
  UM {
    umPassages = Set.fromList $ Map.keys $ Map.filter (/= '#') charmap,
    umStart = head $ Map.keys $ Map.filter (== '@') charmap,
    umKeys = Map.filter (`elem` '@':['a'..'z']) charmap,
    umDoors = Map.map toLower $ Map.filter isUpper charmap
  }
  where
    charmap = Map.fromList [(V2 x y, c) | (y,line) <- zip [0..] $ lines input, (x,c) <- zip [0..] line]

data Connection = Connection {
  cDist :: !Int,
  cDoors :: !(Set Char)
} deriving (Show)

connections :: UMap -> Map (Set Char) Connection
connections m = Map.fromList $ concat [connFromKey key pt Set.empty | (pt, key) <- Map.toList $ umKeys m]
  where
    connFromKey :: Char -> Point -> Set Point -> [(Set Char, Connection)]
    connFromKey startKey pt visited
      | Just key <- Map.lookup pt $ umKeys m, key /= startKey
        = (Set.fromList [startKey, key], Connection (Set.size visited) doorsSeen) : branches
      | otherwise = branches
      where
        nextPts = umPassages m `Set.intersection` neighbors pt `Set.difference` visited
        newVisited = Set.insert pt visited
        branches = concat [connFromKey startKey nPt newVisited | nPt <- Set.toList nextPts]
        doorsSeen = Set.fromList $ mapMaybe (`Map.lookup` umDoors m) $ Set.toList visited

neighbors :: Point -> Set Point
neighbors pt = Set.fromList [pt+pt2 | pt2 <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]]

data Path = Path {
  pKeys :: !(Set Char),
  pPos :: !Char
} deriving (Show, Eq, Ord)

-- Build a map with the shortest solution from the given Path.
type Solutions = Map Path Int

-- 5278 is from a previous run I had to abort
collectKeys :: UMap -> Int
collectKeys m = snd $ solve Map.empty $ Path (Set.singleton '@') '@'
  where
    cons = connections m

    solve :: Solutions -> Path -> (Solutions, Int)
    solve seen here@(Path keys pos)
      -- | trace (show here) False = undefined
      | length keys == Map.size (umKeys m) = (Map.empty, 0)
      | Just best <- Map.lookup here seen = (Map.empty, best)
      | otherwise = foldl foldSolutions (seen, 1000000) subSolutions
      where
        subSolutions = [(Path (Set.insert newPos keys) newPos, cDist)
                        | newPos <- Map.elems (umKeys m)
                        , newPos `Set.notMember` keys
                        , let Connection{..} = cons ! Set.fromList [pos, newPos]
                        , cDoors `Set.isSubsetOf` keys]

    foldSolutions :: (Solutions, Int) -> (Path, Int) -> (Solutions, Int)
    foldSolutions (seen', best) (path, dist) = (newSeen, newBest)
      where
        (subMap, len) = solve seen' path
        newSeen = Map.insert path len $ Map.union seen' subMap
        newBest = min best (len + dist)

part1 :: UMap -> Int
part1 = collectKeys

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  -- putStrLn $ unlines $ map show $ Map.toList $ connections input
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ show (part2 input)
