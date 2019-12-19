{-# Language RecordWildCards #-}

module Main where

import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
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
  pLength :: !Int,
  pPos :: !Char
} deriving (Show, Eq, Ord)

{-
  option 1: do bfs like before
  option 2: build tree of valid options (basically the same)
  both are O(n!)
-}

collectKeys :: UMap -> [Path]
collectKeys m = loop Set.empty [Path (Set.singleton '@') 0 '@'] []
  where
    cons = connections m
    loop :: Set Path -> [Path] -> [Path] -> [Path]
    loop seen [] done = done
    loop seen (here@(Path keys steps pos):xs) done
      -- | trace (show here) False = undefined
      | length keys == Map.size (umKeys m) = loop seen xs (here:done)
      | here `Set.member` seen = loop seen xs done
      | otherwise = loop (Set.insert here seen) sortedQueue done
      where
        candidates = Map.filterWithKey (\ks _ -> pos `Set.member` ks) cons
        openCandidates = Map.filter (Set.null . (`Set.difference` keys) . cDoors) candidates
        newPaths = [Path (Set.insert newKey keys) (steps + cDist) newKey
                    | (ks,Connection{..}) <- Map.toList openCandidates
                    , not . Set.null $ ks `Set.difference` keys
                    , let newKey = head $ filter (/= pos) $ Set.toList ks]
        queue = xs ++ newPaths
        -- minDone = traceShowId $ minimum (5278:map pLength done)
        minDone = minimum (5278:map pLength done) -- 5278 is from a previous run I had to abort
        sortedQueue = sortOn (negate . Set.size . pKeys) $ filter ((< minDone) . pLength) queue

part1 :: UMap -> Int
part1 = minimum . map pLength . collectKeys

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  -- putStrLn $ unlines $ map show $ Map.toList $ connections input
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ show (part2 input)
