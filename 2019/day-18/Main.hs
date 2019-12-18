module Main where

import Control.Applicative (liftA2)
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import Data.Tuple
import qualified Data.Set as Set

import Debug.Trace (trace)

import Linear.V2

type Point = V2 Int

data UMap = UM {
  passages :: Set Point,
  position :: Point,
  dkeys :: Map Char Point,
  doors :: Map Char Point,
  steps :: Int
} deriving (Show)

consumeKey :: UMap -> Char -> UMap
consumeKey m k = m {dkeys = Map.delete k $ dkeys m,
                    doors = Map.delete (chr $ ord k - 0x20) $ doors m}

parse :: String -> UMap
parse input =
  UM {
    passages = Set.fromList $ Map.keys $ Map.filter (/= '#') charmap,
    position = head $ Map.keys $ Map.filter (== '@') charmap,
    dkeys = reverseMap $ Map.filter ((> 0x60) . ord) charmap,
    doors = reverseMap $ Map.filter (liftA2 (&&) (> 0x40) (< 0x60) . ord) charmap,
    steps = 0
  }
  where
    charmap = Map.fromList $ concat [[(V2 x y, c) | (x,c) <- zip [0..] line ]
                                    | (y,line) <- zip [0..] $ lines input]
    reverseMap = Map.fromList . map swap . Map.toList

neighbors :: Point -> Set Point
neighbors pt = Set.fromList [pt+pt2 | pt2 <- [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]]

shortestPath :: UMap -> Point -> Maybe Int
shortestPath m to = shortestPath' (Set.singleton $ position m) 0
  where
    shortestPath' :: Set Point -> Int -> Maybe Int
    shortestPath' candidates count
      | to `Set.member` candidates = Just count
      | candidates == newCandidates = Nothing
      | otherwise = shortestPath' newCandidates (count+1)
      where candidateNeighbors = foldl Set.union candidates (Set.map neighbors candidates)
            newCandidates = candidateNeighbors `Set.intersection` passages m `Set.difference` doorPts
    doorPts = Set.fromList $ Map.elems $ doors m

collectKeys :: UMap -> Int
collectKeys m
  | Map.null $ dkeys m = steps m
  | otherwise = minimum $ map collectKeys newMaps
  where
    newMaps = [ (consumeKey m k) {position = pt, steps = steps m + dist}
                | (k,pt) <- Map.toList $ dkeys m
                , let path = shortestPath m pt
                , isJust path
                , let Just dist = path]

part1 :: UMap -> Int
part1 = collectKeys

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  -- putStrLn $ "Part 2: " ++ show (part2 input)
