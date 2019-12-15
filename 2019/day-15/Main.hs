module Main where

import Data.List.Split (splitOn)
import Data.Maybe

import Intcode

data Direction = North | South | West | East deriving (Bounded, Eq, Enum)
data Tile = Wall | Normal | Oxygen deriving (Enum, Eq)

backwards :: Direction -> Direction
backwards North = South
backwards South = North
backwards West = East
backwards East = West

toInput :: Direction -> Int
toInput = (+ 1) . fromEnum

data Droid = Droid {
  machine :: IntcodeMachine,
  lastDirection :: Maybe Direction,
  tile :: Maybe Tile,
  steps :: Int
}

newDroid = Droid newIM Nothing Nothing 0

droidStep :: Droid -> Direction -> Droid
droidStep droid dir = droid { machine = newMachine { output = [] }
                            , lastDirection = Just dir
                            , tile = Just newTile
                            , steps = steps droid + 1
                            }
  where
    newMachine = run (machine droid) [toInput dir]
    newTile = toEnum $ head $ output $ newMachine

newDirections :: Maybe Direction -> [Direction]
newDirections mPDir | Just prevDir <- mPDir = filter (/= backwards prevDir) $ enumFrom North
                    | otherwise = enumFrom North

findPath :: Droid -> Maybe Int
findPath droid
  | Just Oxygen <- tile droid = Just $ steps droid
  | Just Wall <- tile droid = Nothing
  | otherwise = maybeMin [findPath $ droidStep droid newDir | newDir <- directions]
  where
    directions = newDirections $ lastDirection droid

-- TODO try composing this using `min`
maybeMin :: Ord a => [Maybe a] -> Maybe a
maybeMin xs = case catMaybes xs of
  [] -> Nothing
  as -> Just $ minimum as


parse :: String -> Tape
parse = map read . splitOn ","

part1 :: Tape -> Int
part1 intape = fromJust $ findPath $ newDroid { machine = newIM { tape = intape } }

-- part2 :: Tape -> Int

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  -- putStrLn $ "Part 2: " ++ (show $ part2 input)
