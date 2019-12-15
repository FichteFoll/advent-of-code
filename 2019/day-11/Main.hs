module Main where

import Control.Applicative (liftA2)
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

import Linear.V2
import Intcode

-- Prefix with D to prevent collision with Either constructors
data Direction = DUp | DRight | DDown | DLeft deriving (Bounded, Eq, Enum)
type Point = V2 Int
data Color = Black | White deriving (Enum, Eq)

toV2 :: Direction -> V2 Int
toV2 DUp = V2 0 (-1)
toV2 DRight = V2 1 0
toV2 DDown = V2 0 1
toV2 DLeft = V2 (-1) 0

turn :: Direction -> Direction -> Direction
turn a DLeft  = if a == minBound then maxBound else pred a
turn a DRight = if a == maxBound then minBound else succ a
turn a _ = undefined

paint :: IntcodeMachine -> Point -> Direction -> Map Point Color -> Map Point Color
paint amp pos dir painted
  | hasTerminated amp = painted
  | otherwise = paint newIM newPos newDir newPainted
  where
    inColor = fromEnum $ Map.findWithDefault Black pos painted
    withOutput = run amp [inColor]
    newIM = withOutput { output = [] }
    (outColor:outTurn:_) = output withOutput
    newDir = turn dir (if outTurn == 0 then DLeft else DRight)
    newPos = pos + toV2 newDir
    newPainted = Map.insert pos (toEnum outColor) painted

part1 :: Tape -> Int
part1 intape = length $ paint (newIM {tape = intape}) (pure 0) DUp Map.empty

part2 :: Tape -> String
part2 intape = unlines [[if elem (V2 x y) whites then '█' else ' '
                        | x <- [minimum xs..maximum xs]]
                       | y <- [minimum ys..maximum ys]]
  where
    painted = paint (newIM {tape = intape}) (pure 0) DUp $ Map.singleton (pure 0) White
    whites = Map.keys $ Map.filter (== White) painted
    (xs, ys) = unzip $ map (\(V2 x y) -> (x,y)) whites

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2:\n" ++ part2 input
