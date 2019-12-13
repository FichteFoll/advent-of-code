module Main where

import Data.List
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map

import Intcode

type Point = (Int, Int)
data Tile = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq, Show)

data Game = Game
  { machine :: IntcodeMachine
  , score :: Int
  , fields :: Map Point Tile
  }

newGame = Game newIM 0 Map.empty

-- alternative: split output into types and do everything at once instead of folding
readOutput :: Game -> Game
readOutput game = foldl interpretTriplet newGame out
  where
    newGame = game {machine = (machine game) {output = []} }
    out = chunksOf 3 $ output $ machine game
    interpretTriplet game triplet = case triplet of
      [] -> game
      (-1:0:s:[]) -> game { score = s }
      (x:y:t:[]) -> game { fields = Map.insert (x,y) (toEnum t) (fields game) }

tilePos :: Game -> Tile -> [Point]
tilePos game tile = Map.keys $ Map.filter (== tile) $ fields game

stepGame :: Game -> [Int] -> Game
stepGame game inp = readOutput $ game { machine = run (machine game) inp }

calcInput :: Game -> [Int]
calcInput game = [signum $ fst ballPos - fst paddlePos]
  where
    ballPos = head $ tilePos game Ball
    paddlePos = head $ tilePos game Paddle

runGame :: Game -> [Int] -> Game
runGame game inp | hasTerminated $ machine game = game
                 | otherwise = runGame nextGame $ calcInput nextGame
                   where nextGame = stepGame game inp

render :: Game -> String
render game = "Score: " ++ (show $ score game) ++ "\n"
  ++ unlines [[tileChar $ Map.findWithDefault Empty (x, y) $ fields game
              | x <- [minimum xs..maximum xs]]
             | y <- [minimum ys..maximum ys]]
  where
    (xs, ys) = unzip $ Map.keys $ fields game
    tileChar :: Tile -> Char
    tileChar Empty = ' '
    tileChar Wall = '█'
    tileChar Block = '#'
    tileChar Paddle = '—'
    tileChar Ball = 'o'

-------------------------------------------------------------------------------

parse :: String -> Tape
parse = map read . splitOn ","

part1 :: Tape -> Int
part1 intape = length $ tilePos (runGame game []) Block
  where game = newGame { machine = newIM { tape = intape } }

part2 :: Tape -> Int
part2 intape = score $ runGame game []
  where game = newGame { machine = newIM { tape = [2] ++ tail intape } }

debugGame :: Game -> [Int] -> IO ()
debugGame game inp = do
  let nextGame = stepGame game inp
  putStrLn $ render nextGame
  putStrLn ""
  if hasTerminated (machine nextGame) then
    return ()
  else do
    debugGame nextGame $ calcInput nextGame

main :: IO ()
main = do
  input <- parse <$> getContents
  -- let game = newGame { machine = newIM { tape = [2] ++ tail input} }
  -- debugStep game []
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)
