{-# LANGUAGE TemplateHaskell #-}

module Main (main, parse, part1, part2) where

import Control.Applicative
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Char
import Debug.Trace
import Linear.V2

type Input = [String]
-- data Number = Number (V2 Int) Int Int
data Number = Number { _pos :: V2 Int, _val :: Int, _len :: Int }
$(makeLenses ''Number)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = lines

part1 :: Input -> Int
part1 rows = sum $ map (view val) $ filter hasAdjacentSymbol nums
  where
    nums =
      [ Number (V2 x y) (read $ traceShowId value') (length value')
      | (y, row) <- zip [0..] rows
      , (x, char) <- zip [0..] row
      , isDigit $ row !! x
      , x == 0 || not (isDigit $ row !! (x - 1))
      , let value' = takeWhile isDigit $ drop x row
      ]
    hasAdjacentSymbol num = or
      [ isSymbol $ rows !! y !! x
      | xOffset <- [-1..num ^. len]
      , let x = num ^. (pos . _x) + xOffset
      , yOffset <- [-1..1]
      , let y = num ^. (pos . _y) + yOffset
      , y >= 0, y < length rows
      , x >= 0, x < length (rows !! y)
      ]
    isSymbol = liftA2 (&&) (/= '.') (not . isDigit)

part2 :: Input -> Int
part2 x = 0
