{-# OPTIONS_GHC -W #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main, parse, part1, part2) where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Char (isDigit)
import Linear.V2

type Input = ([String], [Number])
data Number = Number { _pos :: V2 Int, _val :: Int, _len :: Int }
  deriving Show
$(makeLenses ''Number)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse text = (grid, nums)
  where
    grid = lines text
    nums =
      [ Number (V2 x y) (read value') (length value')
      | (y, row) <- zip [0..] grid
      , (x, char) <- zip [0..] row
      , isDigit char
      , x == 0 || not (isDigit $ row !! pred x)
      , let value' = takeWhile isDigit $ drop x row
      ]

part1 :: Input -> Int
part1 (grid, nums) = sum $ map (^. val) $ filter hasAdjacentSymbol nums
  where
    hasAdjacentSymbol num
      = or
        [ isSymbol $ grid !! y !! x
        | xOffset <- [-1..num ^. len]
        , let x = num ^. (pos . _x) + xOffset
        , yOffset <- [-1..1]
        , let y = num ^. (pos . _y) + yOffset
        , y >= 0, y < length grid
        , x >= 0, x < length (grid !! y)
        ]
    isSymbol = liftA2 (&&) (/= '.') (not . isDigit)

part2 :: Input -> Int
part2 (grid, nums)
  = sum
    $ map (product . map (^. val))
    $ filter ((== 2) . length)
    $ map adjacentNums stars
  where
    stars =
      [ (x, y)
      | (y, row) <- zip [0..] grid
      , (x, char) <- zip [0..] row
      , char == '*'
      ]
    adjacentNums (x, y) =
      [ num
      | num <- nums
      , let xNum = num ^. (pos . _x)
      , let yNum = num ^. (pos . _y)
      , abs (yNum - y) <= 1
      , x - 1 <= xNum + num ^. len - 1
      , xNum <= x + 1
      ]
