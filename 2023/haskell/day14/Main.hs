{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Data.Map (Map)
import Linear.V2
import qualified Data.Map as Map
import Control.Lens ((^.), (&), (.~))

type Input = (Map (V2 Int) Char, (Int, Int))

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse input = (pts, (length $ head rows, length rows))
  where
    rows = lines input
    pts = Map.fromList
      [ (V2 x y, c)
      | (y, row) <- zip [0..] rows
      , (x, c) <- zip [0..] row
      , c /= '.'
      ]

part1 :: Input -> Int
part1 = loadNorth . tiltNorth

part2 :: Input -> Int
part2 _ = 0

-- north = V2 (-1) 0
tiltNorth :: Input -> Input
tiltNorth (pts, (width, height)) = (pts', (width, height))
  where
    pts' = Map.fromList $ concat
      [ snd $ foldl move (0, []) col
      | x <- [0..pred width]
      -- relies on maps being sorted
      , let col = filter ((== x) . (^. _x) . fst) $ Map.toList pts
      ]
    move (next, acc) (pt, c)
      | c == '#' = (succ $ pt ^. _y, (pt, c):acc)
      | c == 'O' = (succ next, (pt & _y .~ next, c):acc)
      | otherwise = undefined

loadNorth :: Input -> Int
loadNorth (pts, (_, height))
  = sum
  . map ((height -) . (^. _y) . fst)
  . filter ((== 'O') . snd)
  $ Map.toList pts
