{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Control.Lens ((^.), (&), (.~))
import Data.List (sort)
import Linear.V2
import qualified Data.Map as Map

type Input = ([(V2 Int, Char)], (Int, Int))

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse input = (pts, (length $ head rows, length rows))
  where
    rows = lines input
    pts = sort
      [ (V2 x y, c)
      | (y, row) <- zip [0..] rows
      , (x, c) <- zip [0..] row
      , c /= '.'
      ]

part1 :: Input -> Int
part1 = loadNorth . tiltNorth

part2 :: Input -> Int
part2 = loadNorth . untilRepeats 1000000000 . tail . iterate tiltAll

tiltNorth :: Input -> Input
tiltNorth (pts, (w, h)) = (pts', (w, h))
  where
    pts' = sort $ concat
      [ snd $ foldl move (0, []) col
      | x <- [0..pred w]
      -- relies on the list being sorted
      , let col = filter ((== x) . (^. _x) . fst) pts
      ]
    move (next, acc) (pt, c)
      | c == '#' = (succ $ pt ^. _y, (pt, c):acc)
      | c == 'O' = (succ next, (pt & _y .~ next, c):acc)
      | otherwise = undefined

loadNorth :: Input -> Int
loadNorth (pts, (_, h))
  = sum
  . map ((h -) . (^. _y) . fst)
  . filter ((== 'O') . snd)
  $ pts

untilRepeats :: Int -> [Input] -> Input
untilRepeats reps xs = go Map.empty (zip [1..reps] xs)
  where
    go _ [(_, x)] = x -- as if this is gonna happen
    go cache aixs@((i, x):ixs) = case Map.lookup x cache of
      Just prevI -> let rest = (reps - prevI) `mod` (i - prevI) in
        snd $ aixs !! rest
      Nothing    -> go (Map.insert x i cache) ixs
    go _ _ = undefined

tiltAll :: Input -> Input
tiltAll = (!! 4) . iterate (rotateRight . tiltNorth)

rotateRight :: Input -> Input
rotateRight (pts, (w, h)) = (pts', (h, w))
  where
    pts' = sort
      [ (V2 x' y', c)
      | (V2 x  y , c) <- pts
      , let x' = pred $ h - y
      , let y' = x
      ]

-- traceInput x = traceShow (loadNorth $ render x) x
-- render inp@(pts, (w, h)) = trace rendered inp
--   where
--     rendered = intercalate "\n" $ map line [0..pred h]
--     line y = map (\x -> char x y pts) [0..pred w]
--     char _ _ [] = '.'
--     char x y ((V2 x' y', c):_) | x == x' && y == y' = c
--     char x y (_:pts') = char x y pts'
