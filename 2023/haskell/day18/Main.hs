{-# OPTIONS_GHC -W #-}

module Main (main, parse, part1, part2) where

import Linear.V2

type Input = [(V2 Int, Int, String)]

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

parse :: String -> Input
parse = map (parseLine . words) . lines
  where
    parseLine [d, n, col] = (parseDir d, read n, take 6 $ drop 2 col)
    parseLine ws = error $ "parse error: " ++ show ws
    parseDir "R" = V2 1 0
    parseDir "D" = V2 0 1
    parseDir "L" = V2 (-1) 0
    parseDir "U" = V2 0 (-1)
    parseDir _   = undefined

-- TODO look out for gaps!
part1 :: Input -> Int
part1 input = length outline + length interior
  where
    outline = dig (V2 0 0) input
    interior =
      [ V2 x y
      -- TODO
      -- get all border tiles in col
      -- groupBy (y' + 1 == y'')
      -- break (y' < y)
      -- both sides must be of odd length
      -- ignore y' == y
      |
      ]

part2 :: Input -> Int
part2 _ = 0

dig :: V2 Int -> Input -> [V2 Int]
dig _ [] = []
dig pos ((dir, n, col):xs)
  | n == 0 = dig pos xs
  | otherwise = pos : dig (pos + dir) ((dir, pred n, col):xs)
